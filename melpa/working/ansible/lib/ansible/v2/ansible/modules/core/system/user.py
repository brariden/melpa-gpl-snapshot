#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) 2012, Stephen Fromm <sfromm@gmail.com>
#
# This file is part of Ansible
#
# Ansible is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Ansible is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Ansible.  If not, see <http://www.gnu.org/licenses/>.

DOCUMENTATION = '''
---
module: user
author: Stephen Fromm
version_added: "0.2"
short_description: Manage user accounts
requirements: [ useradd, userdel, usermod ]
description:
    - Manage user accounts and user attributes.
options:
    name:
        required: true
        aliases: [ "user" ]
        description:
            - Name of the user to create, remove or modify.
    comment:
        required: false
        description:
            - Optionally sets the description (aka I(GECOS)) of user account.
    uid:
        required: false
        description:
            - Optionally sets the I(UID) of the user.
    non_unique:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - Optionally when used with the -u option, this option allows to
              change the user ID to a non-unique value.
        version_added: "1.1"
    group:
        required: false
        description:
            - Optionally sets the user's primary group (takes a group name).
    groups:
        required: false
        description:
            - Puts the user in this comma-delimited list of groups. When set to
              the empty string ('groups='), the user is removed from all groups
              except the primary group.
    append:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - If C(yes), will only add groups, not set them to just the list
              in I(groups).
    shell:
        required: false
        description:
            - Optionally set the user's shell.
    home:
        required: false
        description:
            - Optionally set the user's home directory.
    password:
        required: false
        description:
            - Optionally set the user's password to this crypted value.  See
              the user example in the github examples directory for what this looks
              like in a playbook. The `FAQ <http://docs.ansible.com/faq.html#how-do-i-generate-crypted-passwords-for-the-user-module>`_
              contains details on various ways to generate these password values.
    state:
        required: false
        default: "present"
        choices: [ present, absent ]
        description:
            - Whether the account should exist.  When C(absent), removes
              the user account.
    createhome:
        required: false
        default: "yes"
        choices: [ "yes", "no" ]
        description:
            - Unless set to C(no), a home directory will be made for the user
              when the account is created or if the home directory does not
              exist. 
    move_home:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - If set to C(yes) when used with C(home=), attempt to move the
              user's home directory to the specified directory if it isn't there
              already.
    system:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - When creating an account, setting this to C(yes) makes the user a
              system account.  This setting cannot be changed on existing users.
    force:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - When used with C(state=absent), behavior is as with
              C(userdel --force).
    login_class:
        required: false
        description:
            - Optionally sets the user's login class for FreeBSD, OpenBSD and NetBSD systems.
    remove:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        description:
            - When used with C(state=absent), behavior is as with
              C(userdel --remove).
    generate_ssh_key:
        required: false
        default: "no"
        choices: [ "yes", "no" ]
        version_added: "0.9"
        description:
            - Whether to generate a SSH key for the user in question.
              This will B(not) overwrite an existing SSH key.
    ssh_key_bits:
        required: false
        default: 2048
        version_added: "0.9"
        description:
            - Optionally specify number of bits in SSH key to create.
    ssh_key_type:
        required: false
        default: rsa
        version_added: "0.9"
        description:
            - Optionally specify the type of SSH key to generate. 
              Available SSH key types will depend on implementation
              present on target host.
    ssh_key_file:
        required: false
        default: $HOME/.ssh/id_rsa
        version_added: "0.9"
        description:
            - Optionally specify the SSH key filename.
    ssh_key_comment:
        required: false
        default: ansible-generated
        version_added: "0.9"
        description:
            - Optionally define the comment for the SSH key.
    ssh_key_passphrase:
        required: false
        version_added: "0.9"
        description:
            - Set a passphrase for the SSH key.  If no
              passphrase is provided, the SSH key will default to
              having no passphrase.
    update_password:
        required: false
        default: always
        choices: ['always', 'on_create']
        version_added: "1.3"
        description:
            - C(always) will update passwords if they differ.  C(on_create) will only set the password for newly created users.
'''

EXAMPLES = '''
# Add the user 'johnd' with a specific uid and a primary group of 'admin'
- user: name=johnd comment="John Doe" uid=1040 group=admin

# Add the user 'james' with a bash shell, appending the group 'admins' and 'developers' to the user's groups
- user: name=james shell=/bin/bash groups=admins,developers append=yes

# Remove the user 'johnd'
- user: name=johnd state=absent remove=yes

# Create a 2048-bit SSH key for user jsmith
- user: name=jsmith generate_ssh_key=yes ssh_key_bits=2048
'''

import os
import pwd
import grp
import syslog
import platform

try:
    import spwd
    HAVE_SPWD=True
except:
    HAVE_SPWD=False


class User(object):
    """
    This is a generic User manipulation class that is subclassed
    based on platform.

    A subclass may wish to override the following action methods:-
      - create_user()
      - remove_user()
      - modify_user()
      - ssh_key_gen()
      - ssh_key_fingerprint()
      - user_exists()

    All subclasses MUST define platform and distribution (which may be None).
    """

    platform = 'Generic'
    distribution = None
    SHADOWFILE = '/etc/shadow'

    def __new__(cls, *args, **kwargs):
        return load_platform_subclass(User, args, kwargs)

    def __init__(self, module):
        self.module     = module
        self.state      = module.params['state']
        self.name       = module.params['name']
        self.uid        = module.params['uid']
        self.non_unique  = module.params['non_unique']
        self.group      = module.params['group']
        self.groups     = module.params['groups']
        self.comment    = module.params['comment']
        self.home       = module.params['home']
        self.shell      = module.params['shell']
        self.password   = module.params['password']
        self.force      = module.params['force']
        self.remove     = module.params['remove']
        self.createhome = module.params['createhome']
        self.move_home  = module.params['move_home']
        self.system     = module.params['system']
        self.login_class = module.params['login_class']
        self.append     = module.params['append']
        self.sshkeygen  = module.params['generate_ssh_key']
        self.ssh_bits   = module.params['ssh_key_bits']
        self.ssh_type   = module.params['ssh_key_type']
        self.ssh_comment = module.params['ssh_key_comment']
        self.ssh_passphrase = module.params['ssh_key_passphrase']
        self.update_password = module.params['update_password']
        if module.params['ssh_key_file'] is not None:
            self.ssh_file = module.params['ssh_key_file']
        else:
            self.ssh_file = os.path.join('.ssh', 'id_%s' % self.ssh_type)

        # select whether we dump additional debug info through syslog
        self.syslogging = False

    def execute_command(self, cmd):
        if self.syslogging:
            syslog.openlog('ansible-%s' % os.path.basename(__file__))
            syslog.syslog(syslog.LOG_NOTICE, 'Command %s' % '|'.join(cmd))

        return self.module.run_command(cmd)

    def remove_user_userdel(self):
        cmd = [self.module.get_bin_path('userdel', True)]
        if self.force:
            cmd.append('-f')
        if self.remove:
            cmd.append('-r')
        cmd.append(self.name)

        return self.execute_command(cmd)

    def create_user_useradd(self, command_name='useradd'):
        cmd = [self.module.get_bin_path(command_name, True)]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)
        elif self.group_exists(self.name):
            # use the -N option (no user group) if a group already
            # exists with the same name as the user to prevent 
            # errors from useradd trying to create a group when 
            # USERGROUPS_ENAB is set in /etc/login.defs.
            cmd.append('-N')

        if self.groups is not None and len(self.groups):
            groups = self.get_groups_set()
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.password is not None:
            cmd.append('-p')
            cmd.append(self.password)

        if self.createhome:
            cmd.append('-m')
        else:
            cmd.append('-M')

        if self.system:
            cmd.append('-r')

        cmd.append(self.name)
        return self.execute_command(cmd)


    def _check_usermod_append(self):
        # check if this version of usermod can append groups
        usermod_path = self.module.get_bin_path('usermod', True)

        # for some reason, usermod --help cannot be used by non root
        # on RH/Fedora, due to lack of execute bit for others
        if not os.access(usermod_path, os.X_OK):
            return False

        cmd = [usermod_path]
        cmd.append('--help')
        rc, data1, data2 = self.execute_command(cmd)
        helpout = data1 + data2

        # check if --append exists
        lines = helpout.split('\n')
        for line in lines:
            if line.strip().startswith('-a, --append'):
                return True

        return False



    def modify_user_usermod(self):
        cmd = [self.module.get_bin_path('usermod', True)]
        info = self.user_info()
        has_append = self._check_usermod_append()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups_need_mod = False
            groups = []

            if self.groups == '':
                if current_groups and not self.append:
                    groups_need_mod = True
            else:
                groups = self.get_groups_set(remove_existing=False)
                group_diff = set(current_groups).symmetric_difference(groups)

                if group_diff:
                    if self.append:
                        for g in groups:
                            if g in group_diff:
                                if has_append:
                                    cmd.append('-a')
                                groups_need_mod = True
                                break
                    else:
                        groups_need_mod = True

            if groups_need_mod:
                if self.append and not has_append:
                    cmd.append('-A')
                    cmd.append(','.join(group_diff))
                else:
                    cmd.append('-G')
                    cmd.append(','.join(groups))


        if self.comment is not None and info[4] != self.comment:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            cmd.append('-d')
            cmd.append(self.home)
            if self.move_home:
                cmd.append('-m')

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.update_password == 'always' and self.password is not None and info[1] != self.password:
            cmd.append('-p')
            cmd.append(self.password)

        # skip if no changes to be made
        if len(cmd) == 1:
            return (None, '', '')
        elif self.module.check_mode:
            return (0, '', '')

        cmd.append(self.name)
        return self.execute_command(cmd)

    def group_exists(self,group):
        try:
            if group.isdigit():
                if grp.getgrgid(int(group)):
                    return True
            else:
                if grp.getgrnam(group):
                    return True
        except KeyError:
            return False

    def group_info(self,group):
        if not self.group_exists(group):
            return False
        if group.isdigit():
            return list(grp.getgrgid(group))
        else:
            return list(grp.getgrnam(group))

    def get_groups_set(self, remove_existing=True):
        if self.groups is None:
            return None
        info = self.user_info()
        groups = set(filter(None, self.groups.split(',')))
        for g in set(groups):
            if not self.group_exists(g):
                self.module.fail_json(msg="Group %s does not exist" % (g))
            if info and remove_existing and self.group_info(g)[2] == info[3]:
                groups.remove(g)
        return groups

    def user_group_membership(self):
        groups = []
        info = self.get_pwd_info()
        for group in grp.getgrall():
            if self.name in group.gr_mem and not info[3] == group.gr_gid:
                groups.append(group[0])
        return groups

    def user_exists(self):
        try:
            if pwd.getpwnam(self.name):
                return True
        except KeyError:
            return False

    def get_pwd_info(self):
        if not self.user_exists():
            return False
        return list(pwd.getpwnam(self.name))

    def user_info(self):
        if not self.user_exists():
            return False
        info = self.get_pwd_info()
        if len(info[1]) == 1 or len(info[1]) == 0:
            info[1] = self.user_password()
        return info

    def user_password(self):
        passwd = ''
        if HAVE_SPWD:
            try:
                passwd = spwd.getspnam(self.name)[1]
            except KeyError:
                return passwd
        if not self.user_exists():
            return passwd
        else:
            # Read shadow file for user's encrypted password string
            if os.path.exists(self.SHADOWFILE) and os.access(self.SHADOWFILE, os.R_OK):
                for line in open(self.SHADOWFILE).readlines():
                    if line.startswith('%s:' % self.name):
                        passwd = line.split(':')[1]
        return passwd

    def get_ssh_key_path(self):
        info = self.user_info()
        if os.path.isabs(self.ssh_file):
            ssh_key_file = self.ssh_file
        else:
            ssh_key_file = os.path.join(info[5], self.ssh_file)
        return ssh_key_file

    def ssh_key_gen(self):
        info = self.user_info()
        if not os.path.exists(info[5]):
            return (1, '', 'User %s home directory does not exist' % self.name)
        ssh_key_file = self.get_ssh_key_path()
        ssh_dir = os.path.dirname(ssh_key_file) 
        if not os.path.exists(ssh_dir):
            try:
                os.mkdir(ssh_dir, 0700)
                os.chown(ssh_dir, info[2], info[3])
            except OSError, e:
                return (1, '', 'Failed to create %s: %s' % (ssh_dir, str(e)))
        if os.path.exists(ssh_key_file):
            return (None, 'Key already exists', '')
        cmd = [self.module.get_bin_path('ssh-keygen', True)]
        cmd.append('-t')
        cmd.append(self.ssh_type)
        cmd.append('-b')
        cmd.append(self.ssh_bits)
        cmd.append('-C')
        cmd.append(self.ssh_comment)
        cmd.append('-f')
        cmd.append(ssh_key_file)
        cmd.append('-N')
        if self.ssh_passphrase is not None:
            cmd.append(self.ssh_passphrase)
        else:
            cmd.append('')

        (rc, out, err) = self.execute_command(cmd)
        if rc == 0:
            # If the keys were successfully created, we should be able
            # to tweak ownership.
            os.chown(ssh_key_file, info[2], info[3])
            os.chown('%s.pub' % ssh_key_file, info[2], info[3])
        return (rc, out, err)

    def ssh_key_fingerprint(self):
        ssh_key_file = self.get_ssh_key_path()
        if not os.path.exists(ssh_key_file):
            return (1, 'SSH Key file %s does not exist' % ssh_key_file, '')
        cmd = [ self.module.get_bin_path('ssh-keygen', True) ]
        cmd.append('-l')
        cmd.append('-f')
        cmd.append(ssh_key_file)

        return self.execute_command(cmd)

    def get_ssh_public_key(self):
        ssh_public_key_file = '%s.pub' % self.get_ssh_key_path()
        try:
            f = open(ssh_public_key_file)
            ssh_public_key = f.read().strip()
            f.close()
        except IOError:
            return None
        return ssh_public_key

    def create_user(self):
        # by default we use the create_user_useradd method
        return self.create_user_useradd()

    def remove_user(self):
        # by default we use the remove_user_userdel method
        return self.remove_user_userdel()

    def modify_user(self):
        # by default we use the modify_user_usermod method
        return self.modify_user_usermod()

    def create_homedir(self, path):
        if not os.path.exists(path):
            # use /etc/skel if possible
            if os.path.exists('/etc/skel'):
                try:
                    shutil.copytree('/etc/skel', path, symlinks=True)
                except OSError, e:
                    self.module.exit_json(failed=True, msg="%s" % e)
        else:
            try:
                os.makedirs(path)
            except OSError, e:
                self.module.exit_json(failed=True, msg="%s" % e)

    def chown_homedir(self, uid, gid, path):
        try:
            os.chown(path, uid, gid)
            for root, dirs, files in os.walk(path):
                for d in dirs:
                    os.chown(path, uid, gid)
                for f in files:
                    os.chown(os.path.join(root, f), uid, gid)
        except OSError, e:
            self.module.exit_json(failed=True, msg="%s" % e)
           

# ===========================================

class FreeBsdUser(User):
    """
    This is a FreeBSD User manipulation class - it uses the pw command
    to manipulate the user database, followed by the chpass command
    to change the password.

    This overrides the following methods from the generic class:-
      - create_user()
      - remove_user()
      - modify_user()
    """

    platform = 'FreeBSD'
    distribution = None
    SHADOWFILE = '/etc/master.passwd'

    def remove_user(self):
        cmd = [
            self.module.get_bin_path('pw', True),
            'userdel',
            '-n',
            self.name
        ]
        if self.remove:
            cmd.append('-r')

        return self.execute_command(cmd)

    def create_user(self):
        cmd = [
            self.module.get_bin_path('pw', True),
            'useradd',
            '-n',
            self.name,
        ]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)

        if self.groups is not None:
            groups = self.get_groups_set()
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.createhome:
            cmd.append('-m')

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            cmd.append('-L')
            cmd.append(self.login_class)

        # system cannot be handled currently - should we error if its requested?
        # create the user
        (rc, out, err) = self.execute_command(cmd)
        if rc is not None and rc != 0:
            self.module.fail_json(name=self.name, msg=err, rc=rc)

        # we have to set the password in a second command
        if self.password is not None:
            cmd = [
                self.module.get_bin_path('chpass', True),
                '-p',
                self.password,
                self.name 
            ]
            return self.execute_command(cmd)

        return (rc, out, err)

    def modify_user(self):
        cmd = [
            self.module.get_bin_path('pw', True),
            'usermod',
            '-n',
            self.name 
        ]
        cmd_len = len(cmd)
        info = self.user_info()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.comment is not None and info[4] != self.comment:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            if self.move_home:
                cmd.append('-m')
            cmd.append('-d')
            cmd.append(self.home)

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            cmd.append('-L')
            cmd.append(self.login_class)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups = self.get_groups_set()

            group_diff = set(current_groups).symmetric_difference(groups)
            groups_need_mod = False

            if group_diff:
                if self.append:
                    for g in groups:
                        if g in group_diff:
                            groups_need_mod = True
                            break
                else:
                    groups_need_mod = True

            if groups_need_mod:
                cmd.append('-G')
                new_groups = groups
                if self.append:
                    new_groups = groups | set(current_groups)
                cmd.append(','.join(new_groups))

        # modify the user if cmd will do anything
        if cmd_len != len(cmd):
            (rc, out, err) = self.execute_command(cmd)
            if rc is not None and rc != 0:
                self.module.fail_json(name=self.name, msg=err, rc=rc)
        else:
            (rc, out, err) = (None, '', '')

        # we have to set the password in a second command
        if self.update_password == 'always' and self.password is not None and info[1] != self.password:
            cmd = [
                self.module.get_bin_path('chpass', True),
                '-p',
                self.password,
                self.name 
            ]
            return self.execute_command(cmd)

        return (rc, out, err)

# ===========================================

class OpenBSDUser(User):
    """
    This is a OpenBSD User manipulation class.
    Main differences are that OpenBSD:-
     - has no concept of "system" account.
     - has no force delete user

    This overrides the following methods from the generic class:-
      - create_user()
      - remove_user()
      - modify_user()
    """

    platform = 'OpenBSD'
    distribution = None
    SHADOWFILE = '/etc/master.passwd'

    def create_user(self):
        cmd = [self.module.get_bin_path('useradd', True)]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)

        if self.groups is not None:
            groups = self.get_groups_set()
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            cmd.append('-L')
            cmd.append(self.login_class)

        if self.password is not None:
            cmd.append('-p')
            cmd.append(self.password)

        if self.createhome:
            cmd.append('-m')

        cmd.append(self.name)
        return self.execute_command(cmd)

    def remove_user_userdel(self):
        cmd = [self.module.get_bin_path('userdel', True)]
        if self.remove:
            cmd.append('-r')
        cmd.append(self.name)
        return self.execute_command(cmd)

    def modify_user(self):
        cmd = [self.module.get_bin_path('usermod', True)]
        info = self.user_info()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups_need_mod = False
            groups_option = '-G'
            groups = []

            if self.groups == '':
                if current_groups and not self.append:
                    groups_need_mod = True
            else:
                groups = self.get_groups_set()
                group_diff = set(current_groups).symmetric_difference(groups)

                if group_diff:
                    if self.append:
                        for g in groups:
                            if g in group_diff:
                                groups_option = '-S'
                                groups_need_mod = True
                                break
                    else:
                        groups_need_mod = True

            if groups_need_mod:
                cmd.append(groups_option)
                cmd.append(','.join(groups))

        if self.comment is not None and info[4] != self.comment:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            if self.move_home:
                cmd.append('-m')
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            # find current login class
            user_login_class = None
            userinfo_cmd = [self.module.get_bin_path('userinfo', True), self.name]
            (rc, out, err) = self.execute_command(userinfo_cmd)

            for line in out.splitlines():
                tokens = line.split()

                if tokens[0] == 'class' and len(tokens) == 2:
                    user_login_class = tokens[1]

            # act only if login_class change
            if self.login_class != user_login_class:
                cmd.append('-L')
                cmd.append(self.login_class)

        if self.update_password == 'always' and self.password is not None and info[1] != self.password:
            cmd.append('-p')
            cmd.append(self.password)

        # skip if no changes to be made
        if len(cmd) == 1:
            return (None, '', '')
        elif self.module.check_mode:
            return (0, '', '')

        cmd.append(self.name)
        return self.execute_command(cmd)


# ===========================================

class NetBSDUser(User):
    """
    This is a NetBSD User manipulation class.
    Main differences are that NetBSD:-
     - has no concept of "system" account.
     - has no force delete user


    This overrides the following methods from the generic class:-
      - create_user()
      - remove_user()
      - modify_user()
    """

    platform = 'NetBSD'
    distribution = None
    SHADOWFILE = '/etc/master.passwd'

    def create_user(self):
        cmd = [self.module.get_bin_path('useradd', True)]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)

        if self.groups is not None:
            groups = self.get_groups_set()
            if len(groups) > 16:
                self.module.fail_json(msg="Too many groups (%d) NetBSD allows for 16 max." % len(groups))
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            cmd.append('-L')
            cmd.append(self.login_class)

        if self.password is not None:
            cmd.append('-p')
            cmd.append(self.password)

        if self.createhome:
            cmd.append('-m')

        cmd.append(self.name)
        return self.execute_command(cmd)

    def remove_user_userdel(self):
        cmd = [self.module.get_bin_path('userdel', True)]
        if self.remove:
            cmd.append('-r')
        cmd.append(self.name)
        return self.execute_command(cmd)

    def modify_user(self):
        cmd = [self.module.get_bin_path('usermod', True)]
        info = self.user_info()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups_need_mod = False
            groups = []

            if self.groups == '':
                if current_groups and not self.append:
                    groups_need_mod = True
            else:
                groups = self.get_groups_set()
                group_diff = set(current_groups).symmetric_difference(groups)

                if group_diff:
                    if self.append:
                        for g in groups:
                            if g in group_diff:
                                groups = set(current_groups).union(groups)
                                groups_need_mod = True
                                break
                    else:
                        groups_need_mod = True

            if groups_need_mod:
                if len(groups) > 16:
                    self.module.fail_json(msg="Too many groups (%d) NetBSD allows for 16 max." % len(groups))
                cmd.append('-G')
                cmd.append(','.join(groups))

        if self.comment is not None and info[4] != self.comment:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            if self.move_home:
                cmd.append('-m')
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.login_class is not None:
            cmd.append('-L')
            cmd.append(self.login_class)

        if self.update_password == 'always' and self.password is not None and info[1] != self.password:
            cmd.append('-p')
            cmd.append(self.password)

        # skip if no changes to be made
        if len(cmd) == 1:
            return (None, '', '')
        elif self.module.check_mode:
            return (0, '', '')

        cmd.append(self.name)
        return self.execute_command(cmd)


# ===========================================

class SunOS(User):
    """
    This is a SunOS User manipulation class - The main difference between
    this class and the generic user class is that Solaris-type distros
    don't support the concept of a "system" account and we need to 
    edit the /etc/shadow file manually to set a password. (Ugh)

    This overrides the following methods from the generic class:-
      - create_user()
      - remove_user()
      - modify_user()
    """

    platform = 'SunOS'
    distribution = None
    SHADOWFILE = '/etc/shadow'

    def remove_user(self):
        cmd = [self.module.get_bin_path('userdel', True)]
        if self.remove:
            cmd.append('-r')
        cmd.append(self.name)

        return self.execute_command(cmd)

    def create_user(self):
        cmd = [self.module.get_bin_path('useradd', True)]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)

        if self.groups is not None:
            groups = self.get_groups_set()
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.createhome:
            cmd.append('-m')

        cmd.append(self.name)

        if self.module.check_mode:
            return (0, '', '')
        else:
            (rc, out, err) = self.execute_command(cmd)
            if rc is not None and rc != 0:
                self.module.fail_json(name=self.name, msg=err, rc=rc)

            # we have to set the password by editing the /etc/shadow file 
            if self.password is not None:
                try:
                    lines = []
                    for line in open(self.SHADOWFILE, 'rb').readlines():
                        fields = line.strip().split(':')
                        if not fields[0] == self.name:
                            lines.append(line)
                            continue
                        fields[1] = self.password
                        fields[2] = str(int(time.time() / 86400))
                        line = ':'.join(fields)
                        lines.append('%s\n' % line)
                    open(self.SHADOWFILE, 'w+').writelines(lines)
                except Exception, err:
                    self.module.fail_json(msg="failed to update users password: %s" % str(err))

            return (rc, out, err)

    def modify_user_usermod(self):
        cmd = [self.module.get_bin_path('usermod', True)]
        cmd_len = len(cmd)
        info = self.user_info()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

            if self.non_unique:
                cmd.append('-o')

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups = self.get_groups_set()
            group_diff = set(current_groups).symmetric_difference(groups)
            groups_need_mod = False

            if group_diff:
                if self.append:
                    for g in groups:
                        if g in group_diff:
                            groups_need_mod = True
                            break
                else:
                    groups_need_mod = True

            if groups_need_mod:
                cmd.append('-G')
                new_groups = groups
                if self.append:
                    new_groups.extend(current_groups)
                cmd.append(','.join(new_groups))

        if self.comment is not None and info[4] != self.comment:
                cmd.append('-c')
                cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            if self.move_home:
                cmd.append('-m')
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.module.check_mode:
            return (0, '', '')
        else:
            # modify the user if cmd will do anything
            if cmd_len != len(cmd):
                cmd.append(self.name)
                (rc, out, err) = self.execute_command(cmd)
                if rc is not None and rc != 0:
                    self.module.fail_json(name=self.name, msg=err, rc=rc)
            else:
                (rc, out, err) = (None, '', '')

            # we have to set the password by editing the /etc/shadow file 
            if self.update_password == 'always' and self.password is not None and info[1] != self.password:
                try:
                    lines = []
                    for line in open(self.SHADOWFILE, 'rb').readlines():
                        fields = line.strip().split(':')
                        if not fields[0] == self.name:
                            lines.append(line)
                            continue
                        fields[1] = self.password
                        fields[2] = str(int(time.time() / 86400))	
                        line = ':'.join(fields)
                        lines.append('%s\n' % line)
                    open(self.SHADOWFILE, 'w+').writelines(lines)
                    rc = 0
                except Exception, err:
                    self.module.fail_json(msg="failed to update users password: %s" % str(err))

            return (rc, out, err)

# ===========================================

class AIX(User):
    """
    This is a AIX User manipulation class.

    This overrides the following methods from the generic class:-
      - create_user()
      - remove_user()
      - modify_user()
    """

    platform = 'AIX'
    distribution = None
    SHADOWFILE = '/etc/security/passwd'

    def remove_user(self):
        cmd = [self.module.get_bin_path('userdel', True)]
        if self.remove:
            cmd.append('-r')
        cmd.append(self.name)

        return self.execute_command(cmd)

    def create_user_useradd(self, command_name='useradd'):
        cmd = [self.module.get_bin_path(command_name, True)]

        if self.uid is not None:
            cmd.append('-u')
            cmd.append(self.uid)

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            cmd.append('-g')
            cmd.append(self.group)

        if self.groups is not None and len(self.groups):
            groups = self.get_groups_set()
            cmd.append('-G')
            cmd.append(','.join(groups))

        if self.comment is not None:
            cmd.append('-c')
            cmd.append(self.comment)

        if self.home is not None:
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None:
            cmd.append('-s')
            cmd.append(self.shell)

        if self.createhome:
            cmd.append('-m')

        cmd.append(self.name)
        (rc, out, err) = self.execute_command(cmd)

        # set password with chpasswd
        if self.password is not None:
            cmd = []
            cmd.append('echo "'+self.name+':'+self.password+'" |')
            cmd.append(self.module.get_bin_path('chpasswd', True))
            cmd.append('-e')
            cmd.append('-c')
            self.execute_command(' '.join(cmd))

        return (rc, out, err)

    def modify_user_usermod(self):
        cmd = [self.module.get_bin_path('usermod', True)]
        info = self.user_info()

        if self.uid is not None and info[2] != int(self.uid):
            cmd.append('-u')
            cmd.append(self.uid)

        if self.group is not None:
            if not self.group_exists(self.group):
                self.module.fail_json(msg="Group %s does not exist" % self.group)
            ginfo = self.group_info(self.group)
            if info[3] != ginfo[2]:
                cmd.append('-g')
                cmd.append(self.group)

        if self.groups is not None:
            current_groups = self.user_group_membership()
            groups_need_mod = False
            groups = []

            if self.groups == '':
                if current_groups and not self.append:
                    groups_need_mod = True
            else:
                groups = self.get_groups_set()
                group_diff = set(current_groups).symmetric_difference(groups)

                if group_diff:
                    if self.append:
                        for g in groups:
                            if g in group_diff:
                                groups_need_mod = True
                                break
                    else:
                        groups_need_mod = True

            if groups_need_mod:
                cmd.append('-G')
                cmd.append(','.join(groups))

        if self.comment is not None and info[4] != self.comment:
                cmd.append('-c')
                cmd.append(self.comment)

        if self.home is not None and info[5] != self.home:
            if self.move_home:
                cmd.append('-m')
            cmd.append('-d')
            cmd.append(self.home)

        if self.shell is not None and info[6] != self.shell:
            cmd.append('-s')
            cmd.append(self.shell)


        # skip if no changes to be made
        if len(cmd) == 1:
            (rc, out, err) = (None, '', '')
        elif self.module.check_mode:
            return (True, '', '')
        else:
            cmd.append(self.name)
            (rc, out, err) = self.execute_command(cmd)

        # set password with chpasswd
        if self.update_password == 'always' and self.password is not None and info[1] != self.password:
            cmd = []
            cmd.append('echo "'+self.name+':'+self.password+'" |')
            cmd.append(self.module.get_bin_path('chpasswd', True))
            cmd.append('-e')
            cmd.append('-c')
            (rc2, out2, err2) = self.execute_command(' '.join(cmd))
        else:
            (rc2, out2, err2) = (None, '', '')

        if rc != None:
            return (rc, out+out2, err+err2)
        else:
            return (rc2, out+out2, err+err2)

# ===========================================

def main():
    ssh_defaults = {
            'bits': '2048',
            'type': 'rsa',
            'passphrase': None,
            'comment': 'ansible-generated'
    }
    module = AnsibleModule(
        argument_spec = dict(
            state=dict(default='present', choices=['present', 'absent'], type='str'),
            name=dict(required=True, aliases=['user'], type='str'),
            uid=dict(default=None, type='str'),
            non_unique=dict(default='no', type='bool'),
            group=dict(default=None, type='str'),
            groups=dict(default=None, type='str'),
            comment=dict(default=None, type='str'),
            home=dict(default=None, type='str'),
            shell=dict(default=None, type='str'),
            password=dict(default=None, type='str'),
            login_class=dict(default=None, type='str'),
            # following options are specific to userdel
            force=dict(default='no', type='bool'),
            remove=dict(default='no', type='bool'),
            # following options are specific to useradd
            createhome=dict(default='yes', type='bool'),
            system=dict(default='no', type='bool'),
            # following options are specific to usermod
            move_home=dict(default='no', type='bool'),
            append=dict(default='no', type='bool'),
            # following are specific to ssh key generation
            generate_ssh_key=dict(type='bool'),
            ssh_key_bits=dict(default=ssh_defaults['bits'], type='str'),
            ssh_key_type=dict(default=ssh_defaults['type'], type='str'),
            ssh_key_file=dict(default=None, type='str'),
            ssh_key_comment=dict(default=ssh_defaults['comment'], type='str'),
            ssh_key_passphrase=dict(default=None, type='str'),
            update_password=dict(default='always',choices=['always','on_create'],type='str')
        ),
        supports_check_mode=True
    )

    user = User(module)

    if user.syslogging:
        syslog.openlog('ansible-%s' % os.path.basename(__file__))
        syslog.syslog(syslog.LOG_NOTICE, 'User instantiated - platform %s' % user.platform)
        if user.distribution:
            syslog.syslog(syslog.LOG_NOTICE, 'User instantiated - distribution %s' % user.distribution)

    rc = None
    out = ''
    err = ''
    result = {}
    result['name'] = user.name
    result['state'] = user.state
    if user.state == 'absent':
        if user.user_exists():
            if module.check_mode:
                module.exit_json(changed=True)
            (rc, out, err) = user.remove_user()
            if rc != 0:
                module.fail_json(name=user.name, msg=err, rc=rc)
            result['force'] = user.force
            result['remove'] = user.remove
    elif user.state == 'present':
        if not user.user_exists():
            if module.check_mode:
                module.exit_json(changed=True)
            (rc, out, err) = user.create_user()
            result['system'] = user.system
            result['createhome'] = user.createhome
        else:
            # modify user (note: this function is check mode aware)
            (rc, out, err) = user.modify_user()
            result['append'] = user.append
            result['move_home'] = user.move_home
        if rc is not None and rc != 0:
            module.fail_json(name=user.name, msg=err, rc=rc)
        if user.password is not None:
            result['password'] = 'NOT_LOGGING_PASSWORD'

    if rc is None:
        result['changed'] = False
    else:
        result['changed'] = True
    if out:
        result['stdout'] = out
    if err:
        result['stderr'] = err

    if user.user_exists():
        info = user.user_info()
        if info == False:
            result['msg'] = "failed to look up user name: %s" % user.name
            result['failed'] = True
        result['uid'] = info[2]
        result['group'] = info[3]
        result['comment'] = info[4]
        result['home'] = info[5]
        result['shell'] = info[6]
        result['uid'] = info[2]
        if user.groups is not None:
            result['groups'] = user.groups

        # deal with ssh key
        if user.sshkeygen:
            (rc, out, err) = user.ssh_key_gen()
            if rc is not None and rc != 0:
                module.fail_json(name=user.name, msg=err, rc=rc)
            if rc == 0:
                result['changed'] = True
            (rc, out, err) = user.ssh_key_fingerprint()
            if rc == 0:
                result['ssh_fingerprint'] = out.strip()
            else:
                result['ssh_fingerprint'] = err.strip()
            result['ssh_key_file'] = user.get_ssh_key_path()
            result['ssh_public_key'] = user.get_ssh_public_key()

        # handle missing homedirs
        info = user.user_info()
        if user.home is None:
            user.home = info[5]
        if not os.path.exists(user.home) and user.createhome:
            if not module.check_mode:
                user.create_homedir(user.home)
                user.chown_homedir(info[2], info[3], user.home)
            result['changed'] = True

    module.exit_json(**result)

# import module snippets
from ansible.module_utils.basic import *
main()
