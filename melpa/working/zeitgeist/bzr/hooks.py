# -.- coding: utf-8 -.-

# Zeitgeist
#
# Copyright © 2009 Markus Korn <thekorn@gmx.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os.path

from gi.repository import Gio

from bzrlib import (
    branch,
    errors,
    trace,
    urlutils,
    workingtree,
    )

_client = None
_client_checked = None

def get_client():
    global _client_checked, _client
    if _client_checked:
        return _client
    _client_checked = True
    try:
        from zeitgeist.client import ZeitgeistClient
    except ImportError:
        _client = None
        return _client
    import dbus
    try:
        _client = ZeitgeistClient()
    except dbus.DBusException, e:
        trace.warning("zeitgeist: %s. No events will be sent." % e.message)
        _client = None
    except Exception, e:
        trace.warning("Unable to connect to Zeitgeist, won't send events. "
                      "Reason: '%s'" % e)
        _client = None
    return _client


def subjects_for_branch(branch, new_revno, old_revno, is_push, message):
    """
    Generates the event's subject for commit, push and pull events
    
    branch - the bzrlib.branch.Branch instance
    new_revno - the new revision number generated after a commit, 
        after a push or a pull
    old_revno - the old revno which is was present before the commit,
        push or pull event
    is_push - True then push, is False then pull, is None then commit
    message - the Commit message
    """

    from zeitgeist.datamodel import Subject, Interpretation, Manifestation
    location = urlutils.unescape_for_display(branch.base, "utf-8").decode("utf-8")
    
    if is_push is None:
        # Commit
        text = u"Revision: %s, Message: %s" %(new_revno, message)
    elif is_push is True:
        # Push
        text = "Pushed branch from %s to %s" %(old_revno, new_revno)
    elif is_push is False:
        # Pull
        text = "Updated branch from %s to %s" %(old_revno, new_revno)
    
    folder_subj =  Subject.new_for_values(
        uri=unicode(branch.base),
        interpretation=unicode(Interpretation.FOLDER),
        manifestation=unicode(Manifestation.FILE_DATA_OBJECT),
        text=text,
        origin=unicode(branch.base),
        mimetype="application/x-bzr",
    )
    
    all_subjs = [folder_subj, ]
    
    # List down the files only in case of Commit as in case of 
    # push or pull, remote URI is used using which constructing
    # file URI is not possible
    if is_push is None:
        prev_tree = branch.repository.revision_tree(branch.get_rev_id(old_revno))
        next_tree = branch.repository.revision_tree(branch.get_rev_id(new_revno))
    
        for (file_id, path, content_change, versioned, parent_id, name, kind,
             executable) in next_tree.iter_changes(prev_tree):
             
            name = path[0] if path[0] is not None else path[1]
            uri = os.path.join(unicode(branch.base), name)
             
            # fi.get_content_type returns the file mimetype 
            f=Gio.File.new_for_uri(uri)
            fi=f.query_info("*", Gio.FileQueryInfoFlags.NONE, None)
             
            subj = Subject.new_for_values(
                uri=uri,
                manifestation=unicode(Manifestation.FILE_DATA_OBJECT),
                text=text,
                origin=unicode(branch.base),
                mimetype=fi.get_content_type(),
            )
            all_subjs.append(subj)
    
    return all_subjs
    
def get_actor():
    # FIXME: Allow overriding this by e.g. qbzr and bzr-gtk
    return u"application://bzr.desktop"


def post_commit(local, master, old_revno, old_revid, new_revno, new_revid):
    client = get_client()
    if client is None:
        return
    from zeitgeist.datamodel import Event, Interpretation, Manifestation
    revision = master.repository.get_revision(new_revid)
    if new_revno == 1:
        interpretation = Interpretation.CREATE_EVENT
    else:
        interpretation = Interpretation.MODIFY_EVENT
    _text = _("Revision: ")
    _text += str(new_revno) + "\n"
    _text += revision.message.rstrip()

    subjects = subjects_for_branch(master, new_revno, old_revno, 
                None, revision.message.rstrip())
    #subjects.set_text(_text)
    event = Event.new_for_values(
        timestamp=int(revision.timestamp*1000),
        interpretation=unicode(interpretation),
        manifestation=unicode(Manifestation.USER_ACTIVITY),
        actor=get_actor(),
        subjects=subjects,
    )
    client.insert_event(event)


def post_pull(pull_result):
    client = get_client()
    if client is None:
        return
    from zeitgeist.datamodel import Event, Interpretation, Manifestation
    try:
        revision = pull_result.master_branch.repository.get_revision(
            pull_result.new_revid)
    except errors.UnsupportedOperation:
        # Some remote repository formats (e.g. git) don't support looking at invidual
        # revisions
        revision = pull_result.source_branch.repository.get_revision(
            pull_result.new_revid)
    _text = _("Pulled to revision %s:\n %s") % (pull_result.new_revno,
            revision.get_summary())
    subjects = subjects_for_branch(pull_result.master_branch, 
            pull_result.new_revno, pull_result.old_revno, False, None)
    event = Event.new_for_values(
        interpretation=unicode(Interpretation.RECEIVE_EVENT),
        manifestation=unicode(Manifestation.WORLD_ACTIVITY),
        actor=get_actor(),
        subjects=subjects
    )
    client.insert_event(event)


def post_push(push_result):
    client = get_client()
    if client is None:
        return
    from zeitgeist.datamodel import Event, Interpretation, Manifestation
    try:
        revision = push_result.master_branch.repository.get_revision(
            push_result.new_revid)
    except errors.UnsupportedOperation:
        revision = push_result.source_branch.repository.get_revision(
            push_result.new_revid)
    _text = _("Pushed to revision %s:\n %s") % (push_result.new_revno,
        revision.get_summary())
    subjects = subjects_for_branch(push_result.master_branch, 
            push_result.new_revno, push_result.old_revno, True, None)
    event = Event.new_for_values(
        interpretation=unicode(Interpretation.SEND_EVENT),
        manifestation=unicode(Manifestation.USER_ACTIVITY),
        actor=get_actor(),
        subjects=subjects
    )
    client.insert_event(event)



