.. _dynamic_inventory:

Dynamic Inventory
=================

.. contents:: Topics

Often a user of a configuration management system will want to keep inventory
in a different software system.  Ansible provides a basic text-based system as described in
:doc:`intro_inventory` but what if you want to use something else?

Frequent examples include pulling inventory from a cloud provider, LDAP, `Cobbler <http://cobbler.github.com>`_,
or a piece of expensive enterprisey CMDB software.

Ansible easily supports all of these options via an external inventory system.  The plugins directory contains some of these already -- including options for EC2/Eucalyptus, Rackspace Cloud, and OpenStack, examples of some of which will be detailed below.

:doc:`tower` also provides a database to store inventory results that is both web and REST Accessible.  Tower syncs with all Ansible dynamic inventory sources you might be using, and also includes a graphical inventory editor. By having a database record of all of your hosts, it's easy to correlate past event history and see which ones have had failures on their last playbook runs.

For information about writing your own dynamic inventory source, see :doc:`developing_inventory`.


.. _cobbler_example:

Example: The Cobbler External Inventory Script
``````````````````````````````````````````````

It is expected that many Ansible users with a reasonable amount of physical hardware may also be `Cobbler <http://cobbler.github.com>`_ users.  (note: Cobbler was originally written by Michael DeHaan and is now lead by James Cammarata, who also works for Ansible, Inc).

While primarily used to kickoff OS installations and manage DHCP and DNS, Cobbler has a generic
layer that allows it to represent data for multiple configuration management systems (even at the same time), and has
been referred to as a 'lightweight CMDB' by some admins.

To tie Ansible's inventory to Cobbler (optional), copy `this script <https://raw.github.com/ansible/ansible/devel/plugins/inventory/cobbler.py>`_ to /etc/ansible and `chmod +x` the file.  cobblerd will now need
to be running when you are using Ansible and you'll need to use Ansible's  ``-i`` command line option (e.g. ``-i /etc/ansible/cobbler.py``).
This particular script will communicate with Cobbler using Cobbler's XMLRPC API.

First test the script by running ``/etc/ansible/cobbler.py`` directly.   You should see some JSON data output, but it may not have anything in it just yet.

Let's explore what this does.  In cobbler, assume a scenario somewhat like the following::

    cobbler profile add --name=webserver --distro=CentOS6-x86_64
    cobbler profile edit --name=webserver --mgmt-classes="webserver" --ksmeta="a=2 b=3"
    cobbler system edit --name=foo --dns-name="foo.example.com" --mgmt-classes="atlanta" --ksmeta="c=4"
    cobbler system edit --name=bar --dns-name="bar.example.com" --mgmt-classes="atlanta" --ksmeta="c=5"

In the example above, the system 'foo.example.com' will be addressable by ansible directly, but will also be addressable when using the group names 'webserver' or 'atlanta'.  Since Ansible uses SSH, we'll try to contact system foo over 'foo.example.com', only, never just 'foo'.  Similarly, if you try "ansible foo" it wouldn't find the system... but "ansible 'foo*'" would, because the system DNS name starts with 'foo'.

The script doesn't just provide host and group info.  In addition, as a bonus, when the 'setup' module is run (which happens automatically when using playbooks), the variables 'a', 'b', and 'c' will all be auto-populated in the templates::

    # file: /srv/motd.j2
    Welcome, I am templated with a value of a={{ a }}, b={{ b }}, and c={{ c }}

Which could be executed just like this::

    ansible webserver -m setup
    ansible webserver -m template -a "src=/tmp/motd.j2 dest=/etc/motd"

.. note::
   The name 'webserver' came from cobbler, as did the variables for
   the config file.  You can still pass in your own variables like
   normal in Ansible, but variables from the external inventory script
   will override any that have the same name.

So, with the template above (motd.j2), this would result in the following data being written to /etc/motd for system 'foo'::

    Welcome, I am templated with a value of a=2, b=3, and c=4

And on system 'bar' (bar.example.com)::

    Welcome, I am templated with a value of a=2, b=3, and c=5

And technically, though there is no major good reason to do it, this also works too::

    ansible webserver -m shell -a "echo {{ a }}"

So in other words, you can use those variables in arguments/actions as well.

.. _aws_example:

Example: AWS EC2 External Inventory Script
``````````````````````````````````````````

If you use Amazon Web Services EC2, maintaining an inventory file might not be the best approach, because hosts may come and go over time, be managed by external applications, or you might even be using AWS autoscaling. For this reason, you can use the `EC2 external inventory  <https://raw.github.com/ansible/ansible/devel/plugins/inventory/ec2.py>`_ script.

You can use this script in one of two ways. The easiest is to use Ansible's ``-i`` command line option and specify the path to the script after
marking it executable::

    ansible -i ec2.py -u ubuntu us-east-1d -m ping

The second option is to copy the script to `/etc/ansible/hosts` and `chmod +x` it. You will also need to copy the `ec2.ini  <https://raw.githubusercontent.com/ansible/ansible/devel/plugins/inventory/ec2.ini>`_ file to `/etc/ansible/ec2.ini`. Then you can run ansible as you would normally.

To successfully make an API call to AWS, you will need to configure Boto (the Python interface to AWS). There are a `variety of methods <http://docs.pythonboto.org/en/latest/boto_config_tut.html>`_ available, but the simplest is just to export two environment variables::

    export AWS_ACCESS_KEY_ID='AK123'
    export AWS_SECRET_ACCESS_KEY='abc123'

You can test the script by itself to make sure your config is correct::

    cd plugins/inventory
    ./ec2.py --list

After a few moments, you should see your entire EC2 inventory across all regions in JSON.

Since each region requires its own API call, if you are only using a small set of regions, feel free to edit ``ec2.ini`` and list only the regions you are interested in. There are other config options in ``ec2.ini`` including cache control, and destination variables.

At their heart, inventory files are simply a mapping from some name to a destination address. The default ``ec2.ini`` settings are configured for running Ansible from outside EC2 (from your laptop for example) -- and this is not the most efficient way to manage EC2.

If you are running Ansible from within EC2, internal DNS names and IP addresses may make more sense than public DNS names. In this case, you can modify the ``destination_variable`` in ``ec2.ini`` to be the private DNS name of an instance. This is particularly important when running Ansible within a private subnet inside a VPC, where the only way to access an instance is via its private IP address. For VPC instances, `vpc_destination_variable` in ``ec2.ini`` provides a means of using which ever `boto.ec2.instance variable <http://docs.pythonboto.org/en/latest/ref/ec2.html#module-boto.ec2.instance>`_ makes the most sense for your use case.

The EC2 external inventory provides mappings to instances from several groups:

Global
  All instances are in group ``ec2``.

Instance ID
  These are groups of one since instance IDs are unique.
  e.g.
  ``i-00112233``
  ``i-a1b1c1d1``

Region
  A group of all instances in an AWS region.
  e.g.
  ``us-east-1``
  ``us-west-2``

Availability Zone
  A group of all instances in an availability zone.
  e.g.
  ``us-east-1a``
  ``us-east-1b``

Security Group
  Instances belong to one or more security groups. A group is created for each security group, with all characters except alphanumerics, dashes (-) converted to underscores (_). Each group is prefixed by ``security_group_``
  e.g.
  ``security_group_default``
  ``security_group_webservers``
  ``security_group_Pete_s_Fancy_Group``

Tags
  Each instance can have a variety of key/value pairs associated with it called Tags. The most common tag key is 'Name', though anything is possible. Each key/value pair is its own group of instances, again with special characters converted to underscores, in the format ``tag_KEY_VALUE``
  e.g.
  ``tag_Name_Web``
  ``tag_Name_redis-master-001``
  ``tag_aws_cloudformation_logical-id_WebServerGroup``

When the Ansible is interacting with a specific server, the EC2 inventory script is called again with the ``--host HOST`` option. This looks up the HOST in the index cache to get the instance ID, and then makes an API call to AWS to get information about that specific instance. It then makes information about that instance available as variables to your playbooks. Each variable is prefixed by ``ec2_``. Here are some of the variables available:

- ec2_architecture
- ec2_description
- ec2_dns_name
- ec2_id
- ec2_image_id
- ec2_instance_type
- ec2_ip_address
- ec2_kernel
- ec2_key_name
- ec2_launch_time
- ec2_monitored
- ec2_ownerId
- ec2_placement
- ec2_platform
- ec2_previous_state
- ec2_private_dns_name
- ec2_private_ip_address
- ec2_public_dns_name
- ec2_ramdisk
- ec2_region
- ec2_root_device_name
- ec2_root_device_type
- ec2_security_group_ids
- ec2_security_group_names
- ec2_spot_instance_request_id
- ec2_state
- ec2_state_code
- ec2_state_reason
- ec2_status
- ec2_subnet_id
- ec2_tag_Name
- ec2_tenancy
- ec2_virtualization_type
- ec2_vpc_id

Both ``ec2_security_group_ids`` and ``ec2_security_group_names`` are comma-separated lists of all security groups. Each EC2 tag is a variable in the format ``ec2_tag_KEY``.

To see the complete list of variables available for an instance, run the script by itself::

    cd plugins/inventory
    ./ec2.py --host ec2-12-12-12-12.compute-1.amazonaws.com

Note that the AWS inventory script will cache results to avoid repeated API calls, and this cache setting is configurable in ec2.ini.  To
explicitly clear the cache, you can run the ec2.py script with the ``--refresh-cache`` parameter.

.. _other_inventory_scripts:

Other inventory scripts
```````````````````````

In addition to Cobbler and EC2, inventory scripts are also available for::

   BSD Jails
   DigitalOcean
   Google Compute Engine
   Linode
   OpenShift
   OpenStack Nova
   Red Hat's SpaceWalk
   Vagrant (not to be confused with the provisioner in vagrant, which is preferred)
   Zabbix

Sections on how to use these in more detail will be added over time, but by looking at the "plugins/" directory of the Ansible checkout
it should be very obvious how to use them.  The process for the AWS inventory script is the same.

If you develop an interesting inventory script that might be general purpose, please submit a pull request -- we'd likely be glad
to include it in the project.

.. _using_multiple_sources:

Using Multiple Inventory Sources
````````````````````````````````

If the location given to -i in Ansible is a directory (or as so configured in ansible.cfg), Ansible can use multiple inventory sources
at the same time.  When doing so, it is possible to mix both dynamic and statically managed inventory sources in the same ansible run.  Instant
hybrid cloud!

.. seealso::

   :doc:`intro_inventory`
       All about static inventory files
   `Mailing List <http://groups.google.com/group/ansible-project>`_
       Questions? Help? Ideas?  Stop by the list on Google Groups
   `irc.freenode.net <http://irc.freenode.net>`_
       #ansible IRC chat channel

