Interface to a Transmission session.

Based on the JSON RPC library written by Christopher Wellons,
available online here: <https://github.com/skeeto/elisp-json-rpc>

Entry points are the `transmission' and `transmission-add'
commands.  A variety of commands are available for manipulating
torrents and their contents, some of which can be applied over
multiple items by selecting them within a region.

"M-x transmission RET" pops up a torrent list.  One can add,
start/stop, verify, remove torrents, set speed limits, ratio
limits, bandwidth priorities, trackers, etc.  Also, one can
navigate to either a file list or torrent info context.  In the
file list, individual files can be toggled for download, and their
priorities set.

Customize-able are the session address components, RPC credentials,
the display of dates, file sizes and transfer rates, and the
refreshing of the torrent list.  See the `transmission'
customization group.

The design draws from a number of sources, including the
"transmission-remote" command line utility and the
"transmission-remote-cli" ncurses interface.  These can be found
respectively at the following:
<https://trac.transmissionbt.com/browser/trunk/daemon/remote.c>
<https://github.com/fagga/transmission-remote-cli>
