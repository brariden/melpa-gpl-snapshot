## Required for 1.0

* Much improved submode guessing.

* Views: named sets of filters that you can save and reuse later.


## Other ideas

* More movement commands: move inside the same thread, move across
  entries of some view (see above) without activating it.  This is not
  so difficult to implement, but requires pondering on how to make
  the commands comfortable to use.

* Add a way to always show errors/warnings even if they would
  otherwise be filtered out.  Explicit hiding should still take
  precedence though.

* Idle buffer parsing/filtering, otherwise mode is semi-useless in
  huge logs.

* Undo/redo for various filtering and explicit hiding operations.

* Context when filtering (like grep -C): optionally show N entries
  before/after each that matches filter.

* Sections: somehow make certain entries stand out and add navigation
  to the section start, narrow to section etc.  The idea is that
  sections can be made to span single request to your server
  (optionally bind to threads too).  Probably requires views.

* Replace timestamps with difference (likely to section start, as
  defined above) on demand.  E.g. something like this:

      18:11:03.038 [org.me.MyServer] processing request to 'Foo'
            +0.003 [org.me.SpecificServlet] initializing
            +0.004 [org.me.DatabaseUtils] querying the database: '...'

* Maybe optionally highlight the current entry?  Though we already use
  background color heavily.
