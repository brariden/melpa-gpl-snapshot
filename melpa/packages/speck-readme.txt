Speck is a minor mode for "specking" - spell-checking text displayed
in Emacs windows.  Invoke the command `speck-mode' to toggle specking
of all windows showing the current buffer.

Change Log:
2016/07/17 Felix E. Klee
    Fix: Loading buffer local dictionary fails for Hunspell.
    Fix: Input encoding not passed to Hunspell
    Change default Hunspell coding system for `ru_RU' to `utf-8'.
2014/08/30 York Zhao
    Add support to allow binding a lisp function to a "replace key" in
    `speck-replace-keys' and/or `speck-replace-map', so that one can define a
    function to do more hacking. For instance, one can define a function, and
    bind it to the ESC key in `speck-replace-keys' and/or
    `speck-replace-map', to decide whether to accept the speck correction, by
    returning non-nil, or reject it, by returning nil. One use case is that
    in Evil, a Vim emulation in Emacs, one can allow ESC to accept a speck
    correction only in insert state.
2014/04/16 York Zhao
    Fix whitespace, replace TABs with spaces.
2013/12/30 York Zhao
    Require the `cl-lib', instead of `cl'
2013/05/19 Andrei Chițu
    bugfix: bugfix: in `speck-windows' check that `window' is non-nil before specking it
    bugfix: keep case of dictionary names in `speck-hunspell-dictionary-alist'
2013/02/03 Frank Fischer
    Add LIMIT to `looking-back' in `speck-auto-correct-after-change'.
    Set `speck-auto-correct-after-change' to nil.

_____________________________________________________________________________

                           General options
_____________________________________________________________________________
