[![MELPA](https://melpa.org/packages/haskell-tab-indent-badge.svg)](https://melpa.org/#/haskell-tab-indent) [![MELPA Stable](https://stable.melpa.org/packages/haskell-tab-indent-badge.svg)](https://stable.melpa.org/#/haskell-tab-indent)

This file provides `haskell-tab-indent-mode`, a simple Emacs
indentation minor mode for Haskell projects which require tabs for
indentation and do not permit spaces (except for where clauses, as a
special case).  A prominent example of such a project is
[git-annex][].

The user may use TAB to cycle between possible indentations.

# Installation

You may copy `haskell-tab-indent.el` to somewhere in your `load-path`
and simply

    (autoload 'haskell-tab-indent-mode "haskell-tab-indent.el")

Or you may use [git subtrees][]:

    emacs-pkg-subtree add https://git.spwhitton.name/haskell-tab-indent 0.1.0

[git subtrees]: https://spwhitton.name/blog/entry/emacs-pkg-subtree/

haskell-tab-indent is also available from [MELPA][] (and MELPA Stable).

[MELPA]: http://melpa.org/

# Usage

If you set `indent-tabs-mode` in the `.dir-locals.el` file for a
project requiring tabs, you can use something like this in your init
file to enable this mode for such projects, defaulting to the usual
`haskell-indentation-mode`:

    (add-hook 'haskell-mode-hook
                (lambda ()
                  (add-hook 'hack-local-variables-hook
                            (lambda ()
                              (if indent-tabs-mode
                                  (haskell-tab-indent-mode)
                                (haskell-indentation-mode)))
                            nil t))) ; local hook

[git-annex]: https://git-annex.branchable.com/coding_style/
