In `auto-capitalize' minor mode, the first word at the beginning of
a paragraph or sentence (i.e. at `left-margin' on a line following
`paragraph-separate', after `paragraph-start' at `left-margin', or
after `sentence-end') is automatically capitalized when a following
whitespace or punctuation character is inserted.

The `auto-capitalize-words' variable can be customized so that
commonly used proper nouns and acronyms are capitalized or upcased,
respectively.

The `auto-capitalize-yank' option controls whether words in yanked
text should by capitalized in the same way.

To install auto-capitalize.el, copy it to a `load-path' directory,
`M-x byte-compile-file' it, and add this to your
site-lisp/default.el or ~/.emacs file:
	(autoload 'auto-capitalize-mode "auto-capitalize"
	  "Toggle `auto-capitalize' minor mode in this buffer." t)
	(autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
	  "Turn on `auto-capitalize' minor mode in this buffer." t)
	(autoload 'enable-auto-capitalize-mode "auto-capitalize"
	  "Enable `auto-capitalize' minor mode in this buffer." t)

To turn on (unconditional) capitalization in all Text modes, add
this to your site-lisp/default.el or ~/.emacs file:
	(add-hook 'text-mode-hook 'turn-on-auto-capitalize-mode)
To enable (interactive) capitalization in all Text modes, add this
to your site-lisp/default.el or ~/.emacs file:
	(add-hook 'text-mode-hook 'enable-auto-capitalize-mode)

To prevent a word from ever being capitalized or upcased
(e.g. "http"), simply add it (in lowercase) to the
`auto-capitalize-words' list.

To prevent a word in the `auto-capitalize-words' list from being
capitalized or upcased in a particular context (e.g.
"GNU.emacs.sources"), insert the following whitespace or
punctuation character with `M-x quoted-insert' (e.g. `gnu C-q .').

To enable contractions based on a word in the
`auto-capitalize-words' list to be capitalized or upcased
(e.g. "I'm") in the middle of a sentence in Text mode, define the
apostrophe as a punctuation character or as a symbol that joins two
words:
	;; Use "_" instead of "." to define apostrophe as a symbol:
	(modify-syntax-entry ?' ".   " text-mode-syntax-table) ; was "w   "
