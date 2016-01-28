This minor mode will add little +/- displays to foldable regions in the
buffer and to folded regions. It is indented to be used in conjunction with
hideshow.el which is a part of GNU Emacs since version 20.

Currently it works for me but is not tested heavily. Please report any bugs
to the above email address

Installation:
Add the following to your .emacs:

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)


(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

If enabling hideshowvis-minor-mode is slow on your machine use M-x,
customize-option, hideshowvis-ignore-same-line and set it to nil. This will
then display - icons for foldable regions of one line, too but is faster

To enable displaying a + symbol in the fringe for folded regions,
use:

   (hideshowvis-symbols)

in your ~/.emacs

It is not enabled by default because it might interfere with custom
hs-set-up-overlay functions

Changelog

v0.5, 2012-09-11
- Made ELPA compliant and added function `hideshowvis-symbols'

v0.4, 2012-03-13
- fixed bug causing transpose-words to be broken as well as causing problems
  when auto-fill-mode was enabled

v0.3, 2010-08-26
- added autoload cookies
- fixed bug causing major mode menu to disappear, among other things

v0.2, 2009-08-09
- '-' symbol in fringe is clickable
- don't show '-' in fringe if the foldable region ends on the same line
