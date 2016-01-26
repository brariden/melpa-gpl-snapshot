Activate the minor mode (M-x sqlup-mode) and type away
Alternatively, use a hook: (add-hook 'sql-mode 'sqlup-mode)
The capitalization is triggered when you press the following keys:
* SPC
* ,
* ;
* (
* \r (Enter)

This package also provides a function to capitalize SQL keywords inside a region - always available, no need to activate the minor mode to use it:

M-x sqlup-capitalize-keywords-in-region

It is not bound to a keybinding, but here is an example of how you could do it:

(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

Here follows an example setup to activate `sqlup-mode` automatically when entering sql-mode or sql-interactive-mode:

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
