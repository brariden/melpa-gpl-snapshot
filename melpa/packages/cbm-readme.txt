Installation:

Put cbm.el in your `load-path' and require it:

(require 'cbm)

It is recommended to bind `cbm-cycle', `cbm-switch-buffer' and
`cbm-find-org-agenda-file' to a key:

(global-set-key (kbd "C-;") #'cbm-cycle)
(global-set-key (kbd "C-'") #'cbm-switch-buffer)
(global-set-key (kbd "C-c o") #'cbm-find-org-agenda-file)

Usage:

This package provides one usefull commands for switching to
"similiar" buffers.
