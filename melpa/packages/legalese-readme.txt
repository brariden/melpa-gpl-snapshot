This file adds the sometimes necessary legalese to your files. That
is, it adds a license header. For most files, it tries to adhere to
a nice standard layout.

I recommend the following setup:

(setq comment-style 'extra-line)
(add-hook 'scheme-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-add) 1)))
