## Caroline, an Emacs 24 Theme

An Emacs 24 color scheme!

![](/caroline.png)

![](/caroline2.png)

![](/caroline3.png)

## Manual Install

Add this right here to your `.emacs` config file.

![](/caroline4.png)

`(load-theme 'caroline t)`

Will have it load during emacs startup.

`M-x load-theme RET caroline` will load it manually.

Download caroline-theme.el to the directory ~/.emacs.d/themes/ and add this to your .emacs:

```emacs-lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
```

### Contributing

Feel free to yank it down and do whatever you want with it.

UPDATE : I just added this to MELPA as of 10/30/15.
