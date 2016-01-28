`per-buffer-theme.el' is an Emacs library that automatically changes
the global theme according to buffer name or major mode.

It runs through `window-configuration-change-hook' so it is not perfect.

If buffer name matches any of `per-buffer-theme/ignored-buffernames-regex'
no theme change occurs.

Customizable variable `per-buffer-theme/themes-alist' contains the
association between themes and buffer name or major modes.

Special `notheme' theme can be used to make unload all themes and use emacs
default theme.

If no theme matches then it'll load the theme stored in
`per-buffer-theme/default-theme'.
