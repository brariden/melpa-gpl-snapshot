This package was created to eliminate the need for bindings variations of the same core `ido-mode` driven commands that have different window or frame targets. Instead, you can select the target window and frame when making your selection from within `ido`.

A basic example - instead of having to remember the bindings to trigger either `(ido-switch-buffer)` or `(ido-switch-buffer-other-window)`, you can just call `(ido-switch-buffer)` and then trigger `<C-return> o` if you want to open/view your selection in `other-window`.

### Usage ###

The default `ido-exit-target-keymap-prefix` is set to `<C-return>`, which is only active in the `ido` minibuffer. This translates into the following target commands for `ido` selection:

binding       |command
--------------|-------
`<C-return> 1`|ido-exit-target-one-window
`<C-return> 2`|ido-exit-target-split-window-below
`<C-return> 3`|ido-exit-target-split-window-right
`<C-return> 4`|ido-exit-target-other-window
`<C-return> 5`|ido-exit-target-other-frame
`<C-return> o`|ido-exit-target-other-window

You can change the keymap prefix to something else, or bind commands directly to `ido-common-completion-map` yourself.

Example custom setup:

```elisp
(require 'ido-exit-target)
(setq ido-exit-target-keymap-prefix (kbd "s-."))
(define-key ido-common-completion-map (kbd "<s-return>") #'ido-exit-target-other-window)
```

<!-- ### Installation ### -->

<!-- Recommended install from [MELPA](melpa.milkbox.net) with `M-x package-install ido-exit-target`. -->
