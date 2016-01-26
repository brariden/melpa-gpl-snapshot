> Note: `flycheck-purescript.el' uses --json-errors flag available in
> PureScript>=0.8, for older versions, you can use:
> https://github.com/emacs-pe/purescript-mode/blob/4aca396/flycheck-purescript.el

## Setup

    (eval-after-load 'flycheck
      '(flycheck-purescript-setup))
