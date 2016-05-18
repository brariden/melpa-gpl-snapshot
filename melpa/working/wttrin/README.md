[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Gitter][gitter-badge]][gitter-chatroom]
[![Build Status][travis-ci-badge]][travis-ci-status]

# wttrin.el

Emacs frontend for weather web service [wttr.in].

## Usage

Set a default cities list for completion:

```elisp
(setq wttrin-default-cities '("Taipei" "Tainan"))
```

Then run `M-x wttrin` to get the information.

When the weather is displayed you can press `q` to quit the buffer or `g` to query for another city.

![screenshot]

## LICENSE

MIT

[wttr.in]: http://wttr.in/
[screenshot]: wttrin.png
[melpa-badge]: http://melpa.org/packages/wttrin-badge.svg
[melpa-package]: http://melpa.org/#/wttrin
[melpa-stable-badge]: http://stable.melpa.org/packages/wttrin-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/wttrin
[gitter-badge]: https://badges.gitter.im/bcbcarl/emacs-wttrin.svg
[gitter-chatroom]: https://gitter.im/bcbcarl/emacs-wttrin?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge
[travis-ci-badge]: https://travis-ci.org/bcbcarl/emacs-wttrin.svg?branch=master
[travis-ci-status]: https://travis-ci.org/bcbcarl/emacs-wttrin
