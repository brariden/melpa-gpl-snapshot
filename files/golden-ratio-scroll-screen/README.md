golden-ratio-scroll-screen
=================================

[![MELPA](http://melpa.org/packages/golden-ratio-scroll-screen-badge.svg)](http://melpa.org/#/golden-ratio-scroll-screen)

Scroll half screen down or up, and highlight current line before or after
scrolling.

Getting started
------------

The easiest way to get started is to install the package via [MELPA][melpa]:

 [melpa]: http://melpa.org/

```elisp
(package-install 'golden-ratio-scroll-screen)
```

Once installed, activate it by adding the following to your `~/.emacs` startup
file:

```elisp
(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)
```

Copyright & License
------------------------

Copyright (C) 2011~2015, 纪秀峰. Released under the terms of the GNU GPL v3+.
