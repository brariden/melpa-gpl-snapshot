[![MELPA](http://melpa.org/packages/spu-badge.svg)](http://melpa.org/#/spu)

# SPU

SPU stands for Silent Package Upgrader.

It does what its name says.

## Introduction
There are numerous packages for upgrading packages. However, I have not met one that can suppress the freezing during `Contacting Host: elpa.gnus.org:80` and `Contacting Host: melpa.org:433`. The process takes about 5 to 10 seconds. I always believe from the end user point of view, the UI freezing for 1 second is long, 2 seconds is too long, longer than 3 seconds is unacceptable. So I wrote this package.

The second purpose of writing this package is to demonstrate the use of [timp](https://github.com/mola-T/timp).

## Requirement

* Emacs 24.4

## Dependency

* signal.el - https://github.com/mola-T/signal
* timp.el - https://github.com/mola-T/timp


## Usage

Put this in your `init` file. This will upgrade packages daily.

``` elisp
(spu-package-upgrade-daily)
```

If you use `use-package`, you can do:

``` elisp
(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config (spu-package-upgrade-daily))

```

<br>

There is an on demand interactive upgrade package command <kbd>M-x</kbd> `spu-package-upgrade`. You may want to make a keybind.

<br>

If you are uncomfortable with the silent package upgrade (Although I think no one will inspect the source code before they upgrade a package), you can add `(setq spu-require-confirm-upgrade-package t)` to your `init` file.

<br>

To view the package upgrade report, you can do <kbd>M-x</kbd> `spu-view-upgrade-log`.


<br>
_______

<br>

## Contacts

mola@molamola.xyz

If you find any bugs or have any suggestions, you can make a pull request, report an issue or send me an email.

<br>

____________________________

<br>

## Support

[![paypal](image/buy_me_a_beer.png)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=Q2965NHMCR4XQ)    [![paypal](image/buy_me_a_bear.png)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=J573CXHTDTVNJ)

<br>

_____

<br>

## License

* Copyright (C) 2015-2016 Mola-T
* Author: Mola-T <Mola@molamola.xyz>
* URL: https://github.com/mola-T/spu
* [GNU GENERAL PUBLIC LICENSE](LICENSE)
