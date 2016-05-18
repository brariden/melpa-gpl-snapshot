flymake-solidity.el
===============

An Emacs flymake handler for syntax-checking Solidity source code.

Installation
=============

If you choose not to use one of the convenient packages in
[Melpa][melpa], you'll need to add the
directory containing `flymake-solidity.el` to your `load-path`, and then
`(require 'flymake-solidity)`. You'll also need to install
[flymake-easy](https://github.com/purcell/flymake-easy).

Usage
=====

Add the following to your emacs init file:

    (require 'flymake-solidity)
    (add-hook 'solidity-mode-hook 'flymake-solidity-load)

If the "solc" (solidity compiler) path is different from "/usr/local/bin/solc", then add the line below (after changing your path):

    (setq flymake-solidity-executable "/usr/local/bin/solc")

When flymake has been configured as such, you can see live updates while writing solidity code.

[melpa]: http://melpa.org

Screenshot
==========

![](https://github.com/kootenpv/flymake-solidity/blob/master/screenshot.png)

Credits
=======

Thanks to Steve Purcell who actually did the hard work by providing https://github.com/purcell/flymake-easy
If you want to make a flymake mode yourself, use flymake-easy!

<hr>

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](https://linkedin.com/in/pascalvkooten)
Donations through Ethereum are appreciated: 0xf3BbB1D46D360FF56f4D5DaEDb238f8b83F6b261
