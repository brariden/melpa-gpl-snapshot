# angularjs-mode for Emacs

[![MELPA](http://melpa.org/packages/angularjs-mode-badge.svg)](http://melpa.org/#/angularjs-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

If you're looking for snippets, check out
[angularjs-snippets](https://github.com/magnars/angular-snippets.el).

There are two modes:

* angular-mode
* angular-html-mode

*angular-mode* provides font-lock syntax highlighting for JavaScript
files and is derived from js-mode.

*angular-html-mode* provides font-lock syntax highlighting for HTML
templates and is derived from html-mode.

In the future there should be a speedbar minor mode that lists
controllers, directives, services and filters defined in a file.

## YASnippets, Code Snippet Expanding

For both angular-mode and angular-html-mode, there are snippets
defined for [YASnippet](http://emacswiki.org/emacs/Yasnippet) in the
`snippets` directory.

You can load the snippets like this:

    (add-to-list 'yas-snippet-dirs "/path/to/angularjs-mode/snippets")

[![Using YASnippet for AngularJS](https://img.youtube.com/vi/GniuRBlfPsw/0.jpg)](https://www.youtube.com/watch?v=GniuRBlfPsw)

## Auto-Complete (Tab Completion)

For both angular-mode and angular-html-mode, there are dictionaries
defined for [Auto-Complete](http://auto-complete.org/) in the
`ac-dict` directory.

You can load the auto-complete dictionaries like this:

    (add-to-list 'ac-dictionary-directories "/path/to/angularjs-mode/ac-dict")
    (add-to-list 'ac-modes 'angular-mode)
    (add-to-list 'ac-modes 'angular-html-mode)

# Copyright and License

Copyright (C) 2013-2015 Rudolf Olah <omouse@gmail.com>

Licensed under the GNU GPL version 3 or later, see LICENSE for the
full text of the license.
