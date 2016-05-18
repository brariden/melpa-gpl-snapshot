# KSP-CFG mode

[![MELPA](http://melpa.org/packages/ksp-cfg-mode-badge.svg)](http://melpa.org/#/ksp-cfg-mode) [![BSD3](https://img.shields.io/badge/license-BSD3-43cd80.svg)](LICENSE.md)

A major-mode for editing Kerbal Space Program part (and other things) config files in Emacs.

## Status

* Release 0.3; in MELPA!
* Syntactic lex with good (well, enthusiastic and correct if not tasteful) highlighting
* Intelligent automatic indentation
* Context-sensitive advice in the minibuffer

## Changes

* Changed dependency to cl-lib 0.5 only, instead of Emacs 24.3.
* Wrapped the `(require 'cl-lib)` inside a `(eval-when-compile)` as we're only using it for macros.
* Reorganized the buffer-local variables to use simply setq when they must already be buffer-local, and `(set (make-local-variable 'foo) 'bar)` when not, for compatibility with older emacsen.
* Removed unnecessary font-lock fontification and bad font-lock assumptions.

## To-Do features

* Implement line-relative re-indent function for proper O(n) buffer-wide re-indent.
* Set up travis-ci testing across various Emacs releases.
* Support more [ModuleManager](https://github.com/sarbian/ModuleManager) syntaxes; variables, regexps, and node-indexing aren't implemented.
* Refactor context help; a more flexible means of inserting new help tips on a per-module and per-keyword basis would help.
* Better context help for common node-keys and modules.

## Features probably needed for a wider release

* More conservative cleanup-on-load settings
* Testing by more people
