# flycheck-protobuf
A protobuf syntax checker based on protoc compiler

[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/flycheck-protobuf-badge.svg)](http://melpa.org/#/flycheck-protobuf)
[![Build Status](https://travis-ci.org/edvorg/flycheck-protobuf.svg)](https://travis-ci.org/edvorg/flycheck-protobuf)

Usage

```lisp
(eval-after-load 'flycheck
  '(require 'flycheck-protobuf))
(add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t)
```

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
