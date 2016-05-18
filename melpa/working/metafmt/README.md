# metafmt

`metafmt` is an opinionated front-end for various code beautifiers. It is meant to be used from
the command line or integrated into an editor.

It's opinionated, which means that you can't configure it, *this is by design*.


## Installation

`metafmt` is written in Go. Since there are no pre-built binaries (yet) you should install it by
running:

    go get github.com/lvillani/metafmt


## Usage

When used from the command line, `metafmt` will try to format the files given as
arguments. When given a directory, `metafmt` will beautify all files recursively.

Beautified code is printed on standard output. By passing the `-write` flag you can force
`metafmt` to format files in-place instead.

Formatters are chosen based on the file's extension. Files without extension are skipped.


## Editor Integration

### Emacs

[metafmt.el](metafmt.el) contains a minor mode that you can enable per-buffer or globally. To
integrate `metafmt` with Emacs, copy said file somewhere, load it and then enable either
`metafmt-mode` or `global-metafmt-mode`:

```elisp
(load "~/.emacs.d/metafmt.el")
(global-metafmt-mode t)
```


## Supported Formatters

**NOTE**: These have to be installed separately. If one of them isn't installed, `metafmt` will
skip the file and do nothing.

* C/C++: [clang-format](http://clang.llvm.org/docs/ClangFormat.html);
* CSS: [cssfmt](https://github.com/morishitter/cssfmt);
* Go: [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports);
* JavaScript: [semistandard-format](https://github.com/ricardofbarros/semistandard-format);
* JSON: [jsonlint](https://github.com/zaach/jsonlint);
* Python:
  - [autopep8](https://github.com/hhatto/autopep8);
  - [isort](https://github.com/timothycrosley/isort);
* SASS/SCSS: [ruby-sass](http://sass-lang.com/install);
