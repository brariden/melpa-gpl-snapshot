# multi-compile
Multi target interface to compile.

## Screenshot

![multi-target](https://cloud.githubusercontent.com/assets/1151286/10209424/de607546-67e3-11e5-8cb0-f50d390e823b.png)

## Installation

You can install `multi-compile.el` from [MELPA](https://melpa.org/) with `package.el`

```
 M-x package-install multi-compile
```

## Sample Configuration

```lisp
(require 'multi-compile)

(setq multi-compile-alist '(
    (rust-mode . (("rust-debug" . "cargo run")
                  ("rust-release" . "cargo run --release")
                  ("rust-test" . "cargo test")))
    ))
;;(setq multi-compile-completion-system 'ido)
(setq multi-compile-completion-system 'helm)
;; (setq multi-compile-completion-system 'default)
```

In a compilation commands, you can use the templates:

- "%path" - "/tmp/prj/file.rs"
- "%dir" - "/tmp/prj/"
- "%file-name" - "file.rs"
- "%file-sans" - "file"
- "%file-ext" - "rs"
- "%make-dir" - (Look up the directory hierarchy from current file for a directory containing "Makefile") - "/tmp/"

for example:

```lisp
(setq multi-compile-alist '(
    (c++-mode . (("cpp-run" . "make --no-print-directory -C %make-dir")))
    (rust-mode . (("rust-debug" . "cargo run")
                  ("rust-release" . "cargo run --release")
                  ("rust-test" . "cargo test")))
    ))
```
You can use filename pattern:

```lisp
(setq multi-compile-alist '(
    ("\\.txt\\'" . (("text-filename" . "echo %file-name")))
```

## Usage

- Open *.rs file
- M-x multi-compile-run

## Links

[In Russian](http://reangdblog.blogspot.com/2015/10/emacs-multi-compile.html)
