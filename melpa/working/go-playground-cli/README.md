# Go Playground Emacs client

[![MELPA](https://melpa.org/packages/go-playground-cli-badge.svg)](https://melpa.org/#/go-playground-cli)

go-playground-cli.el is Go Playground (https://play.golang.org) client tool.
You can compile and run a Go program like `go run prog.go`.


## Requirements

- Emacs 24+


## Install

    $ git clone https://github.com/kosh04/emacs-go-playground.git
    $ edit .emacs
    (add-to-list 'load-path "/path/to/emacs-go-playground/")
    (require 'go-playground-cli)

or [MELPA](https://melpa.org/#/getting-started) package

    M-x package-install -> go-playground-cli


## Command

- `M-x go-playground-cli-run`              : Compile and run selected go program.
- `M-x go-playground-cli-run-current-file` : Compile and run current go program.

If you installed [go-mode](https://github.com/dominikh/go-mode.el), enable call from the menu-bar.

    menu > Go > Playground > Run


## Links

- [Go Playground](http://play.golang.org/)
- [Inside the Go Playground](http://blog.golang.org/playground)


## License

This software is licensed under the MIT-License.
