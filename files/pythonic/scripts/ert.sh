#!/bin/bash -e

function info() {
    echo -ne '\e[01;34m'"Emacs version: "'\e[0m'
    echo $emacs
}

for emacs in emacs-24.3 emacs-24.4 emacs-24.5
do
    info
    EMACS=$emacs cask exec ert-runner
done
