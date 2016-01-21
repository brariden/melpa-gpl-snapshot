# org-mobile-sync

Automatically push and pull changes to/from [MobileOrg](http://orgmode.org/manual/MobileOrg.html).

Some of the code has been taken from the [MobileOrg FAQ](https://github.com/matburt/mobileorg-android/wiki/FAQ).

I added support for inotify so that changes of the `org-mobile-capture-file` are detected immediately and trigger a `org-mobile-pull`.

**Emacs 24.3.50 with `file-notify-support` is required for the mode to work.**

## Installation

### quelpa

`quelpa` is at https://github.com/quelpa/quelpa

```lisp
(quelpa '(org-mobile-sync :repo "steckerhalter/org-mobile-sync" :fetcher github))
;; since the recipe is stored in melpa this also works:
(quelpa 'org-mobile-sync)
```
Interactively: `M-x quelpa org-mobile-sync RET`.

### el-get

```lisp
(:name org-mobile-sync
       :type git
       :url "https://github.com/steckerhalter/org-mobile-sync")
```

### MELPA

`org-mobile-sync` is packaged in [MELPA](http://melpa.milkbox.net/) and can be installed via the Emacs package manager:

`M-x package-list-packages`, select `org-mobile-sync` from the list by pressing `i`, then press `x` to execute the changes. At that point, the package will be installed.

## Usage

Either enable `org-mobile-sync-mode` manually by executing `M-x org-mobile-sync-mode` (turn it off again by executing it a second time)

or put something like this into your Emacs init file:

    (require 'org-mobile-sync)
    (org-mobile-sync-mode 1)
