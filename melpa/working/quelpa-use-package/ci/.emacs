;; bootstrap quelpa
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; install use-package
(quelpa '(use-package :fetcher github :repo "jwiegley/use-package" :files ("use-package.el")))

;; require the library
(add-to-list 'load-path travis-ci-dir)
(require 'quelpa-use-package)

;; test some installs
(use-package grandshell-theme :quelpa)
(use-package ipretty :quelpa t)
(use-package flx-ido :quelpa (:stable t))
(use-package flx-ido :quelpa ((flx-ido) :upgrade t))
(use-package git-modes :quelpa (git-modes :fetcher github :repo "magit/git-modes"))
(use-package git-timemachine :quelpa ((git-timemachine :fetcher github :repo "pidu/git-timemachine") :stable t))
(use-package git-timemachine :quelpa ((git-timemachine :fetcher github :repo "pidu/git-timemachine") :upgrade t))

;; test advice
(setq use-package-always-ensure t)
(quelpa-use-package-activate-advice)
(use-package kite-mini
  :quelpa (kite-mini :fetcher github :repo "tungd/kite-mini.el"))

;; show us the cache
(require 'pp)
(pp quelpa-cache)
