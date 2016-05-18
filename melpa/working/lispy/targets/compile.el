(setq files '("lispy.el"
              "lispy-inline.el"
              "le-clojure.el"
              "le-scheme.el"
              "le-lisp.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)
