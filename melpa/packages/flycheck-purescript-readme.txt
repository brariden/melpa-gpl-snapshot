Currently `flycheck-purescript' compiles the project sources and writes the
output in a temporal directory, which could be resource intensive, because it
does each time FlyCheck executes.  Is **recommended** to set the default
output to a directory:

    (setq-default flycheck-purescript-compile-output-dir "output")

Which will write the output to a "output" directory, relative to project root
directory.

## Setup

    (eval-after-load 'flycheck
      '(flycheck-purescript-setup))
