# helm-ag-r

This is program to search something by [ag](https://github.com/ggreer/the_silver_searcher) and display by helm recursively.

### Requirements
You need to install **ag** and **helm** to use this package.

### Installation
You can install MELPA by M-x package-install command.

If you use el-get, you set below configuration to your .emacs.

```lisp
(push '(:name helm-ag-r
        :type git
        :url "https://github.com/yuutayamada/helm-ag-r.git"
        :depends (helm)
        :load-path "./")
        el-get-sources)
```

And then you can install this package by M-x el-get-install helm-ag-r RET.

### Configuration

```lisp
(require 'helm-ag-r)
;; Specify your favorite ag's configuration
;; You can change below option by pushing C-o on helm-ag-r's minibuffer.
(setq helm-ag-r-option-list '("-S -U --hidden" "-S -U -g")
      ;; start searching from 3 character
      helm-ag-r-requires-pattern 3
      ;; delay searching from input
      helm-ag-r-input-idle-delay 0.5)
```

If you think that this program too slow or unstable.
Try following configuration.
(You can change numbers)

```lisp
(setq ;; You need to input characters for this times to search
      helm-ag-r-requires-pattern 3   ; <- this is default
      ;; Delay input
      helm-ag-r-input-idle-delay 0.5 ; <- this is default
      ;; Display candidate only you specified number
      helm-ag-r-candidate-limit 100  ; <- default is helm-candidate-number-limit
      ;; Don't highlight machining words
      helm-ag-r-use-no-highlight t)  : <- default is nil(t means no highlight)
```

### Commands
You can use below commands:
* helm-ag-r-current-file -- search from current file
* helm-ag-r-from-git-repo -- search from git repository
* helm-ag-r-shell-history -- search shell history
* helm-ag-r-git-logs -- search git logs

### Customize
You can create your original function. For example:

    (defun helm-ag-r-shell-history ()
      "Search shell history(I don't make sure without zsh)"
      (interactive)
      (helm-ag-r-pype
       "tac ~/.zsh_history | sed 's/^: [0-9]*:[0-9];//'"
       '((action . (lambda (line)
                     (case major-mode
                       (term-mode (term-send-raw-string line))
                       (t (insert line))))))))

Above example search history file for zsh. As you can see, you can specify shell command and then search by ag and display the result by helm.

### License
GPL3
