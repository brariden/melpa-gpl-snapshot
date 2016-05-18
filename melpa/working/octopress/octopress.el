;;; octopress.el --- A lightweight wrapper for Jekyll and Octopress.

;; Copyright (C) 2015 Aaron Bieber

;; Author: Aaron Bieber <aaron@aaronbieber.com>
;; Version: 1.0
;; Package-Requires ((cl-lib "0.5"))
;; Keywords: octopress, blog
;; URL: https://github.com/aaronbieber/octopress.el

;;; Commentary:

;; Octopress.el is a lightweight wrapper script to help you interact
;; with Octopress blog site and the related Jekyll programs. This
;; package is designed to be unobtrusive and to defer to Octopress and
;; Jekyll as often as possible.

;; This package was built with the assumption of Octopress 3.0 and
;; will probably not work with previous (non-gem) versions of
;; Octopress. Specifically, it expects to be able to use commands like
;; `octopress new post` rather than the old-style `rake new_post[]`.

;; Full documentation is available as an Info manual.

;;; Code:

(require 'cl-lib)

(defface octopress-option-on
  '((t (:inherit 'font-lock-string-face)))
  "An Octopress interactive option when on."
  :group 'octopress)

(defface octopress-option-off
  '((t (:inherit 'font-lock-warning-face)))
  "An Octopress interactive option when off."
  :group 'octopress)

(defface octopress-highlight-line-face
  '((((background dark)) :background "#323878")
    (((background light)) :background "#C7CAF2"))
  "Face used to highlight the active line."
  :group 'octopress)

(defvar octopress-highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting the current line.")

(overlay-put octopress-highlight-current-line-overlay
             'face 'octopress-highlight-line-face)

(defvar octopress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'octopress-toggle-command-window)
    (define-key map "q" 'octopress-status-quit)
    (define-key map "s" 'octopress-start-stop-server)
    (define-key map "g" 'octopress-refresh-status)
    (define-key map "c" 'octopress-create-thing)
    (define-key map "d" 'octopress-deploy)
    (define-key map "b" 'octopress-build)
    (define-key map "$" 'octopress-show-server)
    (define-key map "!" 'octopress-show-process)
    (define-key map "n" 'octopress--move-to-next-thing)
    (define-key map "p" 'octopress--move-to-previous-thing)
    (define-key map "P" 'octopress-publish-unpublish)
    (define-key map (kbd "C-n") 'octopress--move-to-next-heading)
    (define-key map (kbd "C-p") 'octopress--move-to-previous-heading)
    (define-key map (kbd "<tab>") 'octopress--maybe-toggle-visibility)
    (define-key map (kbd "<return>") 'octopress--open-at-point)
    map)
  "Get the keymap for the Octopress status buffer.")

(defvar octopress-server-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'octopress-server-quit)
    map)
  "Get the keymap for the Octopress server buffer.")

(defvar octopress-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'octopress-process-quit)
    map)
  "Get the keymap for the Octopress process buffer.")

;;; Customization
(defcustom octopress-posts-directory
  "_posts"
  "Directory containing posts, relative to /path/to/jekyll-site/."
  :type 'string
  :group 'octopress)

(defcustom octopress-drafts-directory
  "_drafts"
  "Directory containing drafts, relative to /path/to/jekyll-site/."
  :type 'string
  :group 'octopress)

(defcustom octopress-default-build-flags
  '()
  "The default flags to pass to `jekyll build'.

Each option is a type of post that is normally excluded from a Jekyll
build.  The checked options will be enabled by default in the
interactive prompt."
  :type    '(set (const :tag "Drafts" drafts)
                 (const :tag "Posts with future dates" future)
                 (const :tag "Unpublished posts" unpublished))
  :group   'octopress)

(defcustom octopress-default-server-flags
  '(drafts unpublished)
  "The default flags to pass to `jekyll serve'.

Each option is a type of post that is normally ignored by the Jekyll
server.  The checked options will be enabled by default in the
interactive prompt to start the server."
  :type    '(set (const :tag "Drafts" drafts)
                 (const :tag "Posts with future dates" future)
                 (const :tag "Unpublished posts" unpublished))
  :group   'octopress)

(defcustom octopress-blog-root
  ""
  "The default location of your Octopress site.

This variable is optional and, if you have more than one Octopress
site, not recommended. If a value is supplied, it will be used as the
location of your Octopress site every time `octopress' is initialized
in a new Emacs session. You will never be prompted for a site location
and the location of any currently open buffer will be ignored."
  :type 'string
  :group 'octopress)

;;; "Public" functions

;;;###autoload
(defun octopress-status ()
  "The main entry point into octopress."
  (interactive)
  (let ((octopress-buffer (octopress--setup)))
    (if octopress-buffer
        (progn (octopress--draw-status octopress-buffer)
               (pop-to-buffer octopress-buffer)))))

(defun octopress-refresh-status ()
  (interactive)
  (octopress-toggle-command-window t)
  (octopress--maybe-redraw-status))

(defun octopress-start-stop-server ()
  (interactive)
  (let* ((config (octopress--read-char-with-toggles
                  "[s] Server, [k] Kill, [q] Abort"
                  '(?s ?k ?q)
                  octopress-default-server-flags))
         (choice (cdr (assoc 'choice config)))
         (drafts (cdr (assoc 'drafts config)))
         (future (cdr (assoc 'future config)))
         (unpublished (cdr (assoc 'unpublished config))))
    (if choice
        (cond ((eq choice ?s)
               (octopress-toggle-command-window t)
               (octopress--start-server-process drafts future unpublished))
              ((eq choice ?k)
               (progn (octopress-toggle-command-window t)
                      (message "Stopping server...")
                      (octopress--stop-server-process)))))))

(defun octopress-restart-server ()
  (interactive))

(defun octopress-show-server ()
  (interactive)
  (octopress-toggle-command-window t)
  (pop-to-buffer (octopress--prepare-server-buffer)))

(defun octopress-show-process ()
  (interactive)
  (octopress-toggle-command-window t)
  (pop-to-buffer (octopress--prepare-process-buffer)))

(defun octopress-create-thing ()
  "Present a menu through which the user may create a new thing."
  (interactive)
  (let ((choice (read-char-choice
                 "[p] Post, [d] Draft, [g] Page, [q] Abort"
                 '(?p ?d ?g ?q))))
    (cond ((eq choice ?p)
           (octopress--new-post))
          ((eq choice ?d)
           (octopress--new-draft))
          ((eq choice ?g)
           (octopress--new-page))
          ((eq choice ?q)
           (message "Aborted.")))))

(defun octopress-deploy ()
  (interactive)
  (when (yes-or-no-p "Really deploy your site? ")
    (progn
      (octopress-toggle-command-window t)
      (octopress--start-deploy-process))))

(defun octopress-build ()
  (interactive)
  (let* ((config (octopress--read-char-with-toggles
                  "[b] Build, [q] Abort"
                  '(?b ?q)
                  octopress-default-build-flags))
         (choice (cdr (assoc 'choice config)))
         (drafts (cdr (assoc 'drafts config)))
         (future (cdr (assoc 'future config)))
         (unpublished (cdr (assoc 'unpublished config))))
    (when (eq choice ?b)
      (progn
        (octopress-toggle-command-window t)
        (octopress--start-build-process drafts future unpublished)))))

(defun octopress-status-quit ()
  "Quit the Octopress status window, preserving its buffer."
  (interactive)
  (octopress-toggle-command-window t)
  (quit-window))

(defun octopress-server-quit ()
  "Quit the Octopress Server window, preserving its buffer."
  (interactive)
  (quit-window))

(defun octopress-process-quit ()
  "Quit the Octopress Process window, preserving its buffer."
  (interactive)
  (quit-window))

(defun octopress-publish-unpublish ()
  (interactive)
  (let* ((thing (octopress--file-near-point))
         (thing-type (car thing))
         (filename (cdr thing)))
    (if (memq thing-type '(drafts posts))
        (octopress--publish-unpublish thing-type filename)
      (message "There is no post nor draft on this line."))))

(defun octopress-insert-post-url ()
  "Prompt for a post and insert a Jekyll URL tag at point.

Assuming that authors typically want to link to newer posts, the
directory list will be sorted in reverse alphabetical order.  Provided
that the files are named using the YYYY-MM-DD prefix format, this will
result in newer posts appearing first in the list."
  (interactive)
  (let* ((post-dir (expand-file-name octopress-posts-directory (octopress--get-root)))
         (posts (sort (directory-files post-dir nil nil t)
                      #'(lambda (s1 s2) (string-lessp s2 s1))))
         (post (file-name-base (completing-read
                                "Link to: "
                                posts
                                '(lambda (f) (and (not (string= f "."))
                                                  (not (string= f ".."))))))))
    (insert (concat "{% post_url " post " %}"))))

(defun octopress--publish-unpublish (type filename)
  "Publish or unpublish a thing based on TYPE, found at FILENAME."
  (let ((source-path (cond ((eq type 'posts)
                            (expand-file-name octopress-posts-directory (octopress--get-root)))
                           ((eq type 'drafts)
                            (expand-file-name octopress-drafts-directory (octopress--get-root)))))
        (subcommand (cond ((eq type 'posts)
                           "unpublish")
                          ((eq type 'drafts)
                           "publish"))))
    (if (file-exists-p (expand-file-name filename source-path))
        (if (or (eq type 'drafts)
                (yes-or-no-p "Really unpublish this post? "))
            (progn (octopress-toggle-command-window t)
                   (octopress--run-octopress-command (concat subcommand " " filename))))
      (message "The file `%s' doesn't exist in `%s'. Try refreshing?" filename octopress-posts-directory))))

(defun octopress-toggle-command-window (&optional hide)
  "Toggle the display of a helpful command window.

If the optional HIDE argument is not nil, hide the command window if
it exists and do nothing otherwise."
  (interactive)
  (let* ((buffer-name (octopress--buffer-name-for-type "command"))
         (command-buffer (get-buffer-create buffer-name))
         (command-window (get-buffer-window command-buffer)))
    (if command-window
        (delete-window command-window)
      (if (not hide)
          (progn
            (octopress--draw-command-help command-buffer)
            (split-window-below)
            (set-window-buffer (next-window) command-buffer)
            (fit-window-to-buffer (next-window)))))))

;;; "Private" functions
(defun octopress--setup ()
  "Stuff that has to happen before anything else can happen."
  ;; Only set up if we have to...
  (let ((octopress-buffer (get-buffer (octopress--buffer-name-for-type "status"))))
    (if (octopress--buffer-is-configured octopress-buffer)
        octopress-buffer
      (setq octopress-root (octopress--get-root))
      (let* ((octopress-buffer (octopress--prepare-status-buffer)))
        (if (and octopress-buffer octopress-root)
            (progn (with-current-buffer octopress-buffer
                     (make-local-variable 'octopress-root))
                   octopress-buffer)
          (progn (kill-buffer octopress-buffer)
                 nil))))))

(defun octopress--draw-command-help (buffer)
  (with-current-buffer buffer
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (octopress--legend-item "C-n" "Next section" 18)
       (octopress--legend-item "C-p" "Prev section" 18)
       (octopress--legend-item "n" "Next thing" 18)
       (octopress--legend-item "p" "Prev thing" 18) "\n"
       (octopress--legend-item "TAB" "Toggle thing" 18)
       (octopress--legend-item "RET" "Open thing" 18) "\n\n"
       (octopress--legend-item "c" "Create" 18)
       (octopress--legend-item "s" "Server" 18)
       (octopress--legend-item "b" "Build" 18)
       (octopress--legend-item "P" "[Un]publish" 18) "\n"
       (octopress--legend-item "d" "Deploy" 18)
       (octopress--legend-item "g" "Refresh" 18)
       (octopress--legend-item "!" "Show Process" 18)
       (octopress--legend-item "$" "Show Server" 18) "\n"
       (octopress--legend-item "q" "Quit" 18))
      (goto-char (point-min)))))

(defun octopress--thing-on-this-line ()
  "Determine whether there is a thing on this line."
  (get-text-property (line-beginning-position) 'thing))

(defun octopress--file-near-point ()
  "Return the filename on the current line (of *octopress-status*).

Return a single cons cell where the car of the cons is the `thing
type', e.g. 'drafts or 'posts, and the cdr of the cons is the filename.

If the current line of the current buffer does not have a valid thing type, this
function returns nil."
  (let* ((thing-type (get-text-property (line-beginning-position) 'invisible))
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (found (string-match "^\s*\\([^ ]*\\)" line))
         (filename (match-string 1 line)))
    (if (and thing-type found filename)
        (cons thing-type (octopress--strip-text-properties filename))
      nil)))

(defun octopress--read-char-with-toggles (prompt-suffix choices &optional default-to-on)
  "Read a selection from a menu with toggles.

Display a fixed menu of toggles followed by PROMPT-SUFFIX.  Accept any of
the default choices (d, f, u, q) as well as the supplied CHOICES, which
should be provided as a list of characters (not strings).

If any of the symbols `drafts', `future', or `unpublished' are present in
the DEFAULT-TO-ON list, those toggles will be turned on initially.

This function returns the char value from CHOICES selected by the user."
  (let ((choices (append choices '(?d ?f ?u ?q)))
        (drafts (memq 'drafts default-to-on))
        (future (memq 'future default-to-on))
        (unpublished (memq 'unpublished default-to-on))
        return done)
    (while (not done)
      (let* ((prompt (concat (propertize "(" 'face 'default)
                             (propertize "[d]rafts " 'face (if drafts 'octopress-option-on 'octopress-option-off))
                             (propertize "[f]uture " 'face (if future 'octopress-option-on 'octopress-option-off))
                             (propertize "[u]npublished" 'face (if unpublished 'octopress-option-on 'octopress-option-off))
                             ") " prompt-suffix))
             (choice (read-char-choice prompt choices)))
        (cond ((eq choice ?d)
               (setq drafts (not drafts)
                     done nil))
              ((eq choice ?f)
               (setq future (not future)
                     done nil))
              ((eq choice ?u)
               (setq unpublished (not unpublished)
                     done nil))
              ((eq choice ?q)
               (setq done t)
               (message "Aborted."))
              (t (setq return `((choice . ,choice)
                                (drafts . ,drafts)
                                (future . ,future)
                                (unpublished . ,unpublished))
                       done t)))))
  return))

(defun octopress--get-line-type ()
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'invisible)))

(defun octopress--get-line-filename ()
  (save-excursion
    (back-to-indentation)
    (thing-at-point 'filename)))

(defun octopress--expand-path-for-type (filename type)
  (let ((type-dir (cdr (assoc type `((posts . ,octopress-posts-directory)
                                     (drafts . ,octopress-drafts-directory))))))
    (and filename
         type-dir
         (expand-file-name
          filename (expand-file-name
                    type-dir (octopress--get-root))))))

(defun octopress--open-at-point ()
  "Open the file at point, if there is one."
  (interactive)
  (let* ((type (octopress--get-line-type))
         (filename (octopress--get-line-filename))
         (full-filename (octopress--expand-path-for-type filename type)))
    (if (and type
             (file-exists-p full-filename))
        (pop-to-buffer (find-file full-filename)))))

(defun octopress--new-post ()
  (octopress-toggle-command-window t)
  (let ((name (read-string "Post name: ")))
    (octopress--run-octopress-command (concat "new post \"" name "\""))))

(defun octopress--new-draft ()
  (octopress-toggle-command-window t)
  (let ((name (read-string "Draft name: ")))
    (octopress--run-octopress-command (concat "new draft \"" name "\""))))

(defun octopress--new-page ()
  (octopress-toggle-command-window t)
  (let ((name (read-string "Page name: ")))
    (octopress--run-octopress-command (concat "new page \"" name "\""))))

(defun octopress--buffer-is-configured (buffer)
  "Return t if BUFFER is configured properly for Octopress."
  (and (bufferp buffer)
       (let ((vars (buffer-local-variables
                    (get-buffer buffer))))
         (and (assoc 'octopress-root vars)
              (string= (cdr (assoc 'major-mode vars)) "octopress-mode")))))

(defun octopress--start-deploy-process ()
  (octopress--setup)
  (octopress--run-octopress-command "deploy"))

(defun octopress--start-build-process (&optional with-drafts with-future with-unpublished)
  (octopress--setup)
  (let* ((process-buffer (octopress--prepare-process-buffer))
         (drafts-opt (if with-drafts " --drafts" nil))
         (future-opt (if with-future " --future" nil))
         (unpublished-opt (if with-unpublished " --unpublished" nil))
         (root (octopress--get-root))
         (command (concat "build" drafts-opt future-opt unpublished-opt)))
    (octopress--run-jekyll-command command)))

(defun octopress--start-server-process (&optional with-drafts with-future with-unpublished)
  (octopress--setup)
  (let* ((buffer (octopress--prepare-server-buffer))
         (drafts-opt (if with-drafts " --drafts" nil))
         (future-opt (if with-future " --future" nil))
         (unpublished-opt (if with-unpublished " --unpublished" nil))
         (command (concat (octopress--bundler-command-prefix)
                           "jekyll serve" drafts-opt future-opt unpublished-opt)))
    (if (processp (get-buffer-process (octopress--buffer-name-for-type "server")))
        (message "Server already running!")
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "Running `%s'...\n\n" command) 'face 'font-lock-variable-name-face))))
      (let ((process
            (start-process-shell-command
             "octopress-server"
             buffer
             (concat "cd " (octopress--get-root) " && " command))))
      (message "Server started!")
      (set-process-sentinel process 'octopress--server-sentinel)
      (set-process-filter process 'octopress--generic-process-filter))
      (octopress--maybe-redraw-status))))

(defun octopress--stop-server-process ()
  (let ((server-process (get-buffer-process (octopress--buffer-name-for-type "server"))))
    (if (processp server-process)
        (process-send-string server-process (kbd "C-c")))))

(defun octopress--buffer-name-for-type (type)
  "Return a buffer name for the provided TYPE."
  (concat "*octopress-" type "*"))

(defun octopress--server-sentinel (process event)
  (octopress--maybe-redraw-status)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event)))
    (cond ((string-prefix-p "finished" event)
           (progn (message "Jekyll server has finished.")
                  (with-current-buffer (octopress--prepare-server-buffer)
                    (goto-char (process-mark process))
                    (let ((inhibit-read-only t))
                      (insert (propertize "\nJekyll server has finished.\n\n" 'face 'font-lock-warning-face))
                      (goto-char (point-max)))))))))

(defun octopress--server-status ()
  (let ((server-process (get-buffer-process (octopress--buffer-name-for-type "server"))))
    (and (processp server-process)
         (string= (process-status server-process) "run"))))

(defun octopress--server-status-string ()
  (if (octopress--server-status)
      "Running"
    "Stopped"))

(defun octopress--prepare-buffer-for-type (type &optional mode-function)
  "Prepare an empty buffer for TYPE and optionally run MODE-FUNCTION."
  (let ((buffer-name (octopress--buffer-name-for-type type)))
    (if (bufferp buffer-name)
        (get-buffer buffer-name)
      (let ((buf (get-buffer-create buffer-name)))
        (with-current-buffer buf
          (setq buffer-read-only t)
          (kill-all-local-variables)
          (if (functionp mode-function)
              (funcall mode-function)))
        buf))))

(defun octopress--prepare-status-buffer ()
  "Return the Octopress (\"status\") buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (let ((buffer-name (octopress--buffer-name-for-type "status")))
    (if (get-buffer buffer-name)
        (get-buffer buffer-name)
      (let ((status-buffer (octopress--prepare-buffer-for-type "status" 'octopress-mode)))
        (with-current-buffer status-buffer
          (add-to-invisibility-spec 'posts))
        status-buffer))))

(defun octopress--prepare-server-buffer ()
  "Return the Octopress Server buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (octopress--prepare-buffer-for-type "server" 'octopress-server-mode))

(defun octopress--prepare-process-buffer ()
  "Return the Octopress Process buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (octopress--prepare-buffer-for-type "process" 'octopress-process-mode))

(defun octopress--get-root ()
  "Maybe return the root of the Octopress site.

If `octopress-blog-root' has a value, it is assumed to be the correct
blog root.

Otherwise, we assume we are running from a buffer editing a file
somewhere within the site.  If we are running from some other kind of
buffer, or a buffer with no file, the user will be prompted to enter
the path to an Octopress site."
  (if (not (string= "" octopress-blog-root))
      octopress-blog-root
    (let ((status-buffer (get-buffer (octopress--buffer-name-for-type "status")))
          (this-dir (if (and (boundp 'dired-directory) dired-directory)
                        dired-directory
                      (if (buffer-file-name (current-buffer))
                          (file-name-directory (buffer-file-name (current-buffer)))))))
      (if (and (bufferp status-buffer)
               (assoc 'octopress-root (buffer-local-variables status-buffer))
               (buffer-local-value 'octopress-root status-buffer))
          (buffer-local-value 'octopress-root status-buffer)
        (or (and this-dir
                 (let ((candidate-dir (vc-find-root this-dir "_config.yml")))
                   (if candidate-dir (expand-file-name candidate-dir) nil)))
            (let ((candidate-dir (read-directory-name "Octopress site root: ")))
              (if (file-exists-p (expand-file-name "_config.yml" candidate-dir))
                  (expand-file-name candidate-dir)
                (prog2 (message "Could not find _config.yml in `%s'." candidate-dir)
                    nil))))))))

(defun octopress--maybe-redraw-status ()
  "If the status buffer exists, redraw it with current information."
  (let ((status-buffer (get-buffer (octopress--buffer-name-for-type "status"))))
    (if (bufferp status-buffer)
        (octopress--draw-status status-buffer))))

(defun octopress--get-status-data (buffer)
  "Return the status of the Octopress site linked to BUFFER.

This function can only be called after `octopress-status' has been run and must be
passed the resulting BUFFER."
  (octopress--setup)
  (with-current-buffer buffer
    `((posts-count . ,(number-to-string
                       (length
                        (directory-files
                         (expand-file-name octopress-posts-directory (octopress--get-root))
                         nil
                         "*.md$\\|.*markdown$"))))
      (drafts-count . ,(number-to-string
                        (length
                         (directory-files
                          (expand-file-name octopress-drafts-directory (octopress--get-root))
                          nil
                          ".*md$\\|.*markdown$"))))
      (server-status . ,(octopress--server-status-string)))))

(defun octopress--move-to-next-thing ()
  "Move point to the next item with property 'thing."
  (interactive)
  (octopress--move-to-next-visible-thing))

(defun octopress--move-to-next-heading ()
  "Move point to the next item with property 'heading."
  (interactive)
  (octopress--move-to-next-prop 'heading))

(defun octopress--move-to-next-visible-thing (&optional reverse)
  "Move point to the next item with property 'thing that is visible.

If REVERSE is not nil, move to the previous visible 'thing."
  (goto-char (or (let ((start (point)))
                   (if reverse
                       (beginning-of-line)
                     (end-of-line))
                   (let* (destination)
                     (while (not destination)
                       (let ((next-candidate (if reverse
                                                 (previous-single-property-change (point) 'thing)
                                               (next-single-property-change (point) 'thing))))
                         (if next-candidate
                             (if (memq (get-text-property next-candidate 'invisible)
                                       buffer-invisibility-spec)
                                 (goto-char next-candidate)
                               (setq destination next-candidate))
                           (setq destination start))))
                     destination))
                 (point)))
  (beginning-of-line))

(defun octopress--move-to-next-prop (prop-name)
  "Move to the next item with property PROP-NAME."
  (goto-char
   (or (save-excursion
         (goto-char (line-end-position))
         (let ((thing (next-single-property-change (point) prop-name)))
           (if thing
               (let ((type (get-text-property thing 'invisible)))
                 (if (and type (memq type buffer-invisibility-spec))
                     (remove-from-invisibility-spec type))
                 thing))))
       (point))))

(defun octopress--move-to-previous-thing ()
  "Move to the previous item with property 'thing."
  (interactive)
  (octopress--move-to-next-visible-thing t))

(defun octopress--move-to-previous-heading ()
  "Move to the previous item with property 'heading."
  (interactive)
  (octopress--move-to-previous-prop 'heading))

(defun octopress--move-to-previous-prop (prop-name)
  "Move to the previous item with property PROP-NAME."
  (goto-char
   (or (save-excursion
         (goto-char (line-beginning-position))
         (let ((thing (previous-single-property-change (point) prop-name)))
           (if thing
               (let ((type (get-text-property thing 'invisible)))
                 (if (or (not type)
                         (not (memq type buffer-invisibility-spec)))
                     thing
                   nil)))))
       (point)))
  (goto-char (line-beginning-position)))

(defun octopress--maybe-toggle-visibility ()
  (interactive)
  (let ((hidden (get-text-property (line-beginning-position) 'hidden)))
    (if hidden
        (if (memq hidden buffer-invisibility-spec)
            (remove-from-invisibility-spec hidden)
          (add-to-invisibility-spec hidden))))
  (force-window-update (current-buffer)))

(defun octopress--draw-status (buffer)
  "Draw a display of STATUS in BUFFER.

STATUS is an alist of status names and their printable values."
  (let ((status (octopress--get-status-data buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (window (get-buffer-window))
            (pos (point)))
        (erase-buffer)
        (insert
         (propertize "Octopress Status\n" 'face '(:inherit font-lock-constant-face :height 160))
         "\n"
         (propertize " " 'thing t 'heading t)
         (propertize "   Blog root: " 'face 'font-lock-function-name-face)
         octopress-root "\n"

         (propertize " " 'thing t 'heading t)
         (propertize "      Server: " 'face 'font-lock-function-name-face)
         (cdr (assoc 'server-status status)) "\n"

         (propertize " " 'thing t 'hidden 'drafts 'heading t)
         (propertize "      Drafts: " 'face 'font-lock-function-name-face)
         (cdr (assoc 'drafts-count status)) "\n"
         (octopress--get-display-list (octopress--get-drafts) 'drafts)

         (propertize " " 'thing t 'hidden 'posts 'heading t)
         (propertize "       Posts: " 'face 'font-lock-function-name-face)
         (cdr (assoc 'posts-count status)) "\n"
         (octopress--get-display-list (octopress--get-posts) 'posts)

         "\n"
         "Press `?' for help.")
        (goto-char (if (< pos (point-max))
                       pos
                     (point-min)))
        (if window
            (force-window-update window))))))

(defun octopress--get-display-list (things visibility-name)
  (let ((thing-list ""))
    (cl-loop for thing in things do
             (setq thing-list
                   (concat thing-list
                           (propertize " " 'thing t)
                           (make-string 10 ? ) thing "\n")))
    (propertize thing-list 'invisible visibility-name)))

(defun octopress--legend-item (key label column-width)
  (let ((pad (- column-width (+ (length key) (length label) 2))))
    (concat
     (propertize key 'face 'font-lock-keyword-face) ": "
     label
     (make-string pad ? ))))

(defun octopress--get-articles-in-dir-by-date-desc (dir)
  "Get files in the blog subdir DIR in descending order by date."
  (mapcar #'car
          (sort (directory-files-and-attributes
                 (expand-file-name dir octopress-root)
                 nil
                 "*.md$\\|.*markdown$")
                #'(lambda (f1 f2) (time-less-p (nth 6 f2) (nth 6 f1))))))

(defun octopress--get-posts ()
  "Get a list of posts files."
  (octopress--setup)
  (octopress--get-articles-in-dir-by-date-desc octopress-posts-directory))

(defun octopress--get-drafts ()
  "Get a list of drafts files."
  (octopress--setup)
  (octopress--get-articles-in-dir-by-date-desc octopress-drafts-directory))

(defun octopress--bundler-command-prefix ()
  "Return a `bundle exec' command prefix if required.

If a Gemfile is found in the root of the blog, we'll assume Bundler is
being used and we need to run Octopress with `bundle exec'."
  (if (file-exists-p (expand-file-name "Gemfile" (octopress--get-root)))
      "bundle exec "
    ""))

(defun octopress--run-octopress-command (command)
  "Run an Octopress COMMAND."
  (message "Running Octopress...")
  (octopress--run-command (concat "octopress " command)))

(defun octopress--run-jekyll-command (command)
  "Run a Jekyll COMMAND."
  (message "Running Jekyll...")
  (octopress--run-command (concat "jekyll " command)))

(defun octopress--run-command (command)
  "Run an Octopress-related COMMAND, sending output to the process buffer.

Returns the process object."
  (octopress--setup)
  (let ((pbuffer (octopress--prepare-process-buffer))
        (root (octopress--get-root))
        (command (concat
                  (octopress--bundler-command-prefix)
                  (replace-regexp-in-string "'" "\\\\'" command))))
    (with-current-buffer pbuffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (concat "Running `" command "'...\n\n") 'face 'font-lock-variable-name-face))))
    (let ((process (start-process-shell-command
                    "octopress"
                    pbuffer
                    (concat "cd " root " && " command))))
      (set-process-sentinel process 'octopress--octopress-sentinel)
      (set-process-filter process 'octopress--generic-process-filter)
      process)))

(defun octopress--octopress-sentinel (process event)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event))
        (buffer (get-buffer (octopress--buffer-name-for-type "process"))))
    (cond ((string-prefix-p "finished" event)
           (progn (octopress--handle-octopress-output buffer)
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (insert (concat (propertize (make-string 80 ?-) 'face 'font-lock-comment-face) "\n\n"))
                      (set-marker (process-mark process) (point))))
                  (message "Octopress has completed.")
                  (octopress--maybe-redraw-status)))
          ((string-prefix-p "exited" event)
           (message "Octopress exited abnormally; check the process output for information.")
           (octopress--handle-octopress-output buffer)))))

(defun octopress--generic-process-filter (proc string)
  "Filter PROC output of STRING and manipulate the buffer."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (window (get-buffer-window))
            (inhibit-read-only t))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (replace-regexp-in-string "" "" string))
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc))
          (if window
              (with-selected-window window
                (goto-char (process-mark proc)))))))))

(defun octopress--handle-octopress-output (buffer)
  "Attempt to do something reasonable based on output in BUFFER.

This is 'cheater mode' for not having callbacks in elisp and to avoid creating
different output buffers for different operations to figure out what to do with
each kind of output."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "^[A-Z]" (point-min) t)
      (let ((output (buffer-substring (line-beginning-position) (line-end-position))))
        (cond ((or (string-prefix-p "New post:" output)
                   (string-prefix-p "New draft:" output)
                   (string-prefix-p "New page:" output))
               (let* ((filename (octopress--find-filename-in-output output)))
                 (if (file-exists-p filename)
                     (find-file filename))))
               ((string-prefix-p "Published:" output)
                (let ((draft (octopress--find-filename-in-output output "_drafts"))
                      (post (octopress--find-filename-in-output output "_posts")))
                  (if (and draft post)
                      (octopress--swap-window-files draft post))))
               ((string-prefix-p "Unpublished:" output)
                (let ((draft (octopress--find-filename-in-output output "_drafts"))
                      (post (octopress--find-filename-in-output output "_posts")))
                  (if (and draft post)
                      (octopress--swap-window-files post draft)))))))))

(defun octopress--swap-window-files (old-filename new-filename)
  "Swap any windows displaying OLD-FILENAME to instead display NEW-FILENAME.

This function creates a buffer for NEW-FILENAME if one does not
already exist, finds any windows currently displaying a buffer
corresponding to OLD-FILENAME, and changes them to instead edit the
NEW-FILENAME buffer.  Any buffer visiting OLD-FILENAME is then killed.
This function is called when posts or drafts move between published
and unpublished status."
  (let* ((new-buffer (find-file-noselect new-filename))
         (old-buffer (find-buffer-visiting old-filename))
         (window-visiting-old-file (get-buffer-window old-buffer)))
    (while window-visiting-old-file
      (progn (set-window-buffer window-visiting-old-file new-buffer)
             (setq window-visiting-old-file (get-buffer-window old-buffer))))
    (if old-buffer
        (kill-buffer old-buffer))))

(defun octopress--find-filename-in-output (output &optional prefix)
  "Find the filename in an Octopress OUTPUT line.

This helper function will extract a filename with preceding path
components, if present, from a single line of Octopress output.  Used
by `octopress--handle-octopress-output'.

If the string PREFIX is given, the filename is assumed to begin with
it.  For example, call with '_posts' or '_drafts' to find the
corresponding paths in the output line."
  (let* ((found (if prefix (string-match (concat "\\(" prefix "[^\s]*\\)") output)
                  (string-match ": \\([^\s]*\\)$" output)))
         (filename (and found
                        (expand-file-name (match-string 1 output) (octopress--get-root)))))
    filename))

(defun octopress--prop-command (key label)
  "Propertize a command legend item with pretty colors.

Return a propertized string like KEY: LABEL."
  (concat (propertize key 'face 'font-lock-keyword-face) ": " label))

(defun octopress--strip-text-properties(text)
  "Remove all properties from TEXT and return it."
  (set-text-properties 0 (length text) nil text)
      text)

(defun octopress--highlight-current-line ()
  (if (octopress--thing-on-this-line)
      (let ((end (save-excursion
                   (forward-line 1)
                   (point))))
        (move-overlay octopress-highlight-current-line-overlay (line-beginning-position) end))
    (delete-overlay octopress-highlight-current-line-overlay)))

(define-derived-mode octopress-mode nil "Octopress"
  "The major mode for interacting with a Jekyll site.

The following keys are available in `octopress-mode':

  \\{octopress-mode-map}"
  (setq truncate-lines t)
  (add-hook 'post-command-hook 'octopress--highlight-current-line nil t))

(define-derived-mode octopress-server-mode nil "Octopress[Server]"
  "The major mode for interacting with a Jekyll server process.

The following keys are available in `octopress-server-mode':

  \\{octopress-server-mode-map}"
  (setq truncate-lines t))

(define-derived-mode octopress-process-mode nil "Octopress[Process]"
  "The major mode for interacting with Octopress and Jekyll shell commands.

The following keys are available in `octopress-process-mode':

  \\{octopress-server-mode-map}"
  (setq truncate-lines t))

(provide 'octopress)

;;; octopress.el ends here
