;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I am in buffer \"\\([^\"]+\\)\" in \\(.+\\)$"
  (lambda (buffer mode)
    (switch-to-buffer buffer)
    (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
      (execute-kbd-macro v))))

(And "^I am visiting the project file \"\\([^\"]+\\)\" in \\(.+\\)$"
  (lambda (fname mode)
    (find-file (concat project-dir fname))
    (save-buffer)
    (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
      (execute-kbd-macro v))))

(And "^I close all the other windows$"
  (lambda ()
    (delete-other-windows)))

(When "^I press close the current window$"
  (lambda ()
    (delete-window)))

(And "^I run \\(.+\\)$"
  (lambda (f)
    (funcall (intern f))))

(And "^I turn off \\(.+\\)$"
  (lambda (mode)
    (funcall (intern mode) '0)))

(And "^the frame should be \\([a-z]+\\)\\( aligned\\)?"
  (lambda (alignment _)
    (assert (equal frame-alignment alignment) nil
            "Frame should be %s aligned but is %s aligned"
            alignment frame-alignment)))

(And "^the \\([0-9]+\\)\\(st\\|nd\\|rd\\|th\\) window should be in buffer \"\\([^\"]+\\)\"$"
  (lambda (n _ expected-name)
    (save-window-excursion
      (select-window (window-at 1 1))
      (other-window (- (string-to-number n) 1))
      (assert (equal expected-name (buffer-name)) nil
              "Window #%s should be in buffer %s but is in buffer %s"
              n expected-name (buffer-name)))))

(Then "^there should be \\([0-9]+\\) windows?$"
  (lambda (wins)
    (assert (= (string-to-number wins)
               (length (window-list))) nil
               "There are %s windows when there should be %s"
               (length (window-list)) wins)))

(And "^I kill the current buffer$"
  (lambda ()
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (current-buffer)))))

(And "^I rename the buffer to \"\\([^\"]+\\)\"$"
  (lambda (name)
    (rename-buffer name)))

(When "^I am in the project \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (setq project-dir (concat base-dir "/" project-name "/"))
    (make-directory project-dir)))

(And "^I switch to the next window$"
  (lambda ()
    (other-window 1)))

(And "^I split the window vertically$"
  (lambda ()
    (split-window-vertically)))

(And "^I split the window horizontally$"
  (lambda ()
    (split-window-horizontally)))

(And "^the current directory should be the base directory$"
  (lambda ()
    (assert (string-equal default-directory project-dir) nil
            "Current directory should equal %s but instead is %s"
            project-dir default-directory)))

(And "^I save the buffer$"
  (lambda ()
    (save-buffer)))

(And "^I create the directory \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (make-directory (concat project-dir dirname))))

(Then "^I should be in \\([-a-z]+-mode\\)$"
  (lambda (mode-string)
    (assert (equal major-mode (intern mode-string)) nil
            "Mode should be %s but is %s" major-mode mode-string)))

(And "^the value of \\([-a-z]+\\) should be \"\\([^\"]+\\)\"$"
  (lambda (symbol-name val)
    (let ((symbol-val (symbol-value (intern symbol-name))))
      (assert (equal symbol-val val) nil
              "Symbol %s should equal %s but is %s"
              symbol-name val symbol-val))))

(Then "^the numeric value of \\([-a-z]+\\) should be \\([0-9]+\\)$"
  (lambda (symbol-name val-str)
    (let ((symbol-val (symbol-value (intern symbol-name)))
          (val (string-to-number val-str)))
      (assert (equal symbol-val val) nil
              "Symbol %s should equal %s but is %s"
              symbol-name val symbol-val))))

(And "^I insert some text$"
       (lambda ()
         (Lorem-ipsum-insert-paragraphs 5)))

(And "^I go to the end of the buffer$"
       (lambda ()
         (end-of-buffer)))

(Then "^the point should be at the end$"
  (lambda ()
    (assert (equal (point) (point-max)) nil
            "Point should be at %s but is at %s"
            (point-max) (point))))
