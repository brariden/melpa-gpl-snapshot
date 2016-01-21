:; exec emacs -Q --script "$0" -- "$@"

;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

;;; Code

(when noninteractive
  ;; set load path to . and ..
  (setq standoff-lib-dir (concat (file-name-directory load-file-name) "/.."))
  (push standoff-lib-dir load-path)
  (push (file-name-directory load-file-name) load-path)
  ;; pop "--" from argv
  (setq argv (cdr argv))
  )

(require 'ert)
(require 'standoff-test-utils)
(require 'standoff-dummy)

;;; Functions for set up and tear down

(defun standoff-dummy-test-setup-source-buffer ()
  "Save the current configuration and setup testing environment.
Returns a test buffer with some content."
  (let ((test-buffer (generate-new-buffer "dummy-test")))
    (standoff-test-utils-save-old-config)
    ;; create and fill test buffer
    (set-buffer test-buffer)
    (insert "Was kann das Licht in unſer Auge mahlen?
Was ſich mahlen laͤßt, Bilder. Wie auf der
weißen Wand der dunklen Kammer, ſo faͤllt auf
die Netzhaut des Auges ein Stralenpinſel von
allem, was vor ihm ſtehet, und kann nichts, als
was […] was da ſteht, eine Flaͤche, ein Nebeneinander
aller und der verſchiedenſten ſichtbaren Gegenſtaͤnde
zeichnen. Dinge hinter einander, oder ſolide,
maſſive Dinge als ſolche dem Auge zu geben, iſt
ſo unmoͤglich, als den Liebhaber hinter der dicken
Tapete, den Bauer innerhalb der Windmuͤhle
ſingend zu mahlen.")
    ;; Return test buffer
    test-buffer))

(defun standoff-dummy-test-teardown (buf)
  "Tear down the test setup for dummy testing."
  (kill-buffer buf)
  (standoff-test-utils-restore-old-config))

;;; Tests

(ert-deftest standoff-dummy-markup-create-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    ;;(setq standoff-dummy-create-id-function 'standoff-dummy-create-uuid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    ;; should be appended to backend
    (should (= (length standoff-dummy-markup) 1))
    ;; should have a return value of defined type
    (should
     (equal
      (type-of markup-id)
      (type-of (funcall standoff-dummy-create-id-function standoff-dummy-markup 0))))
    ;;(standoff-dummy-backend-inspect)
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-create-uuid-test ()
  ;; return value should be a string
  (should
   (equal (type-of (standoff-dummy-create-uuid)) 'string))
  ;; should match xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx$
  (should
   (equal (string-match "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}$" (standoff-dummy-create-uuid)) 0)))

(ert-deftest standoff-dummy-create-intid-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer)))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    ;; test create function
    (should
     (=
      (standoff-dummy-create-intid standoff-dummy-markup 0)
      1))
    ;; test funcall
    (should
     (=
      (funcall standoff-dummy-create-id-function standoff-dummy-markup 0)
      1))
    ;; test markup creation 
    (should
     (= 
      (standoff-dummy-create-markup test-buffer 23 42 "example")
      1))
    ;; subsequent creation of markup should increment id
    (should
     (= 
      (standoff-dummy-create-markup test-buffer 23 42 "example")
      2))    
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-markup-get-type-by-inst-id-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 23 42 "hammer")
    (should
     (equal
      (standoff-dummy-markup-get-type-by-inst-id test-buffer markup-id)
      "example"))
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-add-range-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    ;; test return value
    (should
     (= (standoff-dummy-add-range test-buffer 47 49 markup-id)
	markup-id))
    ;; test if in backend
    (should (= (length standoff-dummy-markup) 2))
    ;; should have the same markup-type
    (should (equal (nth 1 (car standoff-dummy-markup))
		   (nth 1 (cadr standoff-dummy-markup))))
    (should (equal (nth 1 (car standoff-dummy-markup)) "example"))
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-delete-range-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 22 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    (should (equal (length standoff-dummy-markup) 3))
    (standoff-dummy-delete-range test-buffer 47 49 "example" markup-id)
    (should (equal (length standoff-dummy-markup) 2))
    ;; delete inexistant markup range
    (should-not (standoff-dummy-delete-range test-buffer 23 24 "example" markup-id))
    (should (equal (length standoff-dummy-markup) 2))
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-read-markup-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 27 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    (should (= (length (standoff-dummy-read-markup test-buffer)) 3))
    (should (= (length (standoff-dummy-read-markup test-buffer nil nil "example")) 2))
    (should (= (length (standoff-dummy-read-markup test-buffer nil nil nil markup-id)) 2))
    ;; 1) test startchar / endchar boundaries
    ;; a) should include overlapping ranges
    (should (= (length (standoff-dummy-read-markup test-buffer 18 22)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 1 22)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 48 55)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32)) 2))
    ;; b) should include completely contained ranges
    (should (= (length (standoff-dummy-read-markup test-buffer 22 44)) 2))
    (should (= (length (standoff-dummy-read-markup test-buffer 46 50)) 1))
    ;; c) should no include non-overlapping an non-contained ranges
    (should (= (length (standoff-dummy-read-markup test-buffer 1 15)) 0))
    ;; 2) should return the same if startchar < endchar
    (should (= (length (standoff-dummy-read-markup test-buffer 32 22)) 2))
    (should-error (standoff-dummy-read-markup test-buffer 22))
    ;; 3) combinations of range and markup-type and markup-inst-id
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 "example")) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 nil markup-id)) 1))
    (should (= (length (standoff-dummy-read-markup test-buffer 22 32 "marker" markup-id)) 0))
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-markup-types-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (standoff-dummy-create-markup test-buffer 16 27 "marker")
    (standoff-dummy-add-range test-buffer 47 49 markup-id)
    ;; "example" and "marker" should be member of returned list
    (should (member "example" (standoff-dummy-markup-types test-buffer)))
    (should (member "marker" (standoff-dummy-markup-types test-buffer)))
    (should-not (member "fail" (standoff-dummy-markup-types test-buffer)))
    ;; duplicates should be removed
    (should (= (length (standoff-dummy-markup-types test-buffer)) 2))
    (standoff-dummy-test-teardown test-buffer)))
    

;; relations

(ert-deftest standoff-dummy-create-relation-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id1 (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (setq markup-id2 (standoff-dummy-create-markup test-buffer 16 27 "marker"))
    (should (= (length standoff-dummy-relations) 0))
    (standoff-dummy-create-relation test-buffer markup-id2 "marks" markup-id1)
    (should (= (length standoff-dummy-relations) 1))
    (standoff-dummy-test-teardown test-buffer)))
  
(ert-deftest standoff-dummy-used-predicates-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id1 (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (setq markup-id2 (standoff-dummy-create-markup test-buffer 47 49 "example"))
    (setq markup-id3 (standoff-dummy-create-markup test-buffer 16 27 "marker"))
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id1)
    ;; should return predicates for similar combinations
    (should
     (equal
      (standoff-dummy-used-predicates test-buffer markup-id3 markup-id2)
      (list "marks")))
    ;; should return zero length if no similar combination exists
    (should
     (=
      (length (standoff-dummy-used-predicates test-buffer markup-id1 markup-id2))
      0))
    ;; we deel with *directed* graphs here, so switching subject and
    ;; object should matter
    (should
     (=
      (length (standoff-dummy-used-predicates test-buffer markup-id2 markup-id3))
      0))
    ;; there should be no duplicates
    (standoff-dummy-create-relation test-buffer markup-id2 "marks" markup-id1)
    (should
     (equal
      (standoff-dummy-used-predicates test-buffer markup-id3 markup-id2)
      (list "marks")))    
    (standoff-dummy-test-teardown test-buffer)))
  
(ert-deftest standoff-dummy-read-relations-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id1 (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (setq markup-id2 (standoff-dummy-create-markup test-buffer 47 49 "example"))
    (setq markup-id3 (standoff-dummy-create-markup test-buffer 16 27 "marker"))
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id1)
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id2)
    (standoff-dummy-create-relation test-buffer markup-id2 "furtherConcretizes" markup-id1)
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer))
      3))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer markup-id1))
      0))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer markup-id3))
      2))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer nil "marks"))
      2))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer markup-id2 "marks"))
      0))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer nil nil markup-id1))
      2))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer nil "furtherConcretizes" markup-id1))
      1))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer markup-id2 "furtherConcretizes" markup-id1))
      1))
    (should
     (=
      (length (standoff-dummy-read-relations test-buffer markup-id2 nil markup-id1))
      1))
    (standoff-dummy-test-teardown test-buffer)))

(ert-deftest standoff-dummy-delete-relation-test ()
  (let ((test-buffer (standoff-dummy-test-setup-source-buffer))
	(markup-id nil))
    (set-buffer test-buffer)
    (setq standoff-dummy-create-id-function 'standoff-dummy-create-intid)
    (standoff-dummy--backend-reset)
    (setq markup-id1 (standoff-dummy-create-markup test-buffer 23 42 "example"))
    (setq markup-id2 (standoff-dummy-create-markup test-buffer 47 49 "example"))
    (setq markup-id3 (standoff-dummy-create-markup test-buffer 16 27 "marker"))
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id1)
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id2)
    (standoff-dummy-create-relation test-buffer markup-id2 "furtherConcretizes" markup-id1)
    ;; duplicate:
    (standoff-dummy-create-relation test-buffer markup-id3 "marks" markup-id2)
    (should (= (length standoff-dummy-relations) 4))
    (standoff-dummy-delete-relation test-buffer markup-id3 "marks" markup-id1)
    (should (= (length standoff-dummy-relations) 3))
    ;; should remove duplicates
    (standoff-dummy-delete-relation test-buffer markup-id3 "marks" markup-id2)
    (should (= (length standoff-dummy-relations) 1))
    (standoff-dummy-test-teardown test-buffer)))

;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))

;;; standoff-dummy-test.el ends here.
