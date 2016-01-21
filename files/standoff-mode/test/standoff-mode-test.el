:; exec emacs -Q --script "$0" -- "$@"

;; WARNING: Close your production files before running tests! Restart
;; Emacs before going from testing to production again, because it is
;; not shure, that all configuration is restored correctly. You might
;; loose data! -- OR BETTER: Run this test file in batch mode.

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
(require 'standoff-mode)

;; Tests

(ert-deftest standoff-markup-types-from-overlay-definition-test ()
  "Testing the function that gets allowed markup types from overlay definitions."
  (standoff-test-utils-save-old-config)
  (standoff-test-utils-setup-overlays)
  (should (member "beispiel" (standoff-markup-types-from-overlay-definition)))
  (should-not (member "fail" (standoff-markup-types-from-overlay-definition)))
  (standoff-test-utils-restore-old-config))

(ert-deftest standoff-labels-for-types-test ()
  "Test the mapping of markup types to labels."
  (let ((types (standoff-test-utils-return-markup-types-allowed))
	(labels nil))
    ;; should have no labels
    (dolist (label (standoff-labels-for-types types labels))
      (should (member label types)))
    (setq labels (standoff-test-utils-return-markup-labels))
    ;; should have Beispiel instead of beispiel
    (should-not (member "beispiel" (standoff-labels-for-types types labels)))
    (should (member "Beispiel" (standoff-labels-for-types types labels)))
    ;; should not have a label for marker
    (should (member "marker" (standoff-labels-for-types types labels)))
    ))

(ert-deftest standoff-labels-for-types-test ()
  "Test the mapping of markup types to labels."
  (let ((types (standoff-test-utils-return-markup-types-allowed))
	(labels nil))
    ;; should return the argument as long as there are no labels
    (should (equal (standoff-type-from-label-or-type "beispiel" labels) "beispiel"))
    (should (equal (standoff-type-from-label-or-type "Beispiel" labels) "Beispiel"))
    (let ((labels (standoff-test-utils-return-markup-labels)))
      ;; should return type for type and type for label
      (should (equal (standoff-type-from-label-or-type "beispiel" labels) "beispiel"))
      (should (equal (standoff-type-from-label-or-type "Beispiel" labels) "beispiel"))
      ;; should return argument for unknown label or type
      (should (equal (standoff-type-from-label-or-type "unknown" labels) "unknown"))
      )))

(ert-deftest standoff-labels-mappable-p-test ()
  "Test the mappable test for labels."
  (let ((types (standoff-test-utils-return-markup-types-allowed))
	(labels '()))
    ;; an empty labels list should be mappable
    (should (standoff-labels-mappable-p types labels))
    ;; an labels list with pairwise distinct labels should be mappable
    (setq labels (standoff-test-utils-return-markup-labels))
    (should (standoff-labels-mappable-p types labels))
    ;; should return nil if labels are not pairwise distinct
    (add-to-list 'labels '("konzept2" . "Konzept"))
    (should-not (standoff-labels-mappable-p types labels))
    ;; remove konzept2
    (setq labels (standoff-test-utils-return-markup-labels))
    (should (standoff-labels-mappable-p types labels))
    ;; should return nil if label in types
    (add-to-list 'types "Konzept")
    (should-not (standoff-labels-mappable-p types labels))
    ))

(ert-deftest standoff-markup-type-completion-test ()
  "Testing the completion list for markup types.
This list depends on the value of
`standoff-markup-require-name-require-match'."
  ;; This test is obsolete. Test standoff-markup-type-from-user-input instead!
  :expected-result :failed
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(standoff-markup-types-allowed-function 'standoff-markup-types-from-elisp)
	(standoff-markup-labels (standoff-test-utils-return-markup-labels))
	(standoff-show-labels nil)
	(standoff-markup-type-require-match t)
	(standoff-markup-types-allowed nil))
    (standoff-dummy-create-markup test-buffer 445 482 "example")
    ;; should allow nothing because markup-types-allowed empty an match required
    (should-not (member "beispiel" (standoff-markup-type-completion test-buffer)))
    (should-not (member "example" (standoff-markup-type-completion test-buffer)))
    ;; should allow "beispiel" after setting markup-types-allowed
    (setq standoff-markup-types-allowed (standoff-test-utils-return-markup-types-allowed))
    (should (member "beispiel" (standoff-markup-type-completion test-buffer)))
    ;; should allow "examples" after if match is not required but input must be confirmed
    (setq standoff-markup-type-require-match 'confirm)
    (should (member "example" (standoff-markup-type-completion test-buffer)))
    ;; should allow "examples" after if match is not required
    (setq standoff-markup-type-require-match nil)
    (should (member "example" (standoff-markup-type-completion test-buffer)))
    (should (member "beispiel" (standoff-markup-type-completion test-buffer)))
    ;; should not allow label
    (should-not (member "Beispiel" (standoff-markup-type-completion test-buffer)))
    ;; should allow label after configuring mapping
    (setq standoff-show-labels t)
    (should (member "Konzept" (standoff-markup-type-completion test-buffer)))
    ;; should not allow label if no labels
    (setq standoff-markup-labels nil)
    (should-not (member "Konzept" (standoff-markup-type-completion test-buffer)))
    ;; should still allow used types
    (should (member "example" (standoff-markup-type-completion test-buffer)))
    ;; should not allow labels if a label occurs in types
    (standoff-dummy-create-markup test-buffer 445 482 "Beispiel")
    (should-not (member "Konzept" (standoff-markup-type-completion test-buffer)))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-number-mapping-test ()
  "Testing the mapping of markup IDs to numbers."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-markup-number-mapping-setup)
    ;; should first create the numer and then subsequently get the same
    (setq number-test1 (standoff-markup-get-number test-buffer "test1"))
    (should (= (standoff-markup-get-number test-buffer "test1") number-test1))
    ;; number should be of type number 
    (should (numberp number-test1))
    ;; should increment by 1
    (setq number-test2 (standoff-markup-get-number test-buffer "test2"))
    (should (= number-test2 (+ number-test1 1)))
    ;; should get markup ID for number
    (should (equal (standoff-markup-get-by-number test-buffer number-test1) "test1"))
    ;; should get nil
    (should-not (standoff-markup-get-by-number test-buffer 23))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-highlightning-test ()
  "Testing highlightning."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer)))
    (standoff-markup-number-mapping-setup)
    ;; 1. Create highlightning
    (standoff-highlight-markup-range test-buffer 445 483 "example" "first")
    (standoff-highlight-markup-range test-buffer 528 537 "example" "first")
    (standoff-highlight-markup-range test-buffer 484 537 "example" "second")
    ;; 1.a) overlays should be created
    (should (= (length (overlays-at 445)) 1))
    ;; 1.b) should not create a similar overlay again
    (standoff-highlight-markup-range test-buffer 445 483 "example" "first")
    (should (= (length (overlays-at 445)) 1))
    ;; 1.c) should create overlay on the same range if not similar
    (make-overlay 429 439)
    (standoff-highlight-markup-range test-buffer 429 439 "example" "first")
    (should (= (length (overlays-at 429)) 2))
    (standoff-highlight-markup-range test-buffer 429 439 "example" "second")
    (should (= (length (overlays-at 429)) 3))
    ;; 2. Hide markup / Delete highlightning ;;
    ;; 2.a) should not delete non-standoff overlays +
    ;; 2.b) filter region
    (standoff-hide-markup 429 430)
    (should (= (length (overlays-at 528)) 2))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 2.c) filter id
    (standoff-hide-markup nil nil nil (standoff-markup-get-number test-buffer "second"))
    (should (= (length (overlays-at 528)) 1))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 2.d) filter for type
    (standoff-highlight-markup-range test-buffer 484 537 "beispiel" "third")
    (standoff-hide-markup nil nil "beispiel")
    (should (= (length (overlays-at 528)) 1))
    ;; non-standoff overlays should stay
    (should (= (length (overlays-at 429)) 1))
    ;; 3. Select
    ;; should not find non-standoff overlays
    (should-error (standoff-highlight-markup--select 430))
    (should (= (length (overlays-at 528)) 1))
    ;; should find
    (should (standoff-highlight-markup--select 528))
    (standoff-highlight-markup-range test-buffer 484 537 "escalmpel" "fourth")
    ;; should fail because ambiguous
    (should-error (standoff-highlight-markup--select 528))
    (standoff-test-utils-teardown-source-buffer test-buffer)
    ))

(ert-deftest standoff-markup-highlightning-with-backend-test ()
  "Test highlightning markup from dummy backend."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(id1)
	(id2)
	(id3))
    (standoff-markup-number-mapping-setup)
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "example"))
    (standoff-dummy-add-range test-buffer 528 537 id1)
    (standoff-dummy-add-range test-buffer 429 439 id1)
    (setq id2 (standoff-dummy-create-markup test-buffer 484 537 "example"))
    (standoff-dummy-add-range test-buffer 429 439 id2)
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    ;; should highlight all
    (standoff-highlight-markup)
    (should (= (length (overlays-at 429)) 3))
    ;; should hide 
    (standoff-hide-markup-region 429 430)
    (should (= (length (overlays-at 429)) 0))
    (should (= (length (overlays-at 528)) 2))
    (standoff-hide-markup-by-number (standoff-markup-get-number test-buffer id1))
    (should (= (length (overlays-at 528)) 1))
    (goto-char 528)
    (standoff-hide-markup-at-point)
    (should (= (length (overlays-at 528)) 0))
    ;; testing buffer wide functions
    (standoff-highlight-markup-buffer "marker")
    (should (= (length (overlays-at 429)) 1))
    (standoff-highlight-markup-buffer "!")
    (should (= (length (overlays-at 429)) 3))
    (standoff-hide-markup-buffer "marker")
    (should (= (length (overlays-at 429)) 2))
    (standoff-hide-markup-buffer "!")
    (should (= (length (overlays-at 429)) 0))
    ;; testing regional functions
    (standoff-highlight-markup-region 1 429 "marker")
    (should (= (length (overlays-at 429)) 1))
    (standoff-highlight-markup-region 1 429 "!")
    (should (= (length (overlays-at 429)) 3))
    (should (= (length (overlays-at 528)) 0))
    (standoff-highlight-markup-buffer)
    (standoff-hide-markup-region 1 429 "marker")
    (should (= (length (overlays-at 429)) 2))
    ;; see what overlapping means!
    (standoff-hide-markup-region 1 429 "!")
    (should (= (length (overlays-at 429)) 2))
    (standoff-hide-markup-region 1 430 "!")
    (should (= (length (overlays-at 429)) 0))
    (should (= (length (overlays-at 528)) 2))
    ;; testing numerical functions
    (let ((n3 (standoff-markup-get-number test-buffer id3)))
      (standoff-highlight-markup-by-number n3)
      ;;(should (= (length (overlays-at 429)) 1));; Fixme!
      (standoff-highlight-markup-buffer)
      (standoff-hide-markup-by-number n3)
      (should (= (length (overlays-at 429)) 2))
      )
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-markup-creation-deletion-test ()
  "Test highlightning markup from dummy backend."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(n1)
	(standoff-y-or-n-p t))
    (standoff-markup-number-mapping-setup)
    ;; 1. standoff-markup-region
    (standoff-markup-region 445 483 "example")
    ;; should have stored markup to the backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 1))
    ;; should have highlightened markup
    (should (= (length (overlays-at 445)) 1))
    ;; 2. standoff-markup-region-continue
    ;; get the number
    (setq n1 (standoff-markup-get-number test-buffer (nth standoff-pos-markup-inst-id (car (funcall standoff-markup-read-function test-buffer))))) 
    (standoff-markup-region-continue 528 537 n1)
    ;; should have stored to backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 2))
    ;; should be of same type
    (should
     (equal
      (nth standoff-pos-markup-type (car (funcall standoff-markup-read-function test-buffer)))
      (nth standoff-pos-markup-type (cadr (funcall standoff-markup-read-function test-buffer)))))
    ;; should have highlightened markup
    (should (= (length (overlays-at 528)) 1))
    ;; 3. standoff-markup-delete-range-at-point
    (standoff-markup-delete-range-at-point 445)
    ;; should have removed from the backend
    (should (= (length (funcall standoff-markup-read-function test-buffer)) 1))    
    ;; should have removed the overlay
    (should (= (length (overlays-at 445)) 0))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-relation-completion-test ()
  "Test highlightning markup from dummy backend."
  ;; This Test is obsolete. Test standoff-predicate-from-user-input instead!
  :expected-result :failed
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(id1)
	(id2)
	(id3)
	(standoff-predicate-require-match 'confirm))
    (standoff-markup-number-mapping-setup)
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "example"))
    (setq id2 (standoff-dummy-create-markup test-buffer 484 537 "example"))
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    (standoff-dummy-create-relation test-buffer id3 "marks" id1)
    (setq standoff-predicate-require-match 'confirm)
    ;; should include used predicate in completion list
    (should (= (length (standoff-predicate-completion test-buffer id3 id2)) 1))
    (should (equal (standoff-predicate-completion test-buffer id3 id2) '("marks")))
    ;; should return an empty completion list
    (should (= (length (standoff-predicate-completion test-buffer id1 id2)) 0))
    ;; should not include used predicate
    (setq standoff-predicate-require-match t)
    (should (= (length (standoff-predicate-completion test-buffer id3 id2)) 0))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-predicates-allowed-from-elisp-test ()
  "Test the restriction of predicates by `standoff-predicates-allowed-from-elisp'."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(id1)
	(id2)
	(id3)
	(standoff-relations-allowed (standoff-test-utils-return-relations-allowed)))
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "beispiel"))
    (setq id2 (standoff-dummy-create-markup test-buffer 1 443 "konzept"))
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    ;; should only return one, i.e. "markiert"
    (should (= (length (standoff-predicates-allowed-from-elisp test-buffer id3 id1)) 1))
    (should (equal (standoff-predicates-allowed-from-elisp test-buffer id3 id1) '("markiert")))
    ;; there are 2 in the setup, with beispiel as subject an nil or '() as object
    (should (= (length (standoff-predicates-allowed-from-elisp test-buffer id1 id3)) 2))
    ;; there is 0 in the setup, with marker as subject and konzept as object
    (should (= (length (standoff-predicates-allowed-from-elisp test-buffer id3 id2)) 0))
    ;; should not if no relations are allowed 
    (let ((standoff-relations-allowed nil))
      (should-not (standoff-predicates-allowed-from-elisp test-buffer id1 id3)))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-relation-creation-deletion-test ()
  "Test highlightning markup from dummy backend."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(id1)
	(id2)
	(id3))
    (standoff-markup-number-mapping-setup)
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "example"))
    (setq id2 (standoff-dummy-create-markup test-buffer 484 537 "example"))
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    ;; create relation
    (standoff-markup-relate id3 "marks" id1)
    ;; should have created a relation
    (should (= (length (funcall standoff-relations-read-function test-buffer)) 1))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-dump-load-test ()
  "Test dumping and loading."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(id1)
	(id2)
	(id3)
	(id4)
	(dump-file (make-temp-file "dump")))
    (standoff-markup-number-mapping-setup)
    (standoff-source-checksum)
    ;; create some markup in the backend
    (setq id1 (standoff-dummy-create-markup test-buffer 445 483 "example"))
    (setq id2 (standoff-dummy-create-markup test-buffer 484 537 "example"))
    (setq id3 (standoff-dummy-create-markup test-buffer 426 444 "marker"))
    ;; create relation
    (setq id4 (standoff-dummy-create-relation test-buffer id3 "marks" id1))
    ;; dump data
    (standoff-dump-elisp dump-file)
    ;; should have no values after reset
    (standoff-dummy--backend-reset)
    (should-not (standoff-dummy-read-markup test-buffer))
    ;; load dumped data
    (standoff-dummy-load-dumped dump-file)
    ;; should have markup
    (should (standoff-dummy-read-markup test-buffer))
    (should
     (equal
      (nth standoff-pos-relation-id (car (standoff-dummy-read-markup test-buffer 446 447)))
      id1))
    ;; should have relations
    (should (standoff-dummy-read-relations test-buffer))
    (should
     (equal
      (nth standoff-pos-relation-id (car (standoff-dummy-read-relations test-buffer)))
      id4))
    (should
     (equal
      (nth standoff-pos-subject (car (standoff-dummy-read-relations test-buffer)))
      id3))
    (should
     (equal
      (nth standoff-pos-predicate (car (standoff-dummy-read-relations test-buffer)))
      "marks"))
    (standoff-test-utils-teardown-source-buffer test-buffer)))

(ert-deftest standoff-load-first-generation-test ()
  "Test loading a dumped file from \"first\" generation of the api."
  (let ((test-buffer (standoff-test-utils-setup-source-buffer))
	(subj-id)
	(obj-id)
	(rel-id)
	(fname "api-first.dump.el"))
    (standoff-markup-number-mapping-setup)
    (standoff-source-checksum)
    (standoff-dummy--backend-reset)
    (standoff-dummy-load-dumped fname)
    ;; should have markup
    (should (standoff-dummy-read-markup test-buffer))
    ;; should have relations
    (should (standoff-dummy-read-relations test-buffer))
    ;; get markup used object in sole relation
    (setq obj-id (nth standoff-pos-markup-inst-id (car (standoff-dummy-read-markup test-buffer 446 447))))
    (setq subj-id (nth standoff-pos-markup-inst-id (car (standoff-dummy-read-markup test-buffer 427 428))))
    (setq rel-id (nth standoff-pos-relation-id (car (standoff-dummy-read-relations test-buffer))))
    ;; should relate the same markup elements
    (should
     (equal
      (nth standoff-pos-object (car (standoff-dummy-read-relations test-buffer)))
      obj-id))
    (should
     (equal
      (nth standoff-pos-subject (car (standoff-dummy-read-relations test-buffer)))
      subj-id))
    ;; should have a relation id
    (should rel-id)
    ;; rel-id should not = subj-id or = obj-id
    (should-not (equal rel-id subj-id))
    (should-not (equal rel-id obj-id))
    ;; should be a uuid
    (should
     (equal (string-match "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}$" rel-id) 0))


    (standoff-test-utils-teardown-source-buffer test-buffer)))


;; run tests and exit
(when noninteractive
  (ert-run-tests-batch-and-exit (car argv)))
