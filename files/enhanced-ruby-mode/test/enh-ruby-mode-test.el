(load-file "helper.el")
(load-file "../enh-ruby-mode.el")

;; (ert-delete-all-tests)
;; (eval-buffer)
;; (ert-run-tests-interactively t)

;; In batch mode, face-attribute returns 'unspecified,
;; and it causes wrong-number-of-arguments errors.
;; This is a workaround for it.
(defun erm-darken-color (name)
  (let ((attr (face-attribute name :foreground)))
    (unless (equal attr 'unspecified)
      (color-darken-name attr 20)
      "#000000")))

(ert-deftest enh-ruby-backward-sexp-test ()
  (with-temp-enh-rb-buffer
   "rubytest-file.rb"
   (search-forward "def backward_sexp")
   (move-beginning-of-line nil)
   (enh-ruby-backward-sexp 1)
   (line-should-equal "def foo")))

(ert-deftest enh-ruby-backward-sexp-test-inner ()
  :expected-result :failed
  (with-temp-enh-rb-buffer
   "rubytest-file.rb"
   (search-forward " word_")
   (move-end-of-line nil)
   (enh-ruby-backward-sexp 2)
   (line-should-equal "%_string #{expr %_another_} word_")))

(ert-deftest enh-ruby-forward-sexp-test ()
  (with-temp-enh-rb-buffer
   "rubytest-file.rb"
   (search-forward "def foo")
   (move-beginning-of-line nil)
   (enh-ruby-forward-sexp 1)
   (forward-char 2)
   (line-should-equal "def backward_sexp")))

(ert-deftest enh-ruby-up-sexp-test ()
  (with-temp-enh-rb-buffer
   "rubytest-file.rb"
   (search-forward "test1_")
   (enh-ruby-up-sexp)
   (line-should-equal "def foo")))

(ert-deftest enh-ruby-indent-trailing-dots ()
  (with-temp-enh-rb-string
   "a.b.\nc\n"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "a.b.\n  c\n")))

(ert-deftest enh-ruby-end-of-defun ()
  (with-temp-enh-rb-string
   "class Class\ndef method\n# blah\nend # method\nend # class"
   (search-forward "blah")
   (enh-ruby-end-of-defun)
   (line-should-equal " # method")))

(ert-deftest enh-ruby-end-of-block ()
  (with-temp-enh-rb-string
   "class Class\ndef method\n# blah\nend # method\nend # class"
   (search-forward "blah")
   (enh-ruby-end-of-block)
   (line-should-equal " # method")))

(ert-deftest enh-ruby-indent-leading-dots ()
  (with-temp-enh-rb-string
   "d.e\n.f\n"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "d.e\n  .f\n")))

(ert-deftest enh-ruby-indent-pct-w-array ()
  (with-temp-enh-rb-string
   "words = %w[\nmoo\n]\n"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "words = %w[\n         moo\n        ]\n")))

(ert-deftest enh-ruby-indent-array-of-strings ()
  (with-temp-enh-rb-string
   "words = [\n'moo'\n]\n"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "words = [\n         'moo'\n        ]\n")))

(ert-deftest enh-ruby-indent-hash ()
  ;; https://github.com/zenspider/enhanced-ruby-mode/issues/78
  (with-temp-enh-rb-string
   "{\na: a,\nb: b\n}\n"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "{\n a: a,\n b: b\n}\n")))

(ert-deftest enh-ruby-indent-hash-after-cmd ()
  ;; https://github.com/zenspider/enhanced-ruby-mode/issues/78
  :expected-result :failed
  (with-temp-enh-rb-string
   "x\n{\na: a,\nb: b\n}"
   (indent-region (point-min) (point-max))
   (buffer-should-equal "x\n{\n a: a,\n b: b\n}")))
