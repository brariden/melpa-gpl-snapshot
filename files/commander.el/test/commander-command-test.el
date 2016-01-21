(ert-deftest commander-test-command/simple ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help" "HELP" help)
    (parse ("help")))))

(ert-deftest commander-test-command/required-argument-present ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help <command>" "HELP" help)
    (parse ("help" "show")))))

(ert-deftest commander-test-command/required-argument-not-present ()
  (with-mock
   (mock (error "Command `%s` requires argument" "help") :times 1)
   (commander
    (command "help <command>" "HELP" help)
    (parse ("help")))))

(ert-deftest commander-test-command/one-or-more-no-arguments ()
  (with-mock
   (mock (error "Command `%s` requires at least one argument" "help") :times 1)
   (commander
    (command "help <*>" "HELP" help)
    (parse ("help")))))

(ert-deftest commander-test-command/one-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (command "help <*>" "HELP" help)
    (parse ("help" "foo" "bar" "baz")))))

(ert-deftest commander-test-command/optional-argument-present ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help [command]" "HELP" help)
    (parse ("help" "show")))))

(ert-deftest commander-test-command/optional-argument-not-present ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help [command]" "HELP" help)
    (parse ("help")))))

(ert-deftest commander-test-command/optional-argument-not-present-with-default-value ()
  (with-mock
   (mock (help "show") :times 1)
   (commander
    (command "help [command]" "HELP" help "show")
    (parse ("help")))))

(ert-deftest commander-test-command/optional-arguments-not-present-with-default-values ()
  (with-mock
   (mock (help "show" "me") :times 1)
     (commander
      (command "help [*]" "HELP" help "show" "me")
      (parse ("help")))))

(ert-deftest commander-test-command/zero-or-more-no-arguments ()
  (with-mock
   (mock (help) :times 1)
   (commander
    (command "help [*]" "HELP" help)
    (parse ("help")))))

(ert-deftest commander-test-command/zero-or-more-with-arguments ()
  (with-mock
   (mock (help "foo" "bar" "baz") :times 1)
   (commander
    (command "help [*]" "HELP" help)
    (parse ("help" "foo" "bar" "baz")))))

(ert-deftest commander-test-command/with-options ()
  (with-mock
   (mock (help "foo") :times 1)
   (mock (foo "bar") :times 1)
   (mock (qux) :times 1)
   (commander
    (option "--foo <arg>" "FOO" foo)
    (option "--qux" "QUX" qux)
    (command "help [*]" "HELP" help)
    (parse ("--foo" "bar" "help" "foo" "--qux")))))

(ert-deftest commander-test-command/required-options-then-option-after ()
  (with-mock
   (mock (say "One" "Two" "Three") :times 1)
   (mock (four) :times 1)
   (commander
    (option "--say <*>" "..." say)
    (option "--four" "..." four)
    (parse ("--say" "One" "Two" "Three" "--four")))))

(ert-deftest commander-test-command/optional-options-then-option-after ()
  (with-mock
   (mock (say "One" "Two" "Three") :times 1)
   (mock (four) :times 1)
   (commander
    (option "--say [*]" "..." say)
    (option "--four" "..." four)
    (parse ("--say" "One" "Two" "Three" "--four")))))

(ert-deftest commander-test-command/with-single-lower-capital-letter ()
  (with-mock
   (mock (command/f) :times 1)
   (commander
    (command "f" "..." command/f)
    (parse ("f")))))

(ert-deftest commander-test-command/with-single-upper-capital-letter ()
  (with-mock
   (mock (command/F) :times 1)
   (commander
    (command "F" "..." command/F)
    (parse ("F")))))

(ert-deftest commander-test-command/with-single-digit-number ()
  (with-mock
   (mock (command/0) :times 1)
   (commander
    (command "0" "..." command/0)
    (parse ("0")))))

(ert-deftest commander-test-command/with-multiple-digit-number ()
  (with-mock
   (mock (command/42) :times 1)
   (commander
    (command "42" "..." command/42)
    (parse ("42")))))

(ert-deftest commander-test-command/with-upper-case-letters ()
  (with-mock
   (mock (command/FOURTY-TWO) :times 1)
   (commander
    (command "FOURTY-TWO" "..." command/FOURTY-TWO)
    (parse ("FOURTY-TWO")))))

(ert-deftest commander-test-command/with-dash ()
  (with-mock
   (mock (command/fourty-two) :times 1)
   (commander
    (command "fourty-two" "..." command/fourty-two)
    (parse ("fourty-two")))))

(ert-deftest commander-test-command/default-with-command ()
  (with-mock
   (mock (foo) :times 1)
   (not-called bar)
   (commander
    (default "foo")
    (command "foo" "..." foo)
    (command "bar" "..." bar)
    (parse ("foo")))))

(ert-deftest commander-test-command/default-without-command ()
  (with-mock
   (mock (foo) :times 1)
   (not-called bar)
   (commander
    (default "foo")
    (command "foo" "..." foo)
    (command "bar" "..." bar)
    (parse nil))))

(ert-deftest commander-test-command/default-with-arguments-without-command ()
  (with-mock
   (mock (foo "bar" "baz" "qux") :times 1)
   (not-called bar)
   (commander
    (default "foo" "bar" "baz" "qux")
    (command "foo [*]" "..." foo)
    (command "bar" "..." bar)
    (parse nil))))
