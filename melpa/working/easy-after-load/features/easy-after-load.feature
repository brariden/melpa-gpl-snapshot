Feature: easy-after-load
  Because managing a metric ton of eval-after-load statements is tedious,
  but I'd still really like most of my Emacs config to only be loaded when
  the relevant features are required, I need a trivial system in place to
  automatically insert those eval-after-load statements.

  Background:
    Given I have an easy-after-load directory
    And I have an "after-array.el" after-load file
    And I have an "after-buff-menu.el" after-load file

  Scenario: eval-after-load added for all relevant libraries
    When I run easy-after-load
    Then 1 after-load-alist entries should be present for 'array
    And 1 after-load-alist entries should be present for 'buff-menu

  Scenario: Only *.el files are loaded
    Given I am recording every file found by easy-after-load
    And I have an "after-wtf.rb" after-load file
    And I have a "no-ext" after-load file
    When I run easy-after-load
    Then easy-after-load should never have seen "after-wtf.rb"
    And easy-after-load should never have seen "no-ext"

  Scenario: easy-after-load a specific directory
    Given I have a second easy-after-load directory "package-inits"
    And I have an "after-env.el" after-load file in "package-inits"
    When I run easy-after-load on "package-inits"
    Then 1 after-load-alist entries should be present for 'env

  Scenario: No such directory: easy-after-load-directory
    Given I set easy-after-load-directory to "/missing/"
    When I run easy-after-load
    Then I should have seen message "easy-after-load encountered an error reading from directory: /missing/"

  Scenario: easy-after-load-pattern doesn't match the filename
    Given I have an "init-foo.el" after-load file
    When I run easy-after-load
    Then I should see message "easy-after-load-pattern failed to match against init-foo.el"

  Scenario: Custom easy-after-load-function
    Given my easy-after-load-function returns the filename sans extension
    And I have a "dired.el" after-load file
    When I run easy-after-load
    Then 1 after-load-alist entries should be present for 'dired

  Scenario: easy-after-load-function fails
    Given my easy-after-load-function just raises errors
    When I run easy-after-load
    Then I should see message "easy-after-load-function failed on after-buff-menu.el: Failure!"

  Scenario: Rerunning easy-after-load removes original entries, but does not remove non-easy-after-load entries
    When I add a custom eval-after-load line for 'array
    And I run easy-after-load
    Then 2 after-load-alist entries should be present for 'array
    When I remove my "after-array.el" after-load file
    And I run easy-after-load
    Then 1 after-load-alist entries should be present for 'array
