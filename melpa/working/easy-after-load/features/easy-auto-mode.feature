Feature: easy-auto-mode
  I write ruby. Ruby has *tons* of filetypes that don't actually
  end in .rb. I also use markdown a lot, and markdown-mode isn't
  nice enough to add .md files to my auto-mode-alist. This happens
  again and again. I just want one friggin single function to run
  that can clearly map all filetypes I'm interested in, without
  having to grep through my emacs.d to figure out how I did it
  last time.

  Scenario: Adding multiple extensions for one mode
    Given I load the following:
      """
      (easy-auto-mode '((ruby-mode "\\.rake$" "\\.ru$")))
      """
    When I open a file "foo.rake"
    Then the major mode should be 'ruby-mode
    When I open a file "config.ru"
    Then the major mode should be 'ruby-mode

  Scenario: Adding multiple modes in one call
    Given I load the following:
      """
      (easy-auto-mode
       '((c-mode "\\.c2$" "\\.alsoC$")
         (sh-mode "\\.shell$" "\\.zsh$")))
      """
    When I open a file "foo.c2"
    Then the major mode should be 'c-mode
    When I open a file "foo.zsh"
    Then the major mode should be 'sh-mode
