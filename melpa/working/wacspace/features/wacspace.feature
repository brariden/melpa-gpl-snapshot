Feature: Set up workspace
  In order to efficiently develop in Ruby with or without Rails
  As a Rails developer
  I want to use wacspace

  Background:
    When I load the following:
    """
    (defwacspace (ruby-mode rinari-minor-mode)
      (:default
       (:winconf 3winv)
       (:aux1 "*rails console*")
       (:aux2 eshell))
      (:1
       (:frame full))
      (:2
       (:winconf 2winh)
       (:frame left))
      (:3
       (:winconf 2winh)
       (:frame right)
       (:main "*rails console*")
       (:aux1 :main)))

    (defwacspace ruby-mode
      (:before run-ruby)
      (:default
       (:winconf 2winv)
       (:aux1 "*ruby*"))
      (:1
       (:frame full))
      (:2
       (:winconf 2winh)
       (:frame left))
      (:3
       (:winconf 2winh)
       (:frame right)
       (:main "*ruby*")
       (:aux1 :main)))

    (defwacspace :default
      (:default
       (:winconf 2winv)
       (:aux1 shell)))

    (defwacspace (:default rinari-minor-mode)
      (:default
       (:winconf 3winv)
       (:aux1 eshell)
       (:aux2 shell)))
    """
    And I am in buffer "*main*" in ruby-mode

  Scenario: wacspace in rinari-mode
    When I turn on rinari-minor-mode
    And I press "C-z C-w"
    Then there should be 3 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*rails console*"
    And the 3rd window should be in buffer "*eshell*"

  Scenario: wacspace in rinari-mode half-screen
    When I turn on rinari-minor-mode
    And I press "C-z C-3"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the frame should be right aligned
    And the 1st window should be in buffer "*rails console*"

  Scenario: wacspace in ruby-mode without rinari
    When I turn off rinari-minor-mode
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: wacspace with regexp buffer matching
    When I load the following:
    """
    (defwacspace ruby-mode
        (:before run-ruby)
        (:default
         (:winconf 2winv)
         (:aux1 "*ruby")))

     (setq wacs-regexp-buffer-switching t)
     """
     And I am in buffer "*main*" in ruby-mode
     And I press "C-z C-w"
     Then there should be 2 windows
     And I should be in buffer "*main*"
     And the 2nd window should be in buffer "*ruby*"

   Scenario: :fn syntax for auxiliary condition
     When I load the following:
     """
     (defun in-foo-buffer ()
       (equal (buffer-name) "*foo*"))

     (defwacspace (ruby-mode (:fn in-foo-buffer))
       (:default
        (:winconf 2winv)
        (:aux1 "*bar*")))
     """
     And I am in buffer "*foo*" in ruby-mode
     And I press "C-z C-w"
     Then there should be 2 windows
     And I should be in buffer "*foo*"
     And the 2nd window should be in buffer "*bar*"
     When I switch to buffer "*main*"
     And I press "C-z C-w"
     Then there should be 2 windows
     And the 2nd window should be in buffer "*ruby*"

   Scenario: :var syntax for auxiliary condition
     When I load the following:
     """
     (setq foo t)

     (defwacspace (ruby-mode (:var foo))
       (:default
        (:winconf 2winv)
        (:aux1 "*baz*")))
     """
     And I am in buffer "*main*" in ruby-mode
     And I press "C-z C-w"
     Then there should be 2 windows
     And I should be in buffer "*main*"
     And the 2nd window should be in buffer "*baz*"

   Scenario: Default configuration
     When I am in buffer "*py*" in python-mode
     And I press "C-z C-w"
     Then there should be 2 windows
     And I should be in buffer "*py*"
     And the 2nd window should be in buffer "*shell*"

   Scenario: Default configuration with auxiliary condition
     When I am in buffer "*html*" in html-mode
     And I turn on rinari-minor-mode
     And I press "C-z C-w"
     Then there should be 3 windows
     And I should be in buffer "*html*"
     And the 2nd window should be in buffer "*eshell*"
     And the 3rd window should be in buffer "*shell*"

  Scenario: Inherit from the default wacspace
    When I load the following:
    """
    (defwacspace octave-mode
      (:default
       (:aux1 "*scratch*")))
    """
    And I am in buffer "*md*" in octave-mode
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*md*"
    And the 2nd window should be in buffer "*scratch*"

  Scenario: Restoring doesn't change the point or start of buffers
    When I am in buffer "*main*" in ruby-mode
    And I press "C-z C-w"
    And I switch to the next window
    And I insert some text
    And I go to the end of the buffer
    And I switch to the next window
    And I press "C-z C-w"
    And I switch to the next window
    Then the point should be at the end
