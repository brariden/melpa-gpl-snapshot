Feature: Fontification
  In order to read Pillar files easily
  As an author using Pillar
  I want to have syntax highlighting

  Background:
    Given I am in buffer "foo.pillar"
    And I clear the buffer
    And I turn on pillar-mode

  Scenario: Standard text is not fontified
    When I insert "Some text"
    And I place the cursor between "t" and "e"
    Then current point should have no face

  Scenario: Titles are fontified
    When I clear the buffer
    And I insert "!Title"
    And I place the cursor between "!" and "Title"
    Then current point should be in bold
    And current point should have the pillar-header-1-face face

    When I clear the buffer
    And I insert "!!Title"
    And I place the cursor between "!" and "Title"
    Then current point should be in bold
    And current point should have the pillar-header-2-face face

    When I clear the buffer
    And I insert "!!!Title"
    And I place the cursor between "!" and "Title"
    Then current point should be in bold
    And current point should have the pillar-header-3-face face

    When I clear the buffer
    And I insert "!!!!Title"
    And I place the cursor between "!" and "Title"
    Then current point should be in bold
    And current point should have the pillar-header-4-face face


  Scenario: Bold test is fontified
    When I clear the buffer
    And I insert "Text ""is"" formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in bold
    And current point should have the pillar-bold-face face

  Scenario: Italic text is fontified
    When I clear the buffer
    And I insert "Text ''is'' formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in italic
    And current point should have the pillar-italic-face face

  Scenario: Strike-through text is fontified
    When I clear the buffer
    And I insert "Text --is-- formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in strike-through
    And current point should have the pillar-strikethrough-face face

  Scenario: Subscript text is fontified
    When I clear the buffer
    And I insert "Text @@is@@ formatted"
    And I place the cursor between "i" and "s"
    Then current point should have the pillar-subscript-face face

  Scenario: Superscript text is fontified
    When I clear the buffer
    And I insert "Text ^^is^^ formatted"
    And I place the cursor between "i" and "s"
    Then current point should have the pillar-superscript-face face

  Scenario: Underline text is fontified
    When I clear the buffer
    And I insert "Text __is__ formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in underline
    And current point should have the pillar-underlined-face face

  Scenario: Link text is fontified
    When I clear the buffer
    And I insert "Text *is* formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in underline
    And current point should have the pillar-link-face face

  Scenario: Embedded link text is fontified
    When I clear the buffer
    And I insert "Text +is+ formatted"
    And I place the cursor between "i" and "s"
    Then current point should be in underline
    And current point should have the pillar-link-embedded-face face

  Scenario: Monospace text is fontified
    When I clear the buffer
    And I insert "Text ==is== formatted"
    And I place the cursor between "i" and "s"
    And current point should have the pillar-monospaced-face face
