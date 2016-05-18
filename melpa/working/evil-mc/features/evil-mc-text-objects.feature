Feature: Text objects and surround
  Scenario: Change the bracket type
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "csbB"
    Then I should see:
    """
    This is a {very} very {long} line with {lots of} words.
    """

  Scenario: Delete brackets inner
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "dib"
    Then I should see:
    """
    This is a () very () line with () words.
    """

  Scenario: Delete brackets outer
    When I replace the buffer text with:
    """
    This is a (very) very (long) line with (lots of) words.
    """
    And I press "f(v"
    And I press "grm"
    And I press "dab"
    Then I should see:
    """
    This is a  very  line with  words.
    """

  Scenario: Copy brackets inner
    When I replace the buffer text with:
    """
    This is a (normal) line.
    This is a (normal) line.
    This is a (normal) line.
    """
    And I press "fn"
    And I press "grm"
    And I press "yib"
    And I press "$p"
    Then I should see:
    """
    This is a (normal) line.normal
    This is a (normal) line.normal
    This is a (normal) line.normal
    """

  Scenario: Copy brackets outer
    When I replace the buffer text with:
    """
    This is a (normal) line.
    This is a (normal) line.
    This is a (normal) line.
    """
    And I press "fn"
    And I press "grm"
    And I press "yab"
    And I press "$p"
    Then I should see:
    """
    This is a (normal) line.(normal)
    This is a (normal) line.(normal)
    This is a (normal) line.(normal)
    """

  Scenario: Change a parenthesis expression inner
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I type "f(cibchanged"
    Then I should see:
    """
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    This is a (changed) with brackets.
    """

  Scenario: Change a parenthesis expression outer
    When I replace the buffer text with:
    """
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    This is a (sentence) with brackets.
    """
    And I press "grm"
    And I type "f(cabchanged"
    Then I should see:
    """
    This is a changed with brackets.
    This is a changed with brackets.
    This is a changed with brackets.
    """

  Scenario: Surround a word with quotes
    When I replace the buffer text with:
    """
    This is a simple line.
    This is a simple line.
    That is a simple line.
    This is a simple line.
    """
    And I press "grm"
    And I type "fmviwS'"
    Then I should see:
    """
    This is a 'simple' line.
    This is a 'simple' line.
    That is a simple line.
    This is a 'simple' line.
    """
