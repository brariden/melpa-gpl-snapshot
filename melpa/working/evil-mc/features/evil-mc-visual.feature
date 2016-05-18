Feature: Visual region

  Scenario: Change selected text with the cursor at the end of the region
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "wve"
    And I press "cisn't"
    Then I should see:
    """
    This isn't a line.
    This isn't a line.
    This isn't a line.
    """

  Scenario: Change selected text with the cursor at the beginning of the region
    When I replace the buffer text with:
    """
    This is a line.
    This is a line.
    This is a line.
    """
    And I press "grm"
    And I press "wveo"
    And I press "cisn't"
    Then I should see:
    """
    This isn't a line.
    This isn't a line.
    This isn't a line.
    """