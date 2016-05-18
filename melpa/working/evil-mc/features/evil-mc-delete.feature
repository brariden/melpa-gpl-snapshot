Feature: Delete text

  Scenario: Delete a word
    When I replace the buffer text with "words words and more words"
    And I press "grm"
    And I type "bdw"
    Then I should see "and more "

  Scenario: Delete a letter
    When I replace the buffer text with "words words and more words"
    And I press "grm"
    And I type "bx"
    Then I should see "ords ords and more ords"

  Scenario: Delete a letter with count
    When I replace the buffer text with "words words and more words"
    And I press "grm"
    And I type "b2x"
    Then I should see "rds rds and more rds"
    
  Scenario: Delete a word with count
    When I replace the buffer text with:
    """
    Lots of words words and more words
    Lots of words words and more words
    Lots of words words and more words
    Lots of words words and more words
    """    
    And I press "grm"
    And I type "fw3dw"
    Then I should see:
    """
    Lots of more words
    Lots of more words
    Lots of more words
    Lots of more words
    """    

  Scenario: Delete a WORD
    When I replace the buffer text with:
    """
    composite-words composite-words and more composite-words
    """    
    And I press "grm"
    And I type "bdaW"
    Then I should see "and more"

  Scenario: Delete a line
    When I replace the buffer text with:
    """
    This is a line.
    That is a line.
    That is a line.
    This is a line.
    That is a line.
    """
    And I press "j"
    And I press "grm"
    And I press "dd"
    Then I should see:
    """
    This is a line.
    This is a line.
    """
    
  Scenario: Delete a line with count
    When I replace the buffer text with:
    """
    That is a line.
    This is a line.
    Another a line.
    That is a line.
    This is a line.
    This is a line.
    That is a line.
    Another a line.
    This is a line.
    Last line.
    """
    And I press "grm"
    And I press "2dd"
    Then I should see:
    """
    Another a line.
    This is a line.
    This is a line.
    Last line.
    """
    
  Scenario: Delete to the end of line
    When I replace the buffer text with:
    """
    This is a super duper long line.
    This is a super duper long line.
    This is a super duper long line.
    This is a super duper long line.
    """
    And I press "grm"
    And I press "fdD"
    Then I should see:
    """
    This is a super 
    This is a super 
    This is a super 
    This is a super 
    """
    
  Scenario: Delete to the beginning of line
    When I replace the buffer text with:
    """
    This is a super duper long line.
    This is a super duper long line.
    This is a super duper long line.
    This is a super duper long line.
    """
    And I press "grm"
    And I type "fdd^"
    Then I should see:
    """
    duper long line.
    duper long line.
    duper long line.
    duper long line.
    """
    
  Scenario: Delete up to a letter
    When I replace the buffer text with:
    """
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    """
    And I press "grm"
    And I press "fwdfh"
    Then I should see:
    """
    The road  patches of green.
    The road  patches of green.
    The road  patches of green.
    The road  patches of green.
    """
    
  Scenario: Delete till before a letter
    When I replace the buffer text with:
    """
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    """
    And I press "grm"
    And I press "fwdth"
    Then I should see:
    """
    The road h patches of green.
    The road h patches of green.
    The road h patches of green.
    The road h patches of green.
    """
    
  Scenario: Delete till before a letter with count
    When I replace the buffer text with:
    """
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    The road was dark brown with patches of green.
    """
    And I press "grm"
    And I press "fw2dth"
    Then I should see:
    """
    The road hes of green.
    The road hes of green.
    The road hes of green.
    The road hes of green.
    """
