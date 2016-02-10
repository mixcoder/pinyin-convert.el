Feature: Convert tone mark pinyin to tone number pinyin
  In order to convert from tone mark pinyin to tone number pinyin
  As an emacs user
  I want to run a command

  Scenario: Converting a marked region
    When I insert:
    """
    ā bé sǐ dè e 
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-number"
    And I select "ā bé sǐ dè e"
    And transient mark mode is active
    And I press "C-c p"
    Then I should see "a1 be2 si3 de4 e"

  Scenario: Converting a word
    When I insert:
    """
    ā bé sǐ dè e 
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-number"
    And I place the cursor before "sǐ"
    And I press "C-c p"
    Then I should see "ā bé si3 dè e"

  Scenario: Converting the next word
    When I insert:
    """
    ā bé sǐ dè e
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-number"
    And I place the cursor after "sǐ"
    And I press "C-c p"
    Then I should see "ā bé sǐ de4 e"
