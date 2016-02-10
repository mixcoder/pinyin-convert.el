Feature: Convert tone number pinyin to tone mark pinyin
  In order to convert from tone number pinyin to tone mark pinyin
  As an emacs user
  I want to run a command

  Scenario: Converting a region
    When I insert:
    """
    mo3 ka1 mo1
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-mark"
    And I select "ka1"
    And transient mark mode is active
    Then the region should be "ka1"
    And I press "C-c p"
    Then I should see "mo3 kā mo1"

  Scenario: Converting a word
    When I insert:
    """
    mo3 ka1 mo1
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-mark"
    And I place the cursor before "ka1"
    And I press "C-c p"
    Then I should see "mo3 kā mo1"

  Scenario: Converting the next word
    When I insert:
    """
    mo3 ka1 mo1
    """
    And I bind key "C-c p" to "pinyin-convert-to-tone-mark"
    And I place the cursor after "ka1"
    And I press "C-c p"
    Then I should see "mo3 ka1 mō"
