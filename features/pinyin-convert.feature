Feature: Do Some things
  In order to convert a buffer containing pīnyīn to pin1yin1
  As an emacs user
  I want to run a command

  Scenario: Test a simple thing
    When I insert:
    """
    kā fēi
    """
    And I bind key "C-c p" to "pinyin-convert"
    And I press "C-c p"
    Then I should see "ka1 fei1"
