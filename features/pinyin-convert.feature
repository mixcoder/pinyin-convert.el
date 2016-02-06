Feature: Convert some pinyin
  In order to convert a buffer containing pinyin
  As an emacs user
  I want to run a command

  Scenario: Convertng kā fēi
    When I insert:
    """
    kā fēi
    """
    And I bind key "C-c p" to "pinyin-convert"
    And I press "C-c p"
    Then I should see "ka1 fei1"
