(ert-deftest pinyin-convert--syllable-with-mark-to-number/ka-to-ka1 ()
  "Should convert 'kā' to 'ka1'."
  (should (equal "ka1" (pinyin-convert--syllable-with-mark-to-number "kā"))))
