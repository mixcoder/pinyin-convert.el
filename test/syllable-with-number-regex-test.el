(ert-deftest pinyin-convert--syllable-with-number-regex/basic-pinyin ()
  "Should match basic pinyin."
  (dolist (pair basic-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-number-regex (car (last pair))))))

(ert-deftest pinyin-convert--syllable-with-number-regex/erhua-pinyin ()
  "Should match erhua pinyin."
  (dolist (pair erhua-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-number-regex (car (last pair))))))
