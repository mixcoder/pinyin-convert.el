(ert-deftest pinyin-convert--syllable-with-number-regexp/basic-pinyin ()
  "Should match basic pinyin."
  (dolist (pair basic-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-number-regexp (car (last pair))))))

(ert-deftest pinyin-convert--syllable-with-number-regexp/erhua-pinyin ()
  "Should match erhua pinyin."
  (dolist (pair erhua-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-number-regexp (car (last pair))))))
