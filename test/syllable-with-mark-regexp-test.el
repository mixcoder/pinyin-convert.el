(ert-deftest pinyin-convert--syllable-with-mark-regexp/basic-pinyin ()
  "Should match basic pinyin."
  (dolist (pair basic-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-mark-regexp (car pair)))))

(ert-deftest pinyin-convert--syllable-with-mark-regexp/erhua-pinyin ()
  "Should match erhua pinyin."
  (dolist (pair erhua-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-mark-regexp (car pair)))))

(ert-deftest pinyin-convert--syllable-with-mark-regexp/cǎk ()
  "Should not match `cǎk`." 
  (should
   (equal
     (string-match pinyin-convert--syllable-with-number-regexp "cǎk") nil)))
