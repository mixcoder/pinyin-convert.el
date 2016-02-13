(ert-deftest pinyin-convert--syllable-with-number-regexp/basic-pinyin ()
  "Should match basic tone number pinyin."
  (dolist (syllable basic-tone-number-pinyin)
    (should
     (string-match pinyin-convert--syllable-with-number-regexp syllable))))

(ert-deftest pinyin-convert--syllable-with-number-regexp/erhua-pinyin ()
  "Should match erhua tone number pinyin."
  (dolist (syllable erhua-tone-number-pinyin)
    (should
     (string-match pinyin-convert--syllable-with-number-regexp syllable))))

(ert-deftest pinyin-convert--syllable-with-number-regexp/neutral-pinyin ()
  "Should match neutral tone number pinyin."
  (dolist (syllable neutral-tone-number-pinyin)
    (should
     (string-match pinyin-convert--syllable-with-number-regexp syllable))))

(ert-deftest pinyin-convert--syllable-with-number-regexp/be5 ()
  "Should not match `be5`."
  (should
   (equal
     (string-match pinyin-convert--syllable-with-number-regexp "be5") 1)))
