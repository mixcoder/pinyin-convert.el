(ert-deftest pinyin-convert--syllable-with-mark-regexp/basic-pinyin ()
  "Should match basic tone mark pinyin."
  (dolist (syllable basic-tone-mark-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-mark-regexp syllable))))

(ert-deftest pinyin-convert--syllable-with-mark-regexp/erhua-pinyin ()
  "Should match erhua tone mark pinyin."
  (dolist (syllable erhua-tone-mark-pinyin)
    (should (string-match
             pinyin-convert--syllable-with-mark-regexp syllable))))

(ert-deftest pinyin-convert--syllable-with-mark-regexp/neutral-pinyin ()
  "Should not match neutral tone mark pinyin."
  (dolist (syllable neutral-tone-mark-pinyin)
    (should
     (not
      (string-match pinyin-convert--syllable-with-mark-regexp syllable)))))

(ert-deftest pinyin-convert--syllable-with-mark-regexp/cǎk ()
  "Should not match `cǎk`."
  (should
   (not
    (string-match pinyin-convert--syllable-with-number-regexp "cǎk"))))
