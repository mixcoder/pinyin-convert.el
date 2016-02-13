(ert-deftest pinyin-convert--syllable-with-number-to-mark/ka1 ()
  "Should convert `ka1` to `kā`."
  (should (equal "kā" (pinyin-convert--syllable-with-number-to-mark "ka1"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/nar3 ()
  "Should convert `nar3` to `nǎr`"
  (should (equal "nǎr" (pinyin-convert--syllable-with-number-to-mark "nar3"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/basic-pinyin ()
  "Should convert basic tone number pinyin."
  (dolist (pinyin basic-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/neutral-pinyin ()
  "Should convert neutral tone number pinyin."
  (dolist (pinyin neutral-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/erhua-pinyin ()
  "Should convert erhua tone number pinyin."
  (dolist (pinyin erhua-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))
