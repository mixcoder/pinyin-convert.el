(ert-deftest pinyin-convert--syllable-with-number-to-mark/ka1-to-ka ()
  "Should convert `ka1` to `kā`."
  (should (equal "kā" (pinyin-convert--syllable-with-number-to-mark "ka1"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/nar3-to-nar ()
  "Should convert `nar3` to `nǎr`"
  (should (equal "nǎr" (pinyin-convert--syllable-with-number-to-mark "nar3"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/basic-pinyin ()
  "Should convert any legal pinyin written with tone numbers properly."
  (dolist (pinyin basic-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/neutral-pinyin ()
  "Should convert pinyin with a 5 or without a number."
  (dolist (pinyin neutral-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/erhua-pinyin ()
  "Should convert erhua pinyin."
  (dolist (pinyin erhua-pinyin)
    (dolist (syllable (cdr pinyin))
      (should
       (equal
        (car pinyin)
        (pinyin-convert--syllable-with-number-to-mark syllable))))))
