(ert-deftest pinyin-convert--syllable-with-mark-to-number/kā ()
  "Should convert `kā` to `ka1`."
  (should (equal "ka1" (pinyin-convert--syllable-with-mark-to-number "kā"))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/nǎr ()
  "Should convert `nǎr` to `nar3`"
  (should (equal "nar3" (pinyin-convert--syllable-with-mark-to-number "nǎr"))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/basic-pinyin ()
  "Should convert basic tone mark pinyin."
  (dolist (pinyin basic-pinyin)
    (should
     (equal
      (car (cdr pinyin))
      (pinyin-convert--syllable-with-mark-to-number (car pinyin))))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/neutral-pinyin ()
  "Should convert neutral tone mark pinyin."
  (dolist (pinyin neutral-pinyin)
    (should
     (equal
      (car (cdr pinyin))
      (pinyin-convert--syllable-with-mark-to-number (car pinyin))))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/erhua-pinyin ()
  "Should convert erhua tone mark pinyin."
  (dolist (pinyin erhua-pinyin)
    (should
     (equal
      (car (cdr pinyin))
      (pinyin-convert--syllable-with-mark-to-number (car pinyin))))))
