(ert-deftest pinyin-convert--syllable-with-number-to-mark/ka1-to-ka ()
  "Should convert `ka1` to `kā`."
  (should (equal "kā" (pinyin-convert--syllable-with-number-to-mark "ka1"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/basic-pinyin ()
  "Should convert any legal pinyin written with tone numbers properly."
  (dolist (pair basic-pinyin)
    (let ((pair-temp pair))
      (should (equal
               (car pair)
               (pinyin-convert--syllable-with-number-to-mark (car (last pair)))))
      (should (equal pair pair-temp)))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/neutral-pinyin ()
  "Should convert pinyin with a tone number of 5."
  (dolist (pair neutral-pinyin)
    (should
     (equal
      (car pair)
      (pinyin-convert--syllable-with-number-to-mark (car (last pair)))))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/nar3-to-nar ()
  "Should convert `nar3` to `nǎr`"
  (should (equal "nǎr" (pinyin-convert--syllable-with-number-to-mark "nar3"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/erhua ()
  "Should convert erhua pinyin."
  (dolist (pair erhua-pinyin)
    (let ((pair-temp pair))
      (should (equal
               (car pair)
               (pinyin-convert--syllable-with-number-to-mark (car (last pair)))))
      (should (equal pair pair-temp)))))
