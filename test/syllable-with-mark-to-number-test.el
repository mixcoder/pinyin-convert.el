(ert-deftest pinyin-convert--syllable-with-mark-to-number/ka-to-ka1 ()
  "Should convert `kā` to `ka1`."
  (should (equal "ka1" (pinyin-convert--syllable-with-mark-to-number "kā"))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/basic-pinyin ()
  "Should convert any legal pinyin written with tone numbers properly."
  (dolist (pair basic-pinyin)
    (should
     (equal
      (car (last pair))
      (pinyin-convert--syllable-with-mark-to-number (car pair))))))

;; (ert-deftest pinyin-convert--syllable-with-mark-to-number/neutral-pinyin ()
;;   "Should do nothing to tone neutral pinyin."
;;   (dolist (pair neutral-pinyin)
;;     (should
;;      (equal
;;       (car (last pair))
;;       (pinyin-convert--syllable-with-mark-to-number (car pair))))))

(ert-deftest pinyin-convert--syllable-with-mark-to-number/nar3-to-nar ()
  "Should convert `nǎr` to `nar3`"
  (should (equal "nar3" (pinyin-convert--syllable-with-mark-to-number "nǎr"))))
