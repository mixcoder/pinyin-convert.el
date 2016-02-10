(ert-deftest pinyin-convert--string-to-tone-number/basic-pinyin ()
  "Should convert giant string of basic pinyin with spaces."
  (should (equal
           (mapconcat 'concat basic-tone-number-pinyin " ")
           (pinyin-convert--string-to-tone-number
            (mapconcat 'concat basic-tone-mark-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-number/erhua-pinyin ()
  "Should convert giant string of erhua pinyin with spaces."
  (should (equal
           (mapconcat 'concat erhua-tone-number-pinyin " ")
           (pinyin-convert--string-to-tone-number
            (mapconcat 'concat erhua-tone-mark-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-number/non-pinyin ()
  "Should not alter text that isn't tone number pinyin."
  (should (equal
           (pinyin-convert--string-to-tone-number "hello")
           "hello"))
  (should (equal
           (pinyin-convert--string-to-tone-number "mā cat mā")
           "ma1 cat ma1")))
