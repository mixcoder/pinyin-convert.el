(ert-deftest pinyin-convert--string-to-tone-number/non-pinyin ()
  "Should not alter text that isn't tone number pinyin."
  (should (equal
           (pinyin-convert--string-to-tone-number "hello") "hello")))

(ert-deftest pinyin-convert--string-to-tone-number/basic-pinyin ()
  "Should convert giant string of basic tone mark pinyin with spaces."
  (should
   (equal
    (mapconcat (lambda (pinyin) (car (cdr pinyin))) basic-pinyin " ")
    (pinyin-convert--string-to-tone-number
     (mapconcat 'concat basic-tone-mark-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-number/neutral-pinyin ()
  "Should convert giant string of neutral tone mark pinyin with spaces."
  (should
   (equal
    (mapconcat (lambda (pinyin) (car (cdr pinyin))) neutral-pinyin " ")
    (pinyin-convert--string-to-tone-number
     (mapconcat 'concat neutral-tone-mark-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-number/erhua-pinyin ()
  "Should convert giant string of erhua tone number pinyin with spaces."
  (should
   (equal
    (mapconcat (lambda (pinyin) (car (cdr pinyin))) erhua-pinyin " ")
    (pinyin-convert--string-to-tone-number
     (mapconcat 'concat erhua-tone-mark-pinyin " ")))))
