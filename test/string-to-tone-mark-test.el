(ert-deftest pinyin-convert--string-to-tone-mark/non-pinyin ()
  "Should not alter text that isn't tone number pinyin."
  (should (equal
           (pinyin-convert--string-to-tone-mark "hello") "hello")))

(ert-deftest pinyin-convert--string-to-tone-mark/basic-pinyin ()
  "Should convert giant string of basic pinyin with spaces."
  (should
   (equal
    (mapconcat 'concat basic-tone-mark-pinyin " ")
    (pinyin-convert--string-to-tone-mark
     (mapconcat (lambda (pinyin) (car (cdr pinyin))) basic-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-mark/neutral-pinyin ()
  "Should convert giant string of neutral pinyin with spaces."
  (should
   (equal
    (mapconcat 'concat neutral-tone-mark-pinyin " ")
    (pinyin-convert--string-to-tone-mark
     (mapconcat (lambda (pinyin) (car (cdr pinyin))) neutral-pinyin " ")))))

(ert-deftest pinyin-convert--string-to-tone-mark/erhua-pinyin ()
  "Should convert giant string of erhua pinyin with spaces."
  (should
   (equal
    (mapconcat 'concat erhua-tone-mark-pinyin " ")
    (pinyin-convert--string-to-tone-mark
     (mapconcat (lambda (pinyin) (car (cdr pinyin))) erhua-pinyin " ")))))
