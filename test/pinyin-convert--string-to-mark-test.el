(ert-deftest pinyin-convert--string-to-tone-mark/basic-pinyin ()
	"Should convert giant string of basic pinyin with no spaces."
	(should (equal
					 (mapconcat 'concat basic-tone-mark-pinyin "")
					 (pinyin-convert--string-to-tone-mark
						(mapconcat 'concat basic-tone-number-pinyin "")))))

(ert-deftest pinyin-convert--string-to-tone-mark/erhua-pinyin ()
	"Should convert giant string of erhua pinyin with no spaces."
	(should (equal
					 (mapconcat 'concat erhua-tone-mark-pinyin "")
					 (pinyin-convert--string-to-tone-mark
						(mapconcat 'concat erhua-tone-number-pinyin "")))))

(ert-deftest pinyin-convert--string-to-tone-mark/non-pinyin ()
	"Should not alter text that isn't tone number pinyin."
	(should (equal
					 (pinyin-convert--string-to-tone-mark "hello")
					 "hello"))
	(should (equal
					 (pinyin-convert--string-to-tone-mark "ma1 cat ma1")
					 "mā cat mā")))
