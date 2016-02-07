(ert-deftest pinyin-convert--string-to-tone-mark/basic-pinyin ()
	"Should convert giant string of basic pinyin with no spaces."
	(should
	 (equal
		(mapconcat 'concat basic-tone-mark-pinyin "")
		(pinyin-convert--string-to-tone-mark
		 (mapconcat 'concat basic-tone-number-pinyin "")))))

(ert-deftest pinyin-convert--string-to-tone-mark/erhua-pinyin ()
	"Should convert giant string of erhua pinyin with no spaces."
	(should
	 (equal
		(mapconcat 'concat erhua-tone-mark-pinyin "")
		(pinyin-convert--string-to-tone-mark
		 (mapconcat 'concat erhua-tone-number-pinyin "")))))
