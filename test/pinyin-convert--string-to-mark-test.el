(ert-deftest pinyin-convert--string-to-tone-mark/all-syllables ()
	"Should convert giant string of legal pinyin with no spaces."
	(should
	 (equal
		(mapconcat 'concat all-legal-tone-mark-pinyin "")
		(pinyin-convert--string-to-tone-mark
		 (mapconcat 'concat all-legal-tone-number-pinyin "")))))
