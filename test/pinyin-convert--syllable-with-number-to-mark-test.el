(ert-deftest pinyin-convert--syllable-with-number-to-mark/ka1-to-ka ()
	"Should convert 'ka1' to 'kā'."
	(should (equal "kā" (pinyin-convert--syllable-with-number-to-mark "ka1"))))

(ert-deftest pinyin-convert--syllable-with-number-to-mark/all-legal-pinyin ()
	"Should convert any legal pinyin written with tone numbers properly."
	(dolist (pair all-legal-pinyin)
		(let ((pair-temp pair))
			(should (equal
							 (car pair)
							 (pinyin-convert--syllable-with-number-to-mark (car (last pair)))))
			(should (equal pair pair-temp)))))
