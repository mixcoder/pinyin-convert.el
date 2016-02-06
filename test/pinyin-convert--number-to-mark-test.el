(ert-deftest pinyin-convert-test/ka1-to-ka ()
	"Should convert 'ka1' to 'kā'."
	(should (equal "kā" (pinyin-convert--number-to-mark "ka1"))))

(ert-deftest pinyin-convert--number-to-mark/all-legal-pinyin ()
	"Should convert any legal pinyin written with tone numbers properly."
	(dolist (pair all-legal-pinyin)
		(should (equal
						 (car pair)
						 (pinyin-convert--number-to-mark (car (last pair)))))))
