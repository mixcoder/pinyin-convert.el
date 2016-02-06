(ert-deftest pinyin-convert-test/ka-to-ka1 ()
	"Should convert 'kā' to 'ka1'."
	(should (equal "ka1" (pinyin-convert--mark-to-number "kā"))))

(ert-deftest pinyin-convert-test/ka1-to-ka ()
	"Should convert 'ka1' to 'kā'."
	(should (equal "kā" (pinyin-convert--number-to-mark "ka1"))))
