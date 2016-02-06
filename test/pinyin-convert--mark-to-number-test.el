(ert-deftest pinyin-convert-test/ka-to-ka1 ()
	"Should convert 'kā' to 'ka1'."
	(should (equal "ka1" (pinyin-convert--mark-to-number "kā"))))

