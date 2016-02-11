(ert-deftest pinyin-convert--tone-number ()
  "Should return correct tone number given any marked vowel."
  (let ((i 1))
    (dolist (vowel
	     '("ā" "á" "ǎ" "à"
	       "ē" "é" "ě" "è"
	       "ī" "í" "ǐ" "ì"
	       "ō" "ó" "ǒ" "ò"
	       "ū" "ú" "ǔ" "ù"
	       "ǖ" "ǘ" "ǚ" "ǜ"))
      (should (equal (pinyin-convert--tone-number vowel) i))
      (setq i (+ i 1))
      (if (> i 4) (setq i 1)))))
