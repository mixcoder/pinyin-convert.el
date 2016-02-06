(defconst pinyin-convert--vowels
	'(("a" "ā" "á" "ǎ" "à" "a")
		("e" "ē" "é" "ě" "è" "e")
		("i" "ī" "í" "ǐ" "ì" "i")
		("o" "ō" "ó" "ǒ" "ò" "o")
		("u" "ū" "ú" "ǔ" "ù" "u")
		("v" "ǖ" "ǘ" "ǚ" "ǜ" "ü")))

(defun pinyin-convert--mark-to-number (syllable)
	"Convert a pinyin syllable with a mark to the same syllable with a tone number."
	"ka1")

(defun pinyin-convert--mark-vowel (vowel tone-number)
	(nth tone-number (assoc vowel pinyin-convert--vowels)))

(defun pinyin-convert--mark-second-vowel (str tone-number)
	(let ((fvi (string-match "[aeiouv]" str)) (index))
		(setq index (cond ((string-match "[aeiouv]" str (+ fvi 1))) (fvi)))
		(concat
		 (substring str 0 index)
		 (pinyin-convert--mark-vowel (substring str index (+ 1 index)) tone-number)
		 (substring str (+ index 1)))))

(defun pinyin-convert--number-to-mark (syllable)
	"Convert a pinyin syllable with a tone number to the same syllable with a mark."
	(replace-regexp-in-string
	 "v" "ü"
	 (let ((tone-number (string-to-number (substring syllable -1 nil))))
		 (substring
			(cond
			 ;; If there is an a or an e, it will take the tone mark.
			 ((string-match "a" syllable)
				(replace-regexp-in-string
				 "a" (pinyin-convert--mark-vowel "a" tone-number) syllable))
			 ((string-match "e" syllable)
				(replace-regexp-in-string
				 "e" (pinyin-convert--mark-vowel "e" tone-number) syllable))
			 ;; If there is an ou, then the o takes the tone mark.
			 ((string-match "ou" syllable)
				(replace-regexp-in-string
				 "ou" (concat (pinyin-convert--mark-vowel "o" tone-number) "u") syllable))
			 ;; Otherwise, the second vowel takes the tone mark.
			 ((string-match "[aeiouv]" syllable)
				(pinyin-convert--mark-second-vowel syllable tone-number))
			 ;; If we get here, syllable isn't a real pinyin syllable
			 (t syllable)) 0 -1))))

(defun pinyin-convert ()
	"Convert buffer from diacritical pinyin to tone-number pinyin."
	(interactive)
	(replace-regexp "kā fēi" "ka1 fei1" nil 0 (point-max)))

(provide 'pinyin-convert)
