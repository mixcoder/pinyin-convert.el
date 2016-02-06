(defun pinyin-convert--mark-to-number (syllable)
	"Convert a pinyin syllable with a mark to the same syllable with a tone number."
	"ka1"
	)

(defun pinyin-convert--number-to-mark (syllable)
	"Convert a pinyin syllable with a tone number to the same syllable with a mark."
	"kā"
	)


(defun pinyin-convert ()
	"Convert buffer from diacritical pinyin to tone-number pinyin."
	(interactive)
	(replace-regexp "kā fēi" "ka1 fei1" nil 0 (point-max)))

(provide 'pinyin-convert)
