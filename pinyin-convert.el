(defun pinyin-convert ()
	"Convert buffer from diacritical pinyin to tone-number pinyin."
	(interactive)
	(replace-regexp "kā fēi" "ka1 fei1" nil 0 (point-max)))

(provide 'pinyin-convert)
