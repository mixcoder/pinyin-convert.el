(require 'pinyin-convert)

(defun read-lines (filePath)
	"Return a list of lines of a file at filePath."
	(with-temp-buffer
		(insert-file-contents filePath)
		(split-string (buffer-string) "\n" t)))

(defun read-pinyin (file)
	"Return a list of pairs of pinyin syllables from file in `/test/fixtures/pinyin`."
	(mapcar 'split-string (read-lines (concat "test/fixtures/pinyin/" file))))

(defconst basic-pinyin
	(append (read-pinyin "tone-1.txt")
					(read-pinyin "tone-2.txt")
					(read-pinyin "tone-3.txt")
					(read-pinyin "tone-4.txt")
					(read-pinyin "tone-5.txt"))
	"A list of pairs of legal pinyin.")

(defconst basic-tone-mark-pinyin
	(mapcar 'car basic-pinyin))

(defconst basic-tone-number-pinyin
	(mapcar (lambda (arg) (car (last arg))) basic-pinyin))

(defconst erhua-pinyin
	(mapcar (lambda (pair)
						(list (concat (car pair) "r")
									(concat (substring (car (last pair)) 0 -1)
													"r"
													(substring (car (last pair)) -1 nil))))
					basic-pinyin))

(defconst erhua-tone-mark-pinyin
	(mapcar 'car erhua-pinyin))

(defconst erhua-tone-number-pinyin
	(mapcar (lambda (arg) (car (last arg))) erhua-pinyin))
