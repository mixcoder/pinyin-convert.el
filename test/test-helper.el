(require 'pinyin-convert)

(defun read-lines (filePath)
	"Return a list of lines of a file at filePath."
	(with-temp-buffer
		(insert-file-contents filePath)
		(split-string (buffer-string) "\n" t)))

(defun read-pinyin (file)
	"Return a list of pairs of pinyin syllables from file in `/test/fixtures/pinyin`."
	(mapcar 'split-string (read-lines (concat "test/fixtures/pinyin/" file))))


(defconst all-legal-pinyin
	(append (read-pinyin "tone-1.txt")
					(read-pinyin "tone-2.txt")
					(read-pinyin "tone-3.txt")
					(read-pinyin "tone-4.txt")
					(read-pinyin "tone-5.txt"))
	"A list of pairs of legal pinyin.")
