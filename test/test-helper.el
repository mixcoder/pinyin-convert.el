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
          (read-pinyin "tone-4.txt"))
  "A list of pairs of legal pinyin.")

(defconst basic-tone-mark-pinyin
  (mapcar 'car basic-pinyin))

(defconst basic-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) basic-pinyin)))

(defconst neutral-pinyin (read-pinyin "tone-5.txt"))

(defconst neutral-tone-mark-pinyin
  (mapcar 'car neutral-pinyin))

(defconst neutral-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) neutral-pinyin)))

(defconst erhua-pinyin
  (mapcar
   (lambda (pinyin)
     (append (list (concat (car pinyin) "r"))
             (mapcar (lambda (syllable)
                       (concat (substring syllable 0 -1)
                               "r"
                               (substring syllable -1 nil)))
                     (cdr pinyin)))) basic-pinyin))

(defconst erhua-tone-mark-pinyin
  (mapcar 'car erhua-pinyin))

(defconst erhua-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) erhua-pinyin)))
