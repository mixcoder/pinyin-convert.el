(require 'pinyin-convert)

(defun read-lines (file-path)
  "Return a list of lines of a file at `file-path'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun read-pinyin (file)
  "Return a list of pairs of pinyin syllables from fixtures."
  (mapcar
   'split-string
   (read-lines
    (concat
     (file-name-directory (or load-file-name buffer-file-name))
     "fixtures/pinyin/" file))))

(defconst basic-pinyin
  (append (read-pinyin "tone-1.txt")
          (read-pinyin "tone-2.txt")
          (read-pinyin "tone-3.txt")
          (read-pinyin "tone-4.txt"))
  "An association list of basic pinyin.  An association list
mapping each tone mark pinyin syllable using tone numbers 1, 2, 3
or 4, to a list of possible tone number pinyin representations.")

(defconst basic-tone-mark-pinyin
  (mapcar 'car basic-pinyin)
  "A list of all basic tone mark pinyin.
Contains all tone mark pinyin for tone numbers 1, 2, 3 and 4.")

(defconst basic-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) basic-pinyin))
  "A list of all basic tone number pinyin.
Contains all tone number pinyin for tone numbers 1, 2, 3 and 4.")

(defconst neutral-pinyin
  (read-pinyin "tone-5.txt")
  "An association list of neutral pinyin.  An association list
mapping each neutral tone tone mark pinyin to a list of possible
tone number pinyin representations.")

(defconst neutral-tone-mark-pinyin
  (mapcar 'car neutral-pinyin)
  "A list of all tone neutral tone mark pinyin.")

(defconst neutral-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) neutral-pinyin))
  "A list of all tone neutral tone number pinyin.")

(defconst erhua-pinyin
  (mapcar
   (lambda (pinyin)
     (append (list (concat (car pinyin) "r"))
             (mapcar (lambda (syllable)
                       (concat (substring syllable 0 -1)
                               "r"
                               (substring syllable -1 nil)))
                     (cdr pinyin)))) basic-pinyin)
  "An association list for all erhua pinyin.  An association list
mapping each erhua tone mark pinyin to a list of possible tone
number pinyin representations. It is constructed essentially by
appending an `r' to existing pinyin.")

(defconst erhua-tone-mark-pinyin
  (mapcar 'car erhua-pinyin)
  "A list of all erhua tone mark pinyin.")

(defconst erhua-tone-number-pinyin
  (apply 'append (mapcar (lambda (syllables) (cdr syllables)) erhua-pinyin))
  "A list of all erhua tone number pinyin.")
