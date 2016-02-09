;; pinyin-convert 0.0.1
;;
;; converts pinyin to other pinyin

(defconst pinyin-convert--syllable-file
  (concat (file-name-directory (or load-file-name buffer-file-name)) "syllables.txt"))

(defconst pinyin-convert--syllable-with-number-regex
  (concat (regexp-opt
           (with-temp-buffer
             (insert-file-contents pinyin-convert--syllable-file)
             (split-string (buffer-string) "\n" t))) "r?[12345]")
  "A regular expression that matches legal pinyin
syllables written with tone numbers.")

(defconst pinyin-convert--vowels
  '(("a" "ā" "á" "ǎ" "à" "a")
    ("e" "ē" "é" "ě" "è" "e")
    ("i" "ī" "í" "ǐ" "ì" "i")
    ("o" "ō" "ó" "ǒ" "ò" "o")
    ("u" "ū" "ú" "ǔ" "ù" "u")
    ("v" "ǖ" "ǘ" "ǚ" "ǜ" "ü")))

(defun pinyin-convert--mark-vowel (vowel tone-number)
  "Given a vowel and a tone number, return the vowel with the appropriate mark."
  (nth tone-number (assoc vowel pinyin-convert--vowels)))

(defun pinyin-convert--syllable-with-number-to-mark (syllable)
  "Convert a pinyin syllable with a tone number to the same syllable with a mark."
  (let ((tone-number (string-to-number (substring syllable -1 nil))))
    (save-match-data
      (with-temp-buffer
        (insert syllable)
        (goto-char (point-min))
        (cond
         ;; If there is an a or an e, it will take the tone mark.
         ((search-forward "a" nil t)
          (replace-match (pinyin-convert--mark-vowel "a" tone-number)))
         ((search-forward "e" nil t)
          (replace-match (pinyin-convert--mark-vowel "e" tone-number)))
         ;; If there is an ou, then the o takes the tone mark.
         ((search-forward "ou" nil t)
          (replace-match
           (concat (pinyin-convert--mark-vowel "o" tone-number) "u")))
         ;; Otherwise, the second vowel takes the tone mark.
         ((search-forward-regexp "[iouv]") ;; should never fail
          (search-forward-regexp "[iouv]" nil t)
          (replace-match
           (pinyin-convert--mark-vowel (match-string 0) tone-number))))
        (replace-regexp-in-string
         "v" "ü" (substring (buffer-string) 0 -1))))))

(pinyin-convert--syllable-with-number-to-mark "mo1")

(defun pinyin-convert--syllable-with-mark-to-number (syllable)
  "Convert a pinyin syllable with a tone mark to the same syllable with a number."
  "ka1") ;; TODO

(defun pinyin-convert--to-tone-mark (begin end)
  "Convert all tone number pinyin found in region to tone mark pinyin."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (search-forward-regexp
            pinyin-convert--syllable-with-number-regex (point-max) t)
      (replace-match
       (pinyin-convert--syllable-with-number-to-mark
        (match-string 0))))))

(defun pinyin-convert--string-to-tone-mark (str)
  "Convert all tone number pinyin found in `str` to tone mark pinyin."
  (with-temp-buffer
    (insert str)
    (goto-char 0)
    (pinyin-convert--to-tone-mark (point-min) (point-max))
    (buffer-string)))

(defun pinyin-convert-to-tone-mark (begin end)
  "Convert tone number pinyin in active region to tone mark pinyin. If there is no active region, convert the current word to tone number pinyin."
  (interactive "r")
  (if (use-region-p)
      (pinyin-convert--to-tone-mark begin end)
    (progn
      (forward-word)
      (let ((end (point)))
        (backward-word)
        (pinyin-convert--to-tone-mark (point) end)))))

(defun pinyin-convert-to-tone-number (begin end)
  "convert any tone mark pinyin in region to tone number pinyin."
  (interactive "r")) ;; TODO

(provide 'pinyin-convert)
