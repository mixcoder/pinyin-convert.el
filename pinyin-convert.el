;; pinyin-convert 0.0.1
;;
;; converts pinyin to other pinyin

(defconst pinyin-convert--syllable-file
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "syllables.txt")
  "Absolute path to `syllables.txt'.
The file contains a list of all toneless pinyin syllables.")

(defconst pinyin-convert--syllables
  (with-temp-buffer
    (insert-file-contents pinyin-convert--syllable-file)
    (split-string (buffer-string) "\n" t))
  "A list of all toneless pinyin syllables.")

(defconst pinyin-convert--vowels
  '(("a" "ā" "á" "ǎ" "à" "a")
    ("e" "ē" "é" "ě" "è" "e")
    ("i" "ī" "í" "ǐ" "ì" "i")
    ("o" "ō" "ó" "ǒ" "ò" "o")
    ("u" "ū" "ú" "ǔ" "ù" "u")
    ("ü" "ǖ" "ǘ" "ǚ" "ǜ" "ü"))
  "An association list mapping vowels to tone-marked vowels.")

(defconst pinyin-convert--marked-vowel-list
  (apply 'append
         (mapcar (lambda (arg) (butlast (cdr arg))) pinyin-convert--vowels))
  "A list of vowels with tone marks.")

(defun pinyin-convert--mark-vowel (vowel tone-number)
  "Given a vowel and a tone number, return the tone-marked vowel."
  (nth tone-number (assoc vowel pinyin-convert--vowels)))

(defun pinyin-convert--unmark-vowel (vowel)
  "Given a vowel, return the vowel without a tone mark."
  (let (unmarked-vowel)
    (dolist (list pinyin-convert--vowels unmarked-vowel)
      (if (member vowel list) (setq unmarked-vowel (car list)))
      unmarked-vowel)))

(defun pinyin-convert--tone-number (vowel)
  "Given a vowel with a tone mark, return the tone number."
  ((lambda (arg) (if arg (- 4 (mod (+ 3 (length arg)) 4)) 5))
   (member vowel pinyin-convert--marked-vowel-list)))

(defconst pinyin-convert--syllable-with-number-regexp
  (concat
   (regexp-opt
    (apply
     'append
     (mapcar
      (lambda (syllable)
        (with-temp-buffer
          (insert syllable)
          (goto-char 0)
          (search-forward "/")
          (delete-backward-char 1)
          (goto-char 0)
          (if (search-forward "ü" nil t)
              (mapcar
               (lambda (rep)
                 (replace-regexp-in-string "ü" rep (buffer-string)))
               '("ü" "v" ":u" "u:"))
            (list (buffer-string))))) pinyin-convert--syllables))) "r?[12345]?")
  "A regular expression that matches tone number pinyin syllables.
Only tone number pinyin syllables found in Standard Chinese
should be matched.")

(defconst pinyin-convert--syllable-with-mark-regexp
  (concat
   (regexp-opt
    (apply 'append
           (mapcar
            (lambda (syllable)
              (with-temp-buffer
                (insert syllable)
                (search-backward "/")
                (delete-forward-char 1)
                (let ((unmarked-vowel (string (following-char))))
                  (mapcar (lambda (tone)
                            (delete-forward-char 1)
                            (insert (pinyin-convert--mark-vowel unmarked-vowel tone))
                            (backward-char)
                            (buffer-string)) '(1 2 3 4)))))
            pinyin-convert--syllables))) "r?")
  "A regular expression that matches tone mark pinyin syllables.
Only tone mark pinyin syllables found in Standard Chinese should
be matched.")

(defun pinyin-convert--syllable-with-number-to-mark (syllable)
  "Convert a tone number syllable to a tone mark syllable."
  (let (tone-number)
    (setq tone-number (string-to-number (substring syllable -1 nil)))
    (save-match-data
      (with-temp-buffer
        (if (equal 0 tone-number)
            (progn
              (setq tone-number 5)
              (insert syllable))
          (insert (substring syllable 0 -1)))
        ;; Replace things that stand for ü with ü
        (goto-char 0)
        (mapcar (lambda (repr)
                  (if (search-forward repr nil t) (replace-match "ü")))
                '("v" ":u" "u:"))
        (goto-char 0)
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
         ((search-forward-regexp "[iouü]") ;; should never fail
          (search-forward-regexp "[iouü]" nil t)
          (replace-match
           (pinyin-convert--mark-vowel (match-string 0) tone-number))))
        (buffer-string)))))

(defun pinyin-convert--syllable-with-mark-to-number (syllable)
  "Convert a tone mark syllable to a tone number syllable."
  (save-match-data
    (with-temp-buffer
      (insert syllable)
      (goto-char 0)
      (if (search-forward-regexp
           (regexp-opt pinyin-convert--marked-vowel-list) nil t)
          (let ((tone-number (pinyin-convert--tone-number (match-string 0))))
            (replace-match (pinyin-convert--unmark-vowel (match-string 0)))
            (goto-char (point-max))
            (insert (number-to-string tone-number))))
      (buffer-string))))

(defun pinyin-convert--to-tone-mark (begin end)
  "Convert all tone number pinyin found in region to tone mark pinyin."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (search-forward-regexp
            pinyin-convert--syllable-with-number-regexp (point-max) t)
      (replace-match
       (pinyin-convert--syllable-with-number-to-mark
        (match-string 0))))))

(defun pinyin-convert--to-tone-number (begin end)
  "Convert all tone mark pinyin found in region to tone number pinyin."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (search-forward-regexp
            pinyin-convert--syllable-with-mark-regexp (point-max) t)
      (replace-match
       (pinyin-convert--syllable-with-mark-to-number
        (match-string 0))))))

(defun pinyin-convert--string-to-tone-mark (str)
  "Convert all tone number pinyin found in `str` to tone mark pinyin."
  (with-temp-buffer
    (insert str)
    (goto-char 0)
    (pinyin-convert--to-tone-mark (point-min) (point-max))
    (buffer-string)))

(defun pinyin-convert--string-to-tone-number (str)
  "Convert all tone mark pinyin found in `str` to tone number pinyin."
  (with-temp-buffer
    (insert str)
    (goto-char 0)
    (pinyin-convert--to-tone-number (point-min) (point-max))
    (buffer-string)))

(defun pinyin-convert-to-tone-mark (begin end)
  "Convert tone number pinyin in active region to tone mark pinyin.
If there is no active region, convert the current word to tone
mark pinyin."
  (interactive "r")
  (if (use-region-p)
      (pinyin-convert--to-tone-mark begin end)
    (progn
      (forward-word)
      (let ((end (point)))
        (backward-word)
        (pinyin-convert--to-tone-mark (point) end)))))

(defun pinyin-convert-to-tone-number (begin end)
  "Convert tone mark pinyin in active region to tone number pinyin.
If there is no active region, convert the current word to tone
number pinyin."
  (interactive "r")
  (if (use-region-p)
      (pinyin-convert--to-tone-number begin end)
    (progn
      (forward-word)
      (let ((end (point)))
        (backward-word)
        (pinyin-convert--to-tone-number (point) end)))))

(provide 'pinyin-convert)
