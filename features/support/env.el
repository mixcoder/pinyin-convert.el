(require 'f)

(defvar pinyin-convert-support-path
	(f-dirname load-file-name))

(defvar pinyin-convert-features-path
	(f-parent pinyin-convert-support-path))

(defvar pinyin-convert-root-path
	(f-parent pinyin-convert-features-path))

(add-to-list 'load-path pinyin-convert-root-path)

(require 'pinyin-convert)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
