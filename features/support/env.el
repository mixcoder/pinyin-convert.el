(require 'f)

(defvar pyd2n-support-path
	(f-dirname load-file-name))

(defvar pyd2n-features-path
	(f-parent pyd2n-support-path))

(defvar pyd2n-root-path
	(f-parent pyd2n-features-path))

(add-to-list 'load-path pyd2n-root-path)

(require 'pyd2n)
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
