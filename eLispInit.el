;;This file handles my emacs-lisp code IDE
;;To load all of my packages

(ensure-packages '(dracula-theme slime slime-company linum))

(require 'dracula-theme)
(require 'slime)
(package-initialize)

;;Visual Modifications
(load-theme 'dracula) ; to load the dracula thems

;;Real Modifications
(setq inferior-lisp-program "/usr/local/bin/clisp")
(slime-setup '(slime-fancy slime-company));(add-to-list 'company-backends 'slime-company)
(slime-mode 1)
(eldoc-mode 1)

;;reactivates company
(company-mode 1)

;;For debuging purposes
(defun was-eLispInit-called ()
  (message "%s" "eLispInit.el was called")
  )
