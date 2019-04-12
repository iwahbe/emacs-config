;;This file handles my emacs-lisp code IDE
;;To load all of my packages

(use-package dracula-theme
  :ensure t
  )

;;Visual Modifications
(load-theme 'dracula) ; to load the dracula thems

;;Real Modifications
(setq inferior-lisp-program "/usr/local/bin/clisp")
(use-package slime
  :ensure t
  :config
  (slime-mode 1)
  (slime-setup)
  (use-package slime-company
    :ensure t
    )
  (slime-setup '(slime-fancy slime-company))
  )
(eldoc-mode 1)

(flymake-mode 1)

;;reactivates company
(company-mode 1)

;;For debuging purposes
(defun was-eLispInit-called ()
  (interactive)
  (message "%s" "eLispInit.el was called")
  )
