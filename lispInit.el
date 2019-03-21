
(ensure-packages '(slime linum slime-company))

;;Loading Packages
(require 'slime)
(package-initialize)

;;Visual Modification
(load-theme 'tango-dark) ; to load the dracula theme
(set-face-foreground 'linum "green")

;;Real Modification
(setq inferior-lisp-program "/usr/local/bin/clisp")
(slime-mode t)
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(slime-setup '(slime-fancy slime-company));(add-to-list 'company-backends 'slime-company)
(slime) ; to turn on slime



;; layout definition
(defun my-startup-layout ()
 (interactive)
 (delete-other-windows)
 (other-window)
 )

;;For debuging purposes
(defun was-lispInit-called ()
  (message "%s" "lispInit.el was called")
  )

;; execute the layout
(my-startup-layout)
