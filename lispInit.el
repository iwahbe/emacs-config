
;;Visual Modification
(load-theme 'tango-dark) ; to load the dracula theme

;;Real Modification
(setq inferior-lisp-program "/usr/local/bin/clisp")
(use-package slime
  :ensure t
  :config
  (slime-mode t)
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-fancy slime-company));(add-to-list 'company-backends 'slime-company)
    (slime) ; to turn on slime
    )
  )



;; layout definition
(defun my-startup-layout ()
 (delete-other-windows)
 (other-window)
 )

;;For debuging purposes
(defun was-lispInit-called ()
  (interactive)
  (message "%s" "lispInit.el was called")
  )

;; execute the layout
(my-startup-layout)
