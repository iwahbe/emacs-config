;;Loading Packages
(require 'dracula-theme)
(require 'slime)
(require 'linum)
(package-initialize)

;;Visual Modification
(load-theme 'dracula) ; to load the dracula theme
(linum-mode) ; to enable linum mode
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

;; execute the layout
(my-startup-layout )



