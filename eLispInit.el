;;This file handles my lisp code IDE
;;To load all of my packages
(require 'dracula-theme)
(require 'slime)
(require 'linum)
(package-initialize)

;;Visual Modifications
(load-theme 'dracula) ; to load the dracula thems
(linum-mode) ; to enable linum mode
(set-face-foreground 'linum "green")

;;Real Modifications
(setq inferior-lisp-program "/usr/local/bin/clisp")
(slime-setup '(slime-fancy slime-company));(add-to-list 'company-backends 'slime-company)
(slime-mode)

