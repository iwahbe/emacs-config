;; This is the main file for emacs to load, and is responsible for calling all other files
;; That means that the actual .emacs file should contatin (load "filepath") which will call everything else

(require 'package)
;;Install uninstalled packages
;;A version of this should preface any individual initFile.el


(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;;installs use-package if its not already there
(unless (package--user-selected-p 'use-package)
  (package-install 'use-package)
  )

(defun setup-iterm2 ()
  (define-key input-decode-map "[1;2A" [S-up])
  )
(setup-iterm2)




(setq read-file-name-completion-ignore-case nil)

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  (custom-set-variables
   '(vlf-application 'dont-ask))
  )



;;There is a set of key-bindings that are practically universal for me, and are thus not package dependent
(defun basic-keybindings ()
  (global-set-key "\C-r" 'scroll-down)
  (global-set-key "\C-v" 'scroll-up)
  (global-set-key "\C-l" 'forward-word)
  (global-set-key "\C-j" 'backward-word)
  (global-set-key "\C-z" 'ispell-word)
  (global-set-key "\M--" 'undo)
  (global-set-key "\C-s" 'isearch-forward-regexp); this replaces normal isearch
  (global-set-key "\C-\M-s" 'isearch-backward-regexp); replaces isearch-forward-regexp
  )
(basic-keybindings)

;;;As far as I am aware, this does not work - which is bad and annoying
;; sets backups to one folder
(defun configure-backups ()
  (setq backup-directory-alist '((".*" . "~/.emacsBackups/")))
  (setq backup-by-copying t)
  ;; sets autosaves to one folder
  (setq auto-save-file-name-transforms '((".*" "~/.emacsAutosaves/")))
  )
(configure-backups)

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0) ; this makes company respond in real time (no delay)
  (setq company-dabbrev-downcase 0) ; this makes it so company correctly gives cases
  )

;;calls the mode-hooks that do most of the heavy lifting
(add-to-list 'load-path "~/Google Drive/Bin/EmacsInit") ;this is not a permanent solution
(add-hook 'prog-mode-hook (lambda () (load "progInit.el")))           ;General Code changes
(add-hook 'python-mode-hook (lambda () (load "pythonInit.el")))       ;python
(add-hook 'emacs-lisp-mode-hook (lambda () (load "eLispInit.el")))    ;elisp
(add-hook 'lisp-mode-hook (lambda () (load "lispInit.el")))           ;lisp
(add-hook 'rust-mode-hook (lambda () (load "rustInit.el")))           ;rust

;;This is for text pages
(add-hook 'text-mode-hook (lambda () (load "textInit.el")))  ;called for everything in the category
(add-hook 'LaTeX-mode-hook (lambda () (load "latexInit.el")))         ;latex
(add-hook 'markdown-mode-hook (lambda () (load "markdownInit.el")))   ;markdown
(add-hook 'org-mode-hook (lambda () (load "orgInit.el")))



;;Add status info to the mode line
(defun mode-line-stuff ()
  (setq display-time-default-load-average nil); must be assigned before (display-time-mode 1) is called
  (display-time-mode 1); does not change in real time, so all settings must be assigned before
  (display-battery-mode 1)
  )
(mode-line-stuff)

;;smart mode line settings
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful) ;conforms to main emacs theme, set to nil to allow default colors
  (defface sml/charging ;this is much easier to see
    '((t :inherit sml/global :foreground "green")) "" :group 'smart-mode-line-faces)
  (sml/setup) ;turn on
  (add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/" ":GDrive:") t) ;re replacement Google Drive -> GDrive
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox" ":DBox:") t) ;re replacement Drop Box -> DBox
  )

(defun menu-bar-stuff ()
  ;;My goal is to have the menu bar display information, but not buttons
  (menu-bar-mode 0) ;disables menu bar
  (define-key global-map [menu-bar file] nil) ;removes the File button
  (define-key global-map [menu-bar edit] nil) ;removes the Edit button
  (define-key global-map [menu-bar options] nil) ;removes the Options button
  (define-key global-map [menu-bar tools] nil) ;removes the Tools button
  (defun disp-time ()
    (interactive)
    (define-key global-map [menu-bar time]
      (cons (shell-command-to-string "date")
	    (make-sparse-keymap "time")))
    )
  )
(menu-bar-stuff)

