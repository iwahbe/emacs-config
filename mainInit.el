;; This is the main file for emacs to load, and is responsible for calling all other files
;; That means that the actual .emacs file should contatin (load "filepath") which will call everythin else

(require 'package)
(require 'company)
(package-initialize)

;;There is a set of key-bindings that are practically universal for me, and are thus not package dependent
(global-set-key "\C-r" 'scroll-down)
(global-set-key "\C-v" 'scroll-up)
(global-set-key "\C-l" 'forward-word)
(global-set-key "\C-j" 'backward-word)
(global-set-key "\C-z" 'ispell-word)
(global-set-key "\M--" 'undo)

;;;As far as I am aware, this does not work - which is bad and annoying
;; sets backups to one folder
(setq backup-directory-alist '((".*" . "~/.emacsBackups/")))
(setq backup-by-copying t)
;; sets autosaves to one folder
(setq auto-save-file-name-transforms '((".*" "~/.emacsAutosaves/")))

;;MELPA is good to start with
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(global-company-mode)
(setq company-idle-delay 0) ; this makes company respond in real time (no delay)
(setq company-dabbrev-downcase 0) ; this makes it so company correctly gives cases

;;calls the mode-hooks that do most of the heavy lifting
(add-to-list 'load-path "~/Google Drive/Bin/EmacsInit") ;this is not a permanent solution
(add-hook 'python-mode-hook (lambda () (load "pythonInit.el")))    ;python
(add-hook 'emacs-lisp-mode-hook (lambda () (load "eLispInit.el"))) ;elisp
(add-hook 'lisp-mode-hook (lambda () (load "lispInit.el")))        ;lsip
(add-hook 'LaTeX-mode-hook (lambda () (load "latexInit.el")))      ;latex
