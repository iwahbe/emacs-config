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
(eval-when-compile
  (require 'use-package))
(unless (package--user-selected-p 'use-package)
  (package-install 'use-package)
  )
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  )


(defun my-prog-mode ()
  (setq display-line-numbers t)
  (line-number-mode 0)
  (if (version<= "26.0.50" emacs-version) 
      (display-line-numbers-mode 1) ; displays line numbers on the left
    (linum-mode 1) ; display-line-numbers-mode was added in v26, so if earlier, we default to linum-mode
    )
  (flyspell-prog-mode) ;this tells flyspell to not complain about variable names
  (eldoc-mode 1)
  (setq company-minimum-prefix-length 1) ;we want an active company for programming, as there are many variable names, and memory is hard
  (setq font-lock-maximum-decoration t) ;lots of syntax highlighting
  (subword-mode t)
  (message "%s" "(my-prog-mode) was called successfully.")
  )


(defun my-text-mode ()
  (wc-mode 1)               ; provides a word count
  (flyspell-mode 1)         ; recognizes misspellings
  (visual-line-mode 1)      ; we want the words to wrap
  (setq tab-width 4)

  (use-package define-word
    :ensure t
    :config
    (global-set-key (kbd "C-c d") 'define-word-at-point)
    (global-set-key (kbd "C-c D") 'define-word)
    )
  (message "%s" "(my-text-mode) was called successfully.")
  )



(defun setup-iterm2 ()
  (define-key input-decode-map "[1;2A" [S-up])
  )
(setup-iterm2)

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  )


(setq read-file-name-completion-ignore-case nil)

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  (custom-set-variables
   '(vlf-application 'dont-ask))
  )


(defun my-org-mode()
  (local-set-key "\C-j" 'backward-word)
  (visual-line-mode t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (message "%s" "Org mode called successfully.")
  )

(add-hook 'org-mode-hook (lambda () (my-org-mode)))



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

(use-package ess
  :mode (("\\.r\\'" . ess-r-mode)
	 ("\\.R\\'" . ess-r-mode))  
  :ensure t
  :init
  (require 'ess-site)
  :config
  (setq inferior-ess-r-program "/usr/local/bin/R")
  ;; We assume the ability to generate graphs using a WindowsX(QuartsX) program.
  (setq ess-dialect "R")
  (setq ess-ask-for-ess-directory nil) ; directory defaults to whatever ess-directory-function returns
  (setq ess-directory-function nil) ; directory defaults to ess-directory
  (setq ess-directory nil) ; directory defaults to the directory of the opened file
  )

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
  :hook (after-init . sml/setup)
  :config
  (setq sml/theme 'respectful) ;conforms to main emacs theme, set to nil to allow default colors
  (defface sml/charging ;this is much easier to see
    '((t :inherit sml/global :foreground "green")) "" :group 'smart-mode-line-faces)
  (add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/" ":GDrive:") t) ;re replacement Google Drive -> GDrive
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox" ":DBox:") t) ;re replacement Drop Box -> DBox
  )

(menu-bar-mode 0) ;disables menu bar

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-enable-math t)
  :config
  (wc-mode 1)
  (visual-line-mode 1)
  )


;;main for lisp and elisp
(use-package slime 
  :ensure t
  :mode (("\\.el\\'" . emacs-lisp-mode)
	 ("\\.lisp\\'" . lisp-mode))
  :init
  (my-prog-mode)
  :bind
  ("C-c q" . comment-or-uncomment-region)
  :init
  (setq inferior-lisp-program "/usr/local/bin/clisp")
  :config
  (slime-mode 1)
  (slime-setup)
  (use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-fancy slime-company))
    )
  (slime)
  (message "%s" "slime package loaded")
  )

;;rust main mode
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :ensure t
  :bind
  ("C-c q" . comment-or-uncomment-region)
  :config
  (setq rust-format-on-save t)
  (use-package flymake-rust
    :ensure t
    :config
    (flymake-mode 1)
    )
  (use-package cargo
    :ensure t
    :config
    (cargo-minor-mode 1)
    (setq cargo-process--enable-rust-backtrace t)
    (setq cargo-process--command-build "build --verbose")
    (setq cargo-process--command-run "run --verbose")
    )
  (use-package company-racer
    :ensure t
    :init
    (company-mode 1)
    (setq company-racer-executable "racer")
    (unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH" (expand-file-name ; this path must be absolute
    			       "/Users/ianwahbe/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")))
    :config
    (add-to-list 'company-backends 'company-racer)
    )
  )

(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . elpy-mode)
  :ensure t
  :init
  :bind
  ("C-c q" . comment-or-uncomment-region)
  ("M-]" . elpy-nav-indent-shift-right)
  ("M-[" . elpy-nav-indent-shift-left)
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "company")
  (add-hook 'before-save-hook (lambda () (elpy-format-code)))
  (use-package highlight-indent-guides
    :ensure t
    :config
    (highlight-indentation-mode 0)
    (setq highlight-indent-guides-method 'column); could be "character", "fill", "column"
    (setq highlight-indent-guides-character ?\|) ;sets character of the highlight, if in character mode
    (setq highlight-indent-guides-responsive nil); options: 'top, 'stack, this dictates if and how it responds to the cursor position
    (setq highlight-indent-guides-delay 0); respond immediately to the cursor
    (setq highlight-indent-guides-auto-enabled nil) ;this means that I can set colors, t means that it will guess based on theme
    (set-face-background 'highlight-indent-guides-odd-face "darkcyan")
    (set-face-background 'highlight-indent-guides-even-face "darkcyan")
    (set-face-foreground 'highlight-indent-guides-character-face "dimgrey")
    (highlight-indent-guides-mode 1); turn on mode
    )
  (setq indent-tabs-mode nil)
  (setq elpy-rpc-python-command "python3")
  (elpy-rpc-restart)
  (defun set-shell-python3 ()
    (interactive)
    (setq python-shell-interpreter "python3")
    (setq python-shell-interpreter-args "-i")
    (with-eval-after-load 'python
      ;;This makes readline work in the interpreter
      (defun python-shell-completion-native-try ()
	"Return non-nil if can trigger native completion."
	(let ((python-shell-completion-native-enable t)
	      (python-shell-completion-native-output-timeout
	       python-shell-completion-native-try-output-timeout))
	  (python-shell-completion-native-get-completions
	   (get-buffer-process (current-buffer))
	   nil "_"))))
    )
  (set-shell-python3)
  (defun set-shell-ipython ()
    (interactive)
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--simple-prompt -i")
    )
  (use-package pyenv-mode
    :ensure t
    :init
    (setenv "WORKON_HOME" "~/.pyenv/versions/")
    :config
    (add-to-list 'exc-path "~/.pyenv/shims")
    (pyenv-mode)
    :bind
    ("C-x p e" . pyenv-activate-current-project)
    )
  (message "%s" "Python mode was called successfully.")
  )

(use-package company-math
  :ensure t
  :after (auctex)
  :config
  (add-to-list 'company-backends 'company-math)
  (company-mode 1)
  (setq company-minimum-prefix-length 1)
  )


(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (local-set-key "\C-j" 'backward-word)
  (add-hook 'latex-mode-hook (lambda () (display-line-numbers--turn-on)))
  (ispell-minor-mode)
  (visual-line-mode)
  (define-skeleton skeleton-math-letter
    "Inserts a latex Letter Outline into the buffer"
    "Title: "
    "\\documentclass[11pt, oneside]{article}\n"
    "\\usepackage{geometry}\n"
    "\\geometry{letterpaper}\n"
    "\\usepackage{graphicx}\n"
    "\\usepackage{amssymb}\n"
    "\\usepackage{enumitem}\n"
    "\\usepackage{amsmath}\n"
    "\\usepackage{amsfonts}\n"
    "\\makeatletter\n"
    "\\newcommand{\\zz}{\\mathbb{Z}}\n"
    "\\newcommand{\\rr}{\\mathbb{R}}\n"
    "\\newcommand{\\cc}{\\mathbb{C}}\n"
    "\\newcommand{\\qq}{\\mathbb{Q}}\n"
    "\\newcommand{\\nsum}{\\sum^n_{i=1}}\n"
    "\\newcommand{\\exc}[1]{$ $\\\\\\noindent\\textbf{Problem #1}}\n"
    "\\newcommand{\\inpr}[2]{\\langle #1, #2\\rangle}\n"
    "\\newcommand{\\floor}[1]{\\lfloor #1 \\rfloor}\n"
    "\\newcommand{\\bmatrix}[1]{\\begin{bmatrix}#1\\end{bmatrix}}\n"
    "\\newcommand{\\fl}{{\\mathcal L}}\n"
    "\\newcommand{\\fu}{{\\mathcal U}}\n"
    "\\usepackage{tikz}\n"
    "\\title{" str | "Title " "\n"
    "\\\\ \\large " (setq v1 (skeleton-read "Class:"))  "}\n"
    "\\author{Ian Wahbe}\n"
    "\\date{" (setq v2 (skeleton-read "Date:")) "}\n"
    "\\begin{document}\n"
    "\\maketitle\n"
    "$\n"
    "$\\\\\n"
    -
    "\n\n\n\\end{document}"
    )
  (message "%s" "LaTex mode was called successfully.")
  )


(use-package wc-mode
  :ensure t
  :hook ((LaTeX-mode ess-mode) . wc-mode)
  :config
  (wc-mode 1)
  )

(use-package electric-operator
  :ensure t
  :hook ((ess-mode LaTeX-mode) . electric-operator-mode)
  :config
  ;(add-hook 'ess-r-mode-hook #'electric-operator-mode)
  )




;;calls the mode-hooks that do most of the heavy lifting
(add-to-list 'load-path "~/Google Drive/Bin/EmacsInit") ;this is not a permanent solution
;(add-hook 'text-mode-hook (lambda () (my-text-mode)))
;(add-hook 'prog-mode-hook (lambda () (my-prog-mode)))
