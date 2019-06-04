;;;;Python mode, this provides a customized IDE for python, tuned for python3 by default

(use-package elpy
  :init
  (elpy-enable) ;elpy is my python IDE
  (elpy-mode) ; turns on elpy
  :config
  (setq elpy-rpc-backend "company")
  (add-hook 'before-save-hook (lambda () (elpy-format-code)))
  :bind
  ("C-c q" . comment-or-uncomment-region)
  )

(use-package flycheck-mypy
  :disabled ; support for type suggestions. Not yet developed enough to matter
  :ensure
  )

(hs-minor-mode t) ; this is hide-show and allows me to fold functions and classes

(defun python-company-setup ()
  (use-package company-jedi
    :ensure t
    )
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)
    )
  (add-to-list 'company-backends 'company-jedi) ; this allows company to use jedi
  (jedi:setup) ; this ensures that jedi is up and running
  (setq jedi:complete-on-dot t) ; this makes jedi start to complete on a dot, so it would display options for something like "class." even before I type anything else
  )

(defun indent-highlight-setup ()
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
  )
(indent-highlight-setup)

(defun do-tabs ()
  ;;actual tabs
  (setq indent-tabs-mode t);tabs instead of spaces as god intended
  (setq tab-width 4)       ;we don't want obnoxisly large tabs
  )


(defun do-spaces ()
  (setq indent-tabs-mode nil);spaces instead of tabs
  )

(do-spaces)

(defun python-keybindings ()
  (global-set-key (kbd "M-]") 'elpy-nav-indent-shift-right) 
  (global-set-key (kbd "M-[") 'elpy-nav-indent-shift-left) 
  (local-set-key "\C-c \C-k" 'elpy-shell-kill) ; allows me to kill the python process with a key-binding
  )
(python-keybindings)


(setq elpy-rpc-python-command "python3") ;To set python3 as the default
(elpy-rpc-restart) ; this ensures that the jedi backend is up

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

(defun set-shell-ipython ()
  (interactive)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  )

(set-shell-python3)

(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project)
  )

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))
