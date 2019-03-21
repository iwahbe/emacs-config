

(ensure-packages '(elpy py-autopep8 company-jedi company-quickhelp))

(require 'elpy)
(require 'py-autopep8)
(require 'company-jedi)
(package-initialize)

(load-theme 'wombat) ; preferred theme

(elpy-enable) ;elpy is my python IDE
(elpy-mode) ; turns on elpy

(hs-minor-mode t) ; this is hide-show and allows me to fold functions and classes

(setq elpy-rpc-backend "company")
(company-quickhelp-mode 1)
(add-to-list 'company-backends 'company-jedi) ; this allows company to use jedi
(jedi:setup) ; this ensures that jedi is up and running
(setq jedi:complete-on-dot t) ; this makes jedi start to complete on a dot, so it would display options for something like "class." even before I type anything else

(global-set-key (kbd "M-]") 'elpy-nav-indent-shift-right) 
(global-set-key (kbd "M-[") 'elpy-nav-indent-shift-left) 
(local-set-key "\C-c \C-k" 'elpy-shell-kill) ; allows me to kill the python process with a key-binding

(py-autopep8-enable-on-save) ;this ensures that autopep8 activates on save
;(setq py-autopep8-options '("--max-line-length=100")("--in-place"))

(setq elpy-rpc-python-command "python3") ;To set python3 as the default
;(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3") ; because the current version of emacs does not play well with python
(elpy-rpc-restart) ; this ensures that the jedi backend is up

(defun set-shell-python ()
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

(set-shell-python)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))
