;;ensuring that all my python packages are fully initialized
;(setq package-load-list '(
;			  (elpy t)
;			  (py-autopep8 t)
;			  (company-jedi t)
;			  (linum t)
					;))
(require 'elpy)
(require 'py-autopep8)
(require 'company-jedi)
(require 'linum)
(package-initialize)

(load-theme 'wombat) ; preferred theme
(elpy-enable) ;elpy is my python IDE
(elpy-mode) ; turns on elpy
(hs-minor-mode t) ; this is hide-show and allows me to fold functions and classes
(add-to-list 'company-backends 'company-jedi) ; this allows company to use jedi
(elpy-rpc-restart) ; this ensures that the jedi backend is up
(linum-mode) ; this turns on linum mode
(global-set-key (kbd "M-]") 'indent-rigidly-right-to-tab-stop) ; allows me to re-tab blocks intelligently
(global-set-key (kbd "M-[") 'indent-rigidly-left-to-tab-stop) ; same function
(local-set-key "\C-c \C-k" 'elpy-shell-kill) ; allows me to kill the python process with a key-binding
(jedi:setup) ; this ensures that jedi is up and running
(setq jedi:complete-on-dot t) ; this makes jedi start to complete on a dot, so it would display options for something like "class." even before I type anything else
(py-autopep8-enable-on-save) ;this ensures that autopep8 activates on save
(setq elpy-rpc-python-command "python3") ;To set python3 as the default
(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3") ; because the current version of emacs does not play well with python
(with-eval-after-load "linum" (set-face-foreground 'linum "green")) ; to change the color of the numbers
(flyspell-prog-mode)
