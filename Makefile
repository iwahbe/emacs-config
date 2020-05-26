.DEFAULT_GOAL := tangle

tangle:
	@emacs -Q -batch --eval="(progn (require 'org) (org-babel-tangle-file  \"init.org\" (concat user-emacs-directory \"init.el\")))"
	@echo -n Tangled to 
	@emacs -Q -batch --eval='(message (concat " " user-emacs-directory "init.el\n"))'

compile: tangle
	@emacs -Q -batch --eval="(byte-compile-file (concat user-emacs-directory \"init.el\"))"

hooks:
	@cp .hooks/pre-commit .git/hooks/pre-commit
	@cp .hooks/post-commit .git/hooks/post-commit
	@chmod +x .git/hooks/pre-commit
	@chmod +x .git/hooks/post-commit
	@echo "Hooks symlinked and enabled"

dict:
	git clone git://anongit.freedesktop.org/libreoffice/dictionaries

pyls:
	@read -p "Python command to install under: " pcommand; \
	$$pcommand -m pip install python-language-server

typescript:
	npm i -g typescript-language-server; npm i -g typescript

init: tangle hooks dict

servers: pyls typescript
