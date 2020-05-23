.DEFAULT_GOAL := tangle

tangle:
	@emacs -Q -batch --eval="(progn (require \'org) (org-babel-tangle-file  \"init.org\" (concat user-emacs-directory \"init.el\")))"
	@echo -n Tangled to 
	@emacs -Q -batch --eval='(message (concat " " user-emacs-directory "init.el\n"))'

compile: tangle
	@emacs -Q -batch --eval="(byte-compile-file (concat user-emacs-directory \"init.el\"))"

