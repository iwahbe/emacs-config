# EmacsInit
This is my Emacs initialization setup

Diffrent blocks corrospond to diffrent use cases.
Each major-mode has it's own block. Loading is managed by `use-package`, which is installed upon running.
To bootstrap the file, you can manually call `org-babel-tangle` to create `init.el`. After that, saving the file will call `tangle-init-call` which will tangle, then byte-compile into an .elc file.

Note: changes to the `init.el` file will be ignored unless you delete the `init.elc` file, and both will be blown away on save by `tangle-init-call`. 
