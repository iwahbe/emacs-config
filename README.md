# EmacsInit
This is my Emacs initialization setup

My fundamental approach is as follows:
1. Have one main file that loads all universal packages and all universal setting such as company
   and MELPA and common keybindings
2. This file will primarily contain hooks for file loads
3. Have another set of files that contain the commands for an individual major mode - such as python
   or latex
4. These files should contain the litany of (require 'packageName) statements that currently slow
   down my load time
5. There should be a function in each mode file as well as the main file that ensures that a set of
   packages are installed, and installs them if not
6. The end goal is that a fully customized emacs will open upon load of the main file, regardless of
   what is already on the computer
