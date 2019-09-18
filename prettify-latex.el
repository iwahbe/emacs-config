(define-minor-mode prettify-latex-mode 
  "Runs regex on LaTeX to standardize whitespace and other trivialities. 
Currently accounts for within both $ $ and $$ $$, but not \verbatim style environments.
Author: iwahbe"
  :init-value nil
  :lighter " pr(LaTeX)"
  :keymap
  :group 'latex

  (defvar latex-1char-regrex "\\(\\\\\[0-9a-zA-Z\]+\\|\[0-9a-zA-Z()\]\\)" "latex string to match a minimum-single-char")

  (defun latex-prettify-find-ranges-between (pattern)
    (let ((out-list nil))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pattern nil t)
	  (message "test")
	  (setq out-list (append out-list (list (point))))
	  ))
      out-list))

  (defun latex-prettify-check-if-between (place ranges)
    (if (< (length ranges) 2)
	nil
      (if (and (>= place (car ranges)) (<= place (cadr ranges )))
	  t
	(latex-prettify-check-if-between place (cddr ranges))))
    
    )

  (defun search-and-replace-in-buffer (regrex replace &optional range)
    (let ((acted nil))
      (save-excursion
	(goto-char (point-min)) ;beginning of file
	(while (re-search-forward regrex nil t)
	  (if (or (not range) (latex-prettify-check-if-between (point) range))
	      (progn (setq acted t)
		     (replace-match replace))))
	acted)))

  ;;Latex searches
  (defvar latex-regrex-replace-equation
    (list			  
     (list "\\(\[a-zA-Z0-9\]\\)\\\\" "\\1 \\\\") ; a\b -> a \b
     (list (concat latex-1char-regrex "=" latex-1char-regrex) "\\1 = \\2") ; a=b -> a = b
     (list (concat latex-1char-regrex "-" latex-1char-regrex) "\\1 - \\2") ; a-b -> a - b
     (list (concat latex-1char-regrex "\\+" latex-1char-regrex) "\\1 + \\2"); a+b -> a + b
     (list (concat latex-1char-regrex "\\*" latex-1char-regrex) "\\1 * \\2"); a*b -> a * b
     (list (concat latex-1char-regrex "/" latex-1char-regrex) "\\1 / \\2"); a/b -> a / b
     (list (concat latex-1char-regrex "," latex-1char-regrex) "\\1, \\2") ; a,b -> a, b
     (list (concat latex-1char-regrex "\\^" latex-1char-regrex) "\\1^{\\2}") ; a^b -> a^{b}
     (list (concat latex-1char-regrex "_" latex-1char-regrex) "\\1_{\\2}") ; a_b -> a_{b}
     (list (concat "  ") " ") ; only one space in any equation
     ) "List of replacements for the math environment")

  (defvar latex-regrex-replace-general
    (list
     (list "\\(\\\\item\\)\\(\[\t \]+\\)\\(.+\\)" "\\1\n\t\\3") ; \item a -> \item \n\t a
     (list "\[ \t\]+$" "") ; removes blanks at the end of lines
     ) "List of replacements regardless of environment.")

  (defun search-and-replace-list (list &optional range)
    (dolist (item list)
      (while (search-and-replace-in-buffer (car item) (cadr item) range))))

  (defun prettify-latex ()
    (interactive)
    (if prettify-latex-mode
	(let ((equation-range (latex-prettify-find-ranges-between "\[^\\\]\\$\\$?")))
	  (search-and-replace-list latex-regrex-replace-equation equation-range)
	  (search-and-replace-list latex-regrex-replace-general nil)
	  )))

  (add-hook 'before-save-hook 'prettify-latex nil 'local)

  )

