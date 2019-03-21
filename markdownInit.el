
(ensure-packages '(pandoc-mode markdown-mode use-package))

(company-mode 1)
(wc-mode 1)
(visual-line-mode 1)
;;preview and open pdf
(defun pdf-and-view ()
  (interactive)
  (let ((call "pandoc -i ") (out buffer-file-name))
    (setq outfil (seq-subseq out 0 (- (length out) 3)))
    (setq command (seq-concatenate 'string call buffer-file-name " -o " outfil ".pdf" ))
    (call-process-shell-command command nil 0)
    (call-process-shell-command (seq-concatenate 'string "open " outfil ".pdf") nil 0)
    )
  )

;;activate setting for the markdown-mode package
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(load-theme 'wheatgrass)
