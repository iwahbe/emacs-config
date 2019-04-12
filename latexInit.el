
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light)
  )

;;LaTeX keybingins


;;LaTeX packages
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (local-set-key "\C-j" 'backward-word)
  )

(use-package wc-mode
  :ensure t
  :config
  (wc-mode 1)
  )

(visual-line-mode) ;this makes the text wrap naturally

(ispell-minor-mode)

(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends 'company-math)
  (company-mode 1)
  (setq company-minimum-prefix-length 1)
  )

;;Cosmetic changes
(if (version<= "26.0.50" emacs-version ) 
    (display-line-numbers-mode) ; displays line numbers on the left
  (linum-mode 1) ; display-line-numbers-mode was added in v26, so if earlier, we default to linum-mode
  )


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
  "\n\n\n\\end{document}")
