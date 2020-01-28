;;; Code:
(define-skeleton skeleton-math-letter
  "Inserts a latex Letter Outline into the buffer"
  "Title: "
  "\\documentclass[11pt, oneside, fullpage]{article}\n"
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
  "\\newcommand{\\nsum}{\\sum^n_{i=1}}\n"
  "\\newcommand{\\qq}{\\mathbb{Q}}\n"
  "\\newcommand{\\nn}{\\mathbb{N}}\n"
  "\\newcommand{\\exc}[1]{$ $\\\\\\noindent\\textbf{Problem #1}}\n"
  "\\newcommand{\\inpr}[2]{\\langle #1, #2\\rangle}\n"
  "\\newcommand{\\floor}[1]{\\lfloor #1 \\rfloor}\n"
  "\\newcommand{\\ceil}[1]{\\lceil #1 \\rceil}\n"
  "\\newcommand{\\bmatrix}[1]{\\begin{bmatrix}#1\\end{bmatrix}}\n"
  "\\newcommand{\\fl}{{\\mathcal L}}\n"
  "\\newcommand{\\fu}{{\\mathcal U}}\n"
  "\\usepackage{tikz}\n"
  "\\title{" (skeleton-read "Title: ") "\n"
  "  \\\\ \\large " (skeleton-read "Class: ") "}\n"
  "\\author{" user-full-name "}\n"
  "\\date{" (skeleton-read "Date: ") "}\n"
  "\\begin{document}\n"
  "\\maketitle\n"
  "$\n"
  "$\\\\\n"
  -
  "\n\n\n\\end{document}"
  )

(define-skeleton skeleton-krumm-math-homework
  "Math homework framework for David Krumm: Math 332"
  "head"
  "\\documentclass[12pt]{amsart}\n"
  "\\usepackage{amsmath,amssymb}\n"
  "\\usepackage{graphicx,enumerate}\n"
  "\n"
  "\\theoremstyle{definition}\n"
  "\\newtheorem{prob}{Problem}\n"
  "\n"
  "\\newcommand{\\N}{\\mathbb N}\n"
  "\\newcommand{\\Z}{\\mathbb Z}\n"
  "\\newcommand{\\Q}{\\mathbb Q}\n"
  "\\newcommand{\\R}{\\mathbb R}\n"
  "\\newcommand{\\C}{\\mathbb C}\n"
  "\\renewcommand{\epsilon}{\varepsilon}\n"
  "\n"
  "\\title{Math 332 Homework " (skeleton-read "Homework number: ") "}\n"
  "\\author{Ian Wahbe}\n"
  "\n"
  "\\begin{document}\n"
  "\\maketitle\n"
  "\n"
  "\\begin{prob}[" (skeleton-read "First Problem: ") "]\n"
  "\n"
  -
  "\n"
  "\\end{prob}\n"
  "\\end{document}\n"
  )

(define-skeleton skeleton-krumm-problem
  "Problem for David Krumm"
  "head"
  "\\begin{prob}[" (skeleton-read "Problem: ") "]\n"
  "\n"
  -
  "\n"
  "\\end{prob}\n"
  )


(provide 'LaTeX_skeletons)
;;; LaTeX_skeletons ends here
