(require 'solarized-theme) ; this is just the theme
(require 'tex-site) ; this is auctex proper
(require 'pandoc-mode)
(require 'company-math)
(require 'wc-mode)
(package-initialize)

;;LaTeX keybingins
(local-set-key "\C-j" 'backward-word)

;;LaTeX packages
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;(setq-default TeX-master nil) ;set true for multidocument pages
(visual-line-mode)

(ispell-minor-mode)

(company-mode t)
(setq company-minimum-prefix-length 1)

;;Cosmetic changes
(load-theme 'solarized-light)
(linum-mode) ; linum-mode
