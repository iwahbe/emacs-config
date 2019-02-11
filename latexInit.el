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
(setq-default TeX-master nil)
(visual-line-mode)


;;Cosmetic changes
(load-theme 'solarized-light)
(linum-mode) ; linum-mode
