

(wc-mode 1)               ; provides a word count
(flyspell-mode 1)         ; recognizes misspellings
(visual-line-mode 1)      ; we want the words to wrap
(indent-tabs-mode 1)
(setq tab-width 4)

(use-package define-word
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'define-word-at-point)
  (global-set-key (kbd "C-c D") 'define-word)
  )

(local-set-key "\C-j" 'backward-word); because it gets overwritten, might want to add these keybindings on a post-init hook
