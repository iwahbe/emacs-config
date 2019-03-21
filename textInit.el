

(wc-mode 1)               ; provides a word count
(flyspell-mode 1)         ; recognizes misspellings
(visual-line-mode 1)      ; we want the words to wrap



(local-set-key "\C-j" 'backward-word); because it gets overwritten, might want to add these keybindings on a post-init hook
