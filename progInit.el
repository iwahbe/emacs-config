;;;This file loads general commands useful for all programing applications

(line-number-mode 0); removes line number from mode line

(if (version<= "26.0.50" emacs-version ) 
    (display-line-numbers-mode) ; displays line numbers on the left
  (linum-mode 1) ; display-line-numbers-mode was added in v26, so if earlier, we default to linum-mode
  )

(flyspell-prog-mode) ;this tells flyspell to not complain about variable names

(setq company-minimum-prefix-length 1) ;we want an active company for programming, as there are many variable names, and memory is hard

