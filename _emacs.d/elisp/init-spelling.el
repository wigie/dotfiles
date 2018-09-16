;; Spell-check setup

;; Require ispell here?  Some other speller?

;; CCW: Not sure if text-mode includes LaTeX here...

;; Assume regular text mode wants spell-check
(add-hook 'text-mode-hook 'flyspell-mode)

;; Add spell-check in comments/strings for programming modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; CCW: Check into this
;(after-load 'flyspell
;  (define-key flyspell-mode-map (kbd "C-;") nil)
;  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

(provide 'init-spelling)
