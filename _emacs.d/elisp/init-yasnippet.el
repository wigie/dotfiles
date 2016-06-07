;; Package yasnippet setup

(require 'yasnippet)

;; Don't use bundled snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)

;; Return key: jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(provide 'init-yasnippet)
