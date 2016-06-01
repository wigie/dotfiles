;;; setup-term.el - Maps keys based on terminal and remote host


;; Make a key translation map if there isn't one
(if (not (keymapp key-translation-map))
    (setq key-translation-map (make-sparse-keymap)))

;; Define a new help key
(define-key key-translation-map "\eh" [help])

;; Rebind some keys to suit my own desires
(define-key global-map [insert] 'overwrite-mode)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [C-home] 'beginning-of-buffer)
(define-key global-map [C-end] 'end-of-buffer)

;;
;; The stuff below is pretty ancient
;;

;; Define keys based on terminal and remote host
;; NOTE: RHOST must be set by user
(setq remote-host (getenv "RHOST"))
(if (and (not window-system) (stringp remote-host))
    (cond
     ;; Dialed in, presumably from home (minicom)
     ((string-match "cc-tip" remote-host)
      (define-key global-map [delete] 'delete-char)
      (define-key key-translation-map [find] [home])
      (define-key key-translation-map [select] [end])
      (define-key key-translation-map [kp-f1] [f1])
      (define-key key-translation-map [kp-f2] [f2])
      (define-key key-translation-map [kp-f3] [f3])
      (define-key key-translation-map [kp-f4] [f4])
      )
     ;; Telnetted from a Mac
     ((string-match "mac" remote-host)
      (define-key global-map [delete] 'delete-char)
      (define-key function-key-map "\eO4" [insert])
      (define-key function-key-map "\eO7" [delete])
      (define-key function-key-map "\eO." [f1])
      ;; the following are in case the terminal got reset
      (define-key function-key-map "\e[1~" [insert])
      (define-key function-key-map "\e[4~" [delete])
      (define-key function-key-map "\e[17~" [f1])
      )))


;(setq term-setup-hook
;      '(lambda ()
;	 (keyboard-translate ?\C-h ?\C-?)))
