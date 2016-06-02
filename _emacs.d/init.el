;;; Craig Wiegert's customization file for Emacs
;;;
;;; Time-stamp: "2016-06-01 23:08:41 wigie@lookout.home.net"

;;; Abbreviations in code:
;;;  abbrev mode
;;;  skeleton
;;;  tempo
;;;  dabbrev?
;;;  snippets
;;;
;;; Tab completion
;;;
;;; Tags?
;;; CEDET?
;;; Semantic?
;;;
;;; Code folding with +/- or arrows in fringe: semantic-tag-folding
;;;     in cedet-contrib
;;;
;;; Should really remove BBDB; it annoys me now.

;; Add personal elisp directory to the path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Graphical vs. terminal setup
(if window-system
	(progn
      (set-scroll-bar-mode 'right)
	  (mouse-wheel-mode 1)
      (blink-cursor-mode 0)
      (set-cursor-color "purple4")
      (setq scalable-fonts-allowed t
            mouse-wheel-progressive-speed nil)
      (setq frame-title-format "%b - emacs")
;      (set-frame-size (selected-frame) 100 50))
      (add-to-list 'default-frame-alist '(height . 51))
      (add-to-list 'default-frame-alist '(width . 101)))
  ;; Turn off menu bar in terminal mode
  (progn
	(menu-bar-mode -1)))
;; These are set in X resources
;	(progn
;	  (set-foreground-color "white")
;	  (set-background-color "blue")
;	  (set-frame-font "9x15"))

;; Display latin1 characters properly (obsolete...)
;(standard-display-european t)

;; Terminal setup and general key bindings
(load "setup-keys")


;; Versioned backups to dedicated directory
;; EmacsWiki: ForceBackups/BackupDirectory
(setq version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t
      backup-directory-alist '(("." . "~/.saves")))
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

;; Time stamping
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format
      "%:y-%02m-%02d %02H:%02M:%02S %u@%h")


;; Default mode and some text mode settings
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook
		  '(lambda ()
			 (setq fill-column 72)
			 (auto-fill-mode 1)))
(line-number-mode t)

;; Tab settings
(setq-default default-tab-width 4
	      indent-tabs-mode nil)
(setq tab-stop-list 
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 
	  84 88 92 96 100 104 108 112 116 120 124 128 132))

;; Fontification and faces
(global-font-lock-mode t)
(setq font-lock-maximum-decoration
      '((c-mode . 2) (c++-mode . 2) (t . 2)))
(add-hook 'font-lock-mode-hook
		  '(lambda ()
			 (set-face-background 'region "DimGray")
			 (set-face-background 'highlight "CornflowerBlue")))
;(add-hook 'Info-mode-hook
;		  '(lambda ()
;			 (set-face-foreground 'info-node "Turquoise")
;			 (set-face-foreground 'info-xref "Orchid")))
;(add-hook 'html-mode-hook
;	  '(lambda ()
;	     (modify-syntax-entry ?! ". 1" sgml-mode-syntax-table)
;	     (modify-syntax-entry ?- "_ 234" sgml-mode-syntax-table)
;	     (set-syntax-table sgml-mode-syntax-table)
;	     (setq comment-start-skip "!--[ \t]*")))

;; C coding mode - my indentation style is basically "K&R"
;(load "cc-mode")
(defun my-c-mode-common-hook ()
  (c-set-style "cc-mode")
;  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  ;; other customizations
  (setq tab-width 8
		indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Fortran90 coding mode
(add-hook 'f90-mode-hook
		  '(lambda () 
			 (setq f90-do-indent 2
				   f90-if-indent 2
				   f90-type-indent 2
				   f90-font-lock-keywords f90-font-lock-keywords-3)
			 (f90-add-imenu-menu)))

;; Work around stupid Emacs 20 indenting
(add-hook 'scheme-mode-hook
		  '(lambda ()
			 (put 'let 'scheme-indent-function 1)))

;; Add modes
(setq auto-mode-alist
      (append '(("\\.wml\\'" . html-mode)
				("\\.F90\\'" . f90-mode)
                ("\\.ctp\\'" . php-mode))
              auto-mode-alist))

;; Version control
(setq vc-initial-comment t)
(add-hook 'vc-checkin-hook 'vc-toggle-read-only)

;; AUCTex
(condition-case nil (require 'tex-site) (error nil))
(cond ((featurep 'tex-site)
       (setq-default TeX-master nil)
       (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

;; MH mail
;(global-set-key "\C-xm" 'mh-rmail)	; normally bound to smail
(global-set-key "\C-xm" 'help)
(setq mh-summary-height		12
      mh-ins-buf-prefix		"> "
      mh-reply-default-reply-to "from"
      mh-recursive-folders	t)
(add-hook 'mh-before-quit-hook
		  '(lambda ()
			 (mh-process-or-undo-commands mh-current-folder)))

;; Global key bindings
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-xf" 'find-file)		; redefine annoying 
(global-set-key "\C-cf" 'set-fill-column)	; set-fill-column
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Bindings making M-RET equivalent to LFD for lisp evals
(define-key lisp-interaction-mode-map "\M-\C-m" 
  (lookup-key lisp-interaction-mode-map "\C-j"))

;; Recipe database
(autoload 'recipe-print "recipe")
(global-set-key "\C-cp" 'recipe-print)

;; BBDB
;(when (condition-case nil (require 'bbdb) (error nil))
;  (bbdb-initialize 'mh-e 'gnus 'message)
;  (setq bbdb-default-area-code 706		; avoid bug in bbdb-create
;		bbdb/mail-auto-create-p nil
;		bbdb/news-auto-create-p nil
;		bbdb-quiet-about-name-mismatches t
;		bbdb-always-add-addresses 'never)
;  (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh))

;; Calendar/Diary
(setq calendar-latitude 33.9047
	  calendar-longitude -83.3265
	  calendar-location-name "Athens, GA")

;; Load other functions
(load "my-misc")
;(load "my-crypt")


;; Customized variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" default)))
 '(font-latex-fontify-script nil)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-script-display (quote ((height (+ 1) height (- 1)))))
 '(font-latex-title-fontify (quote color))
 '(indicate-buffer-boundaries t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
 '(bold-italic ((t (:slant oblique :weight bold))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "sienna"))))
 '(italic ((t (:slant oblique))))
 '(mode-line ((t (:background "SlateBlue" :foreground "wheat1" :box (:line-width 1 :color "SlateBlue" :style released-button)))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width 1 :color "grey75") :weight normal)))))
