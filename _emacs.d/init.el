;;; Craig Wiegert's customization file for Emacs
;;;
;;; Time-stamp: "2018-09-16 14:13:43 wigie@lookout.home.net"

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

;;; TODO:
;;; * Enhanced completion; which one?
;;; * Popups?
;;; * Setup code modes
;;; * Theming
;;; * Split inits out of main file
;;; * Column highlighting: fill-column-mode vs column-marker
;;; * Whitespace/indentation highlight (in e.g. python-mode)
;;; * Updated modeline; also diminish
;;; * More useful fringe?
;;; * Setup variables based on what platform we're on and window system


;; Personal settings path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Package setup
(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
;; delete elpa/archives/melpa/archive-contents if
;; "wrong type argument: arrayp, nil" error appears on startup

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
       (setq-default TeX-PDF-mode t)
       (setq-default font-latex-fontify-script nil)
       (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
       (add-hook 'LaTeX-mode-hook 'flyspell-mode)))

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

;; Load other functions
(load "my-misc")
;(load "my-crypt")

;; Setup packages
(require 'init-modeline)
(require 'init-yasnippet)
(require 'init-calendar)
(require 'init-spelling)

;; Keep emacs Customize settings separate
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(when (file-exists-p custom-file)
;  (load custom-file))
