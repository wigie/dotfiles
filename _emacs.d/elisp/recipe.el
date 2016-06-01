;;; Recipe commands for emacs

;;; Format of recipe file:
;;;
;;; Title: Angel Food Cake
;;; Category: Dessert
;;;
;;; Ingredients:
;;; <list here>
;;;
;;; Directions:
;;; <list here>
;;;


(setq recipe-max-column 72)
(setq recipe-half-column (/ recipe-max-column 2))
(setq recipe-work-buffer "*recipe*")
(setq recipe-ps-string "\C-@ps{348 12 translate}")
(setq recipe-title-font-string "\C-@font{Courier-Bold@9}")
(setq recipe-normal-font-string "\C-@font{Courier@9}")


(defun line-length ()
  "Return length of current line."
  (save-excursion
    (end-of-line) (current-column)))


(defun recipe-replace (regexp newtext &optional n)
  "Replace N occurances of REGEXP with NEWTEXT.  Put point after last replace."
  (let ((count 0))
    (while (and (or (null n) (< count n)) (re-search-forward regexp nil t))
      (replace-match newtext t t)
      (setq count (1+ count)))))


(defun recipe-extract ()
  "Extract the recipe at point into a new buffer."
  (interactive)
  (let (start end (curbuf (current-buffer)))
    ;; Find bounds of recipe, copy to new buffer
    (end-of-line)
    (re-search-forward "^\n*\\(Title:\\|\\'\\)" nil 1)
    (setq end (match-beginning 0))
    (beginning-of-line)
    (re-search-backward "^Title:" nil 1)
    (setq start (point))
    (save-excursion 
      (set-buffer (get-buffer-create recipe-work-buffer))
      (erase-buffer)
      (insert-buffer-substring curbuf start end))))


(defun recipe-format ()
  "Format the recipe in recipe-work-buffer for processing by 'enscript'."
  (interactive)
  (let ((fill-column recipe-max-column)
	ingred-marker direct-marker count move-num)
    (save-excursion
      (set-buffer (get-buffer-create recipe-work-buffer))

      ;; Remove all trailing whitespace
      (goto-char (point-min))
      (recipe-replace "[ \t]+$" "")

      ;; Remove title and category strings
      (goto-char (point-min))
      (recipe-replace "^\n*\\(Title:\\|Category:\\) +" "")

      ;; Put category at right edge of title line
      (goto-char (point-min))
      (forward-line 1)
      (backward-delete-char 1)
      (insert-char ?\  (- recipe-max-column (line-length)))

      ;; Mark start of ingredients
      (goto-char (point-min))
      (recipe-replace "^\n*Ingredients: *\n?" "\n")
      (setq ingred-marker (point-marker))

      ;; Mark start of instructions
      (recipe-replace "^\n*Directions: *\n?" "\n")
      (setq direct-marker (point-marker))

      ;; Put ingredients into 2 columns
      (setq count (count-lines ingred-marker direct-marker))
      (setq move-num (- 1 (/ count 2)))
      (while (> count 2)
	(goto-char (1- direct-marker))
	(if (< move-num 0) (transpose-lines move-num))
	(forward-line (- 2))
	(move-to-column recipe-half-column t)
	(delete-char 1)
	(setq count (- count 2)))

      ;; Re-fill directions
      (goto-char direct-marker)
      (fill-region (point) (point-max))

      ;; Put in formatting codes
      (goto-char (point-min))
      (insert recipe-ps-string)
      (insert recipe-title-font-string)
      (forward-line 1)
      (insert recipe-normal-font-string)
      (untabify (point-min) (point-max)))))


;;; Here are the top-level user routines

(defun recipe-print (file)
  "Print recipe to PostScript file or printer."
  (interactive (list (read-file-name 
		"Print recipe to file (RET for default printer): "
		default-directory "")))
  (let ((lpr-command "enscript")
	(lpr-switches '("-q" "-Bre")))
    (recipe-extract)
    (recipe-format)
    (save-excursion
      (set-buffer (get-buffer-create recipe-work-buffer))
      (if (not (string= file ""))
	  (setq lpr-switches 
		(append lpr-switches 
			(list (concat "-p" (expand-file-name file))))))
      (lpr-buffer)
      (message nil))))


(defun recipe-sort ()
  "Sorts recipes alphabetically by title."
  (interactive)
  (goto-char (point-min))
  (recipe-replace "\n+Title:" "\n\nTitle:")
  (recipe-replace "\n+\\'" "\n\n")
  (goto-char (point-min))
  (sort-subr nil
	     '(lambda nil
		(forward-line 1))
	     '(lambda nil
		(re-search-forward "\\(^Title:\\|\\'\\)" nil 1)
		(goto-char (match-beginning 0))
		(forward-line (- 1)))
	     nil
	     'end-of-line)
  (goto-char (point-min))
  (recipe-replace "\nTitle:" "\n\n\nTitle:")
  (goto-char (point-min)))


;;; end of recipe.el