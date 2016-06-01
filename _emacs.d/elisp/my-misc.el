;;; my-misc.el --- some useful miscellaneous functions


(defun find-all-backup-file-names (file)
  "Return a list of all backup filenames.
Return backups both with and without version numbers, as long as they exist."
  (append
   (let ((kept-new-versions 1) (kept-old-versions 0) (version-control t))
     (cdr (find-backup-file-name file)))
   (let* ((version-control 'never) 
	  (the-backup (find-backup-file-name file)))
     (if (file-exists-p (car the-backup))
	 the-backup))))


(defun mv (new-file)
  "Emulate the Unix 'mv' command for the current buffer.
Rename the current buffer and file.
Also rename the auto-saved file and any backups."
  (interactive "FMove file to: ")
  (let ((old-file buffer-file-name))
    (if (file-exists-p old-file) 
	(rename-file old-file new-file 1))
    (set-visited-file-name new-file)
    (rename-buffer (file-name-nondirectory buffer-file-name))
    (rename-auto-save-file)
    ;; Now rename all the backup files
    (mapcar '(lambda (filename)
	       (string-match (regexp-quote old-file) filename)
	       (rename-file filename
			    (concat (substring filename 0 (match-beginning 0))
				    buffer-file-name
				    (substring filename (match-end 0)))
			    1))
	    (find-all-backup-file-names old-file))))

;;; my-misc.el ends here
