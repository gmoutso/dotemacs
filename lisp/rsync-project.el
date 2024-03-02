(use-package rsync-mode)

(defun gm/tramp-to-shell (file-or-path)
  (with-parsed-tramp-file-name file-or-path tfop
  (format "%s%s:%s" (if tfop-user (format "%s@" tfop-user) "") tfop-host tfop-localname)
  ))
(defmacro gm/value-if-bound (var)
  `(if (boundp (quote ,var)) ,var))
  

(defun gm/rsync-project-file ()
  ;; uses functions from rsync-mode.el
  ;; that uses the -avR flag when find-relative non nil
  (interactive)
  (let* ((file-absolute
	  (file-truename (gm/get-filename (buffer-file-name) t)))
	 (project-root (or (gm/value-if-bound rsync-local-path)
			   (projectile-project-root)))
	 (file-relative (file-relative-name file-absolute project-root))
	 (remote-root (if (bound-and-true-p rsync-remote-paths)
			  (completing-read "Rsync project to: " rsync-remote-paths nil t)
			(gm/tramp-to-shell (read-directory-name "Remote root: "))))
	 )
    (rsync--run remote-root nil project-root nil file-relative))
    )
