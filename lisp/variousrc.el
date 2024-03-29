(defun gm/org-get-image-or-latex-filename-at-point ()
  "Get filename of org-mode image link, overlay or latex fragment.

Coppied org-mode section from ox-clip.el."
  (let ((scale nil) (el (org-element-context)))
    (cond
     ;; condition on a latex fragment
     ((eq 'latex-fragment (org-element-type el))
      (when (ov-at) (org-toggle-latex-fragment))
      ;; should be no image, so we rebuild one
      (let ((current-scale (plist-get org-format-latex-options :scale))
	    ov display file relfile)
	(plist-put org-format-latex-options :scale
		   (or scale ox-clip-default-latex-scale))
	(org-toggle-latex-fragment)
	(plist-put org-format-latex-options :scale current-scale)
	(setq ov (ov-at)
	      display (overlay-get ov 'display)
	      file (plist-get (cdr display) :file))
	(file-relative-name file)))
     ;; condition t a link of an image
     ((and (eq 'link (org-element-type el))
	   (string= "file" (org-element-property :type el))
	   (string-match (cdr (assoc "file" org-html-inline-image-rules))
			 (org-element-property :path el)))
      (file-relative-name (org-element-property :path el)))
     ;; at an overlay with a display that is an image
     ((and (ov-at)
	   (overlay-get (ov-at) 'display)
	   (plist-get (cdr (overlay-get (ov-at) 'display)) :file)
	   (string-match (cdr (assoc "file" org-html-inline-image-rules))
			 (plist-get (cdr (overlay-get (ov-at) 'display))
				    :file)))
      (file-relative-name (plist-get (cdr (overlay-get (ov-at) 'display))
				     :file)))
     ;; not sure what else we can do here.
     (t
      nil))))



(defun gm/copy-file-image-to-clipboard (image-file)
  (cond
	    ((eq system-type 'windows-nt)
	     (message "Not supported yet."))
	    ((eq system-type 'darwin)
	     (do-applescript
	      (format "set the clipboard to POSIX file \"%s\"" (expand-file-name image-file))))
	    ((eq system-type 'gnu/linux)
	     (call-process-shell-command
	      (format "xclip -selection clipboard -t image/%s -i %s"
		      (file-name-extension image-file)
		      image-file))))
  )
(defun gm/copy-image-to-clipboard (&optional image-file)
  "Copy image at point as clipboard image.

This function recognizes org-mode links, org-mode latex, dired-mode files and
image-mode buffers."
  (interactive)
  (let ((image-file (or image-file
			 (cond
			  ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill))
			  ((derived-mode-p 'org-mode)
			   (gm/org-get-image-or-latex-filename-at-point))
			  ((derived-mode-p 'image-mode) (buffer-file-name)))))
	(image (ignore-errors (cdr (image--get-image)))))
    (cond (image-file
	   (gm/copy-file-image-to-clipboard image-file)
	   (message "Copied %s" image-file))
	  (image
	   (let ((image-file (make-temp-file "emacs_copy" nil ".png"
					     ;; (plist-get image :type)
					     (plist-get image :data))))
	     (gm/copy-file-image-to-clipboard image-file)
	     (message "Copied %s" image-file)))
	   )))


;; remove cua-scrolling
(defun gm/set-cua-scroll ()
     (interactive)
(define-key cua-global-keymap [remap scroll-up]	#'cua-scroll-up)
(define-key cua-global-keymap [remap scroll-down]	#'cua-scroll-down)
(define-key cua-global-keymap [remap scroll-up-command]   #'cua-scroll-up)
(define-key cua-global-keymap [remap scroll-down-command] #'cua-scroll-down)
)
(defun gm/unset-cua-scroll ()
(define-key cua-global-keymap [remap scroll-up]	nil)
(define-key cua-global-keymap [remap scroll-down] nil)
(define-key cua-global-keymap [remap scroll-up-command] nil)
(define-key cua-global-keymap [remap scroll-down-command] nil)
)
(gm/unset-cua-scroll)


(defun gm/swap-line/up () (interactive)
       (let ((beg) (end))
	 (beginning-of-line)
	 (setq beg (point))
	 (next-line)
	 (setq end (point))
	 (kill-region beg end)
	 (previous-line)
	 (yank)
	 ))
(defun gm/swap-line/down () (interactive)
       (let ((beg) (end))
	 (beginning-of-line)
	 (setq beg (point))
	 (next-line)
	 (setq end (point))
	 (kill-region beg end)
	 (next-line)
	 (yank)
	 ))

(defun gm/edit-path ()
  "Allows a minibuffer edit of path to open new file.

Keeps old as is."
  (interactive)
  (let* ((is-file (buffer-file-name))
	 (old-path (or (buffer-file-name) default-directory))
	 (new-path (read-string "new path: " old-path)))
    (cond ((eq major-mode 'eshell-mode) (eshell/cd new-path))
	  ((eq major-mode 'shell-mode) (insert (format "cd %s" new-path)))
	  ((find-file new-path))))
 )

(add-to-list 'auto-mode-alist '("[Dd]ockerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))

(defun gm/list-buffers-by-mode (mode)
  (seq-filter (lambda (buf) (with-current-buffer buf (derived-mode-p mode))) (buffer-list)))

(defun gm/switch-to-buffer-or-create (mode create-func &optional switch-unique-func)
  "Jump to a buffer of major-mode MODE (without selecting if unique) or create such a buffer using CREATE-FUNC."
  (let* ((bufs (gm/list-buffers-by-mode mode))
	 (n (length bufs))
	 (candidates (mapcar (lambda (i) (cons (buffer-name i) i)) bufs)))
    (cond ((equal n 0) (funcall create-func))
	  ((equal n 1) (if switch-unique-func
			   (funcall switch-unique-func)
			   (switch-to-buffer (car bufs))))
	  (t  (switch-to-buffer (completing-read (format "switch to %s buffer: " mode)
						 candidates nil t)))
	  )))

(defun gm/jump-to-vterm (&optional arg)
  "Switch to vterm (or create) with multiple vterm choice if they exist."
  (interactive "P")
  (if arg (multi-vterm)
  (if (multi-vterm-dedicated-exist-p)
      (multi-vterm-dedicated-open)
      (gm/switch-to-buffer-or-create 'vterm-mode 'multi-vterm 'vterm-toggle))))

(defun gm/org-remove-and-delete-file-link ()
  (interactive)
  (let* ((link (org-element-context))
         (path (org-element-property :path link))
	 )
    (move-file-to-trash path)
    (goto-char (org-element-property :begin link))
    (delete-region (org-element-property :begin link)
                   (org-element-property :end link))
    (set-mark (point))
    (insert (org-element-property :description link))
    (activate-mark)
    )
  )
(defun gm/run-etags ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
   (shell-command "find . -type f -iname \"*.py\" | emacs.etags -")
  ))
