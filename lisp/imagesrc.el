(defun gm/org-get-image-or-latex-filename-at-point ()
  "Get filename of org-mode image link, overlay or latex fragment.

Copied org-mode section from ox-clip.el by jkitchen."
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
  "Copied from ox-clip.el by jkitchen."
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

(defun gm/image-save (&optional filename)
  "Save the image under point. Returns FILENAME.

Copied from builtin image-save but with optional filename."
  (interactive)
  (let ((image (image--get-image))
	(filename (or filename
                    (read-file-name "Write image to file: "))))
    (with-temp-buffer
      (let ((file (plist-get (cdr image) :file)))
        (if file
            (if (not (file-exists-p file))
                (error "File %s no longer exists" file)
              (insert-file-contents-literally file))
          (insert (plist-get (cdr image) :data))))
      (write-region (point-min) (point-max)
		    filename))
    filename))

(defun gm/copy-image-to-clipboard (&optional image-file)
  "Copy image at point as clipboard image.

This function recognizes org-mode links, org-mode latex, dired-mode files, 
image-mode buffers, image-at-point (assumes png)."
  (interactive)
  (let ((image-file (cond
		     (image-file)
		     ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill))
		     ((derived-mode-p 'org-mode)
		      (gm/org-get-image-or-latex-filename-at-point))
		     ((derived-mode-p 'image-mode) (buffer-file-name))
		     ((image-at-point-p) (gm/image-save (make-temp-file "emacs_copy" nil ".png")))
		    )))
    (if image-file
	(progn
	 (gm/copy-file-image-to-clipboard image-file)
	   (message "Copied %s" image-file))
	(message "no image found"))
	   ))

