;; to make lsp work in eg org
;; fixes error: "lsp is not a valied checker"
;; (flycheck-define-generic-checker 'lsp
;;     "A syntax checker using the Language Server Protocol (LSP)
;; provided by lsp-mode.
;; See https://github.com/emacs-lsp/lsp-mode."
;;     :start #'lsp-diagnostics--flycheck-start
;;     :modes '(lsp-placeholder-mode) ;; placeholder
;;     :predicate (lambda () lsp-mode)
;;     :error-explainer (lambda (e)
;;                        (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
;;                               (lsp-cpp-flycheck-clang-tidy-error-explainer e))
;;                              (t (flycheck-error-message e)))))

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
			  ((derived-mode-p 'image-mode) (buffer-file-name))))))
    (when image-file
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
		 image-file)))))
    (message "Copied %s" image-file)))



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
