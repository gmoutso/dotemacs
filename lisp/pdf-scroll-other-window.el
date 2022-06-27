(defun gm/other-window-for-scrolling ()
  (next-window))
(defun gm/pdf-tools/scroll-other-window-up ()
  (interactive)
  (let* ((wind (gm/other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
        (with-selected-window wind
	  (pdf-view-scroll-up-or-next-page 2))
      ;; (scroll-other-window 2)
      )))
(defun gm/pdf-tools/scroll-other-window-down ()
  (interactive)
  (let* ((wind (gm/other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
	(with-selected-window wind
	  (pdf-view-scroll-down-or-previous-page))
      ;; (scroll-other-window-down 2)
      )))
(defun gm/pdf-tools/scroll-all-up ()
  (interactive)
  (ignore-errors (pdf-view-scroll-up-or-next-page 2))
  (ignore-errors (gm/pdf-tools/scroll-other-window-up))
  )
(defun gm/pdf-tools/scroll-all-down ()
  (interactive)
  (ignore-errors (pdf-view-scroll-down-or-previous-page))
  (ignore-errors (gm/pdf-tools/scroll-other-window-down))
  )
(defvar scroll-all-pdf-mode-map (make-keymap) "scroll pdf all keymap.")
(define-key scroll-all-pdf-mode-map (kbd "M-j") 'gm/pdf-tools/scroll-all-up)
(define-key scroll-all-pdf-mode-map (kbd "M-k") 'gm/pdf-tools/scroll-all-down)
(define-minor-mode scroll-all-pdf-mode
  "A major mode for side to side pdf scrolling."
  :global nil
  :lighter " ScrAll"
  :keymap scroll-all-pdf-mode-map
  )
;; see keys.el for priorities!

(add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)
(add-hook 'pdf-view-mode-hook 'scroll-all-pdf-mode)
