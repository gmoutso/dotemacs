(defun gm/pdf-tools/scroll-other-window-up ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
        (with-selected-window wind
	  (pdf-view-scroll-up-or-next-page 2))
      (scroll-other-window 2))))
(defun gm/pdf-tools/scroll-other-window-down ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
	(with-selected-window wind
	  (pdf-view-scroll-down-or-previous-page))
	  (scroll-other-window-down 2))))
(defun gm/pdf-tools/scroll-all-up ()
  (interactive)
  (pdf-view-scroll-up-or-next-page 2)
  (gm/pdf-tools/scroll-other-window-up)
  )
(defun gm/pdf-tools/scroll-all-down ()
  (interactive)
  (pdf-view-scroll-down-or-previous-page)
  (gm/pdf-tools/scroll-other-window-down)
  )
(defvar scroll-all-pdf-mode-map (make-keymap) "scroll pdf all keymap.")
(define-key scroll-all-pdf-mode-map (kbd "M-j") 'gm/pdf-tools/scroll-all-up)
(define-key scroll-all-pdf-mode-map (kbd "M-k") 'gm/pdf-tools/scroll-all-down)
(define-minor-mode scroll-all-pdf-mode
  "A major mode for side to side pdf scrolling."
  nil " ScrAll" 'scroll-all-pdf-mode-map
  :global t
  )
;; se keys.el for priorities!

(add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)
