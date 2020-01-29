(defun pdf-tools/scroll-other-window ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
        (with-selected-window wind
      (pdf-view-next-line-or-next-page 2))
      (scroll-other-window 2))))

(defun pdf-tools/scroll-other-window-down ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
    (with-selected-window wind
      (progn
        (pdf-view-previous-line-or-previous-page 2)
        (other-window 1)))
    (scroll-other-window-down 2))))

