;;; helm-org-images --- Show images in org file and jump to section

;;; Code:

(require 'helm-img)

(defun helm-org-images-matches-in-buffer (&optional buffer)
  "Get all image links and their locations."
  (let ((regex "\\[\\[file:\\(?1:[-[:alnum:]/_.]*.png\\)\\]")
	(matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (cons (match-beginning 0) (match-string-no-properties 1)) matches)))))
      matches)))

