(require 'org-annotate-code)
(require 'org-annotate-index)
(require 'f)

(defun gm/org-annotate-insert-file-heading (filename)
  "Insert heading NAME and a file link to it as CUSTOM_ID."
  (interactive "f")
  (let* ((filename (file-relative-name filename))
	 (annotation (org-annotate-index-make-annotation-from-filename filename)))
    (save-excursion
      (org-back-to-heading t)
      (org-annotate-code-create-subannotation annotation))))

(defun gm/org-annotate-insert-file-headings (directory)
  (interactive "D")
  (save-excursion
    (org-back-to-heading t)
    (f-entries (or directory default-directory) 'gm/org-annotate-insert-file-heading nil)))
