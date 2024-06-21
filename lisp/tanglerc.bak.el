;;
;; tangle functions org version 9.4
;;
;; to be used with header arguments :tangle yes :comments yes :noweb yes

(defun gm/org-babel-tangle-count-this ()
  "Count source block number in section"
    (let ((here (point))
	  (beg (org-with-wide-buffer
		(org-with-limited-levels (or (outline-previous-heading) (point-min))))))
      (let ((case-fold-search nil))
	(count-matches "^ *#\\+begin_src" beg here))))

(defun gm/org-babel-get-block-header (&optional property)
  "Returns alist of header properties of this block or specific PROPERTY.

Eg., use with PROPERTY :results or :session.
"
  (let* ((info (org-babel-get-src-block-info 'light))
	 (properties (nth 2 info)))
    (if property (cdr (assq property properties))
      properties)))

(defun gm/org-babel-tangle-collect-this-block ()
  "Tangle this block with correct counter.

Note, does not give correct file search field in orglink as in the tangled file before all headings."
   (let ((tangle (gm/org-babel-get-block-header :tangle))
	 (counter (gm/org-babel-tangle-count-this)))
     (unless (string= tangle "no")
       (org-babel-tangle-single-block counter))))

(defun gm/org-babel-tangle-get-this-comment-link ()
  "Extracts the org link that comments the source block in the tangled file."
  (pcase-let*
      ((`(,start ,file ,link ,source ,info ,body ,comment) (gm/org-babel-tangle-collect-this-block))
       (comments (cdr (assq :comments info)))
       (link? (or (string= comments "both") (string= comments "link")
		  (string= comments "yes") (string= comments "noweb")))
       (link-data `(("start-line" . ,(number-to-string start))
		    ("file" . ,file)
		    ("link" . ,link)
		    ("source-name" . ,source))))
    (when link?
	       (org-fill-template
		org-babel-tangle-comment-format-beg link-data))))

(defun gm/tangle-and-goto-block ()
  "Goes to the tangled file at the source block. Will use the orglink description as a search only."
  (interactive)
  (let ((filename (expand-file-name (nth 0 (org-babel-tangle))))
	(orglink (gm/org-babel-tangle-get-this-comment-link)))
    (string-match "\\[\*.*]" orglink)
    (org-open-file filename t nil (match-string-no-properties 0 orglink))))

(defun gm/detangle-and-goto-block ()
  "Detangle and go to block at point. Unfortunately all languages must be the same in org file."
  (interactive)
  (org-babel-detangle)
  (org-babel-tangle-jump-to-org))
