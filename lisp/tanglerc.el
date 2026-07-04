;;; tanglerc.el --- tangle functions org version 9.4

;;; Commentary:
;; Custom configuration file.

;;; Code:

;;
;; tangle functions org version 9.4
;;
;; to be used with header arguments :tangle yes :comments yes :noweb yes

(setq org-babel-tangle-comment-format-beg
 "%% [[%link][%source-name]]")

(defun gm/org-babel-get-block-header (&optional property)
  "Returns alist of header properties of this block or specific PROPERTY.

Eg., use with PROPERTY :results or :session.
"
  (let* ((info (org-babel-get-src-block-info 'light))
	 (properties (nth 2 info)))
    (if property (cdr (assq property properties))
      properties)))

;; To be able to go to jump to the link in tangled file from a given block in org
;; we need the comment link using 'gm/org-babel-tangle-get-this-comment-link
;; most functions here try to get this (viz. getting the counter used in the link)

(defun gm/org-babel-tangle-count-this ()
  "Count source block number in section.

Note, does not give correct file search field in orglink as in the tangled file if before all headings!"
    (let ((here (point))
	  (beg (org-with-wide-buffer
		(org-with-limited-levels (or (outline-previous-heading) (point-min))))))
      (let ((case-fold-search nil))
	(count-matches "^ *#\\+begin_src" beg here))))

(defun gm/org-babel-tangle-get-this-comment-link ()
  "Extracts the org link that comments the source block in the tangled file."
  (pcase-let*
      ((counter (gm/org-babel-tangle-count-this))
       (tangled-block (org-babel-tangle-single-block counter))
       (`(,start ,file ,link ,source ,info ,body ,comment) tangled-block)
       (link-data `(("start-line" . ,(number-to-string start))
		    ("file" . ,file)
		    ("link" . ,link)
		    ("source-name" . ,source))))
    (org-fill-template
		org-babel-tangle-comment-format-beg link-data)))

(defun gm/goto-tangled-block ()
  "The opposite of `org-babel-tangle-jump-to-org'. Jumps at tangled code from org src block.

https://emacs.stackexchange.com/a/69591"
  (interactive)
  (if (org-in-src-block-p)
      (let* ((header (car (org-babel-tangle-single-block 1 'only-this-block)))
	     ;; ("test.py" ("python" 9 "test.org" "file:test.org::*a" "a:1" properties code nil))
	     ;; if tangle is no then car will be nil!
	     (tangle (car header))
	     (rest (cadr header))
             (lang (car rest))
             (org-buffer (nth 2 rest))
             (org-id (nth 3 rest))
             (source-name (nth 4 rest))
             (search-comment (gm/org-babel-tangle-get-this-comment-link))
             (file (expand-file-name
                    (org-babel-effective-tangled-filename org-buffer lang tangle))))
        (if (not (file-exists-p file))
            (message "File does not exist. 'org-babel-tangle' first to create file.")
          (find-file file)
          (beginning-of-buffer)
          (search-forward search-comment)
	  (forward-line)))
    (message "Cannot jump to tangled file because point is not at org src block.")))

(defun gm/tangle-and-goto-block ()
  "Goes to the tangled file at the source block."
  (interactive)
  (save-excursion (org-babel-tangle))
  (gm/goto-tangled-block))

(defun gm/detangle-and-goto-block ()
  "Detangle and go to block at point.

Note sure why this was written: all languages must be the same in org file."
  (interactive)
  (let ((org-src-preserve-indentation t))
    (org-babel-detangle))
  (org-babel-tangle-jump-to-org))


(defun org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org mode file."
  (interactive)
  (let ((mid (point))
	start body-start end target-buffer target-char link block-name body)
    (save-window-excursion
      (save-excursion
	(while (and (re-search-backward org-link-bracket-re nil t)
		    (not ; ever wider searches until matching block comments
		     (and (setq start (line-beginning-position))
			  (setq body-start (line-beginning-position 2))
			  (setq link (match-string 0))
			  (setq block-name (match-string 2))
			  (save-excursion
			    (save-match-data
			      (re-search-forward
			       (concat " " (regexp-quote block-name)
				       " ends here")
			       nil t)
			      (setq end (line-beginning-position))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "Not in tangled code"))
        (setq body (buffer-substring body-start end)))
      ;; Go to the beginning of the relative block in Org file.
      ;; Explicitly allow fuzzy search even if user customized
      ;; otherwise.
      (let (org-link-search-must-match-exact-headline)
        (org-link-open-from-string link))
      (setq target-buffer (current-buffer))
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (let ((n (string-to-number (match-string 1 block-name))))
	    (if (org-before-first-heading-p) (goto-char (point-min))
	      (org-back-to-heading t))
	    ;; Do not skip the first block if it begins at point min.
	    (cond ((or (org-at-heading-p)
		       (not (org-element-type-p (org-element-at-point) 'src-block)))
		   (org-babel-next-src-block n))
		  ((= n 1))
		  (t (org-babel-next-src-block (1- n)))))
        (org-babel-goto-named-src-block block-name))
      (goto-char (org-babel-where-is-src-block-head))
      (forward-line 1)
      ;; Try to preserve location of point within the source code in
      ;; tangled code file.
      (let* ((el (org-element-at-point))
	     (org-block-size (length (org-element-property :value el)))
             (offset (min org-block-size (- mid body-start)))
             )
	(forward-char offset))
      (setq target-char (point)))
    (org-src-switch-to-buffer target-buffer t)
    (goto-char target-char)
    body))

(provide 'tanglerc)
;;; tanglerc.el ends here