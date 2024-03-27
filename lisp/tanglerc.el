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
          (search-forward search-comment)))
    (message "Cannot jump to tangled file because point is not at org src block.")))

(defun gm/tangle-and-goto-block ()
  "Goes to the tangled file at the source block."
  (interactive)
  (let ((current-prefix-arg 8))
    (call-interactively 'org-babel-tangle))
  (gm/goto-tangled-block))

(defun gm/detangle-and-goto-block ()
  "Detangle and go to block at point.

Note sure why this was written: all languages must be the same in org file."
  (interactive)
  (let ((org-src-preserve-indentation t))
    (org-babel-detangle))
  (org-babel-tangle-jump-to-org))
