;;; org-annotate-code.el --- Annotate code using org-capture

;; Copyright (C) 2029

;; Author: George Moutsopoulos <gmoutso@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-mode))
;; Keywords: annotate, capture, code, comments

;;; Commentary:

;; This package provides a function-finding-location for org-mode capture.
;; In a source code buffer, capturing will insert two headings if not already there.
;; The top-level heading is a link to the file and the second a link to the function definition at point.
;; A line can be inserted with the line or word at point.

(require 'org)

(defgroup org-annotate-code nil "Org-annotate-code")

(defcustom org-annotate-code-org-file "~/Documents/org/code-annotations.org"
  "The org file where annotations are saved"
  :type 'string
  :group 'org-annotate-code)

(defun org-annotate-code-search-or-create-level (heading id)
       (interactive)
       (condition-case nil
         (org-link-search (concat "#" id))
	 (error
	  (goto-char (point-min))
	  (org-insert-heading '(4))
	  (insert heading)
	  (org-set-property "CUSTOM_ID"  id)))
       )

(defun org-annotate-code-info-at-point-python ()
  (interactive)
  (let*
  ((searchmatch 
  (save-excursion
    (python-nav-beginning-of-defun)
    (search-forward-regexp "^ *\\(def\\) \\([[:alnum:]_]*\\)(" (line-end-position))
    ))
   (type (match-string-no-properties 1))
   (name (match-string-no-properties 2))
   )
  '(:type type
	  :name name
	  :filename (buffer-file-name)
	  :module (file-name-base (buffer-file-name))
	  :searchname (concat type " " name)
	  ;; :word (substring-no-properties (thing-at-point 'word))
	  ;; :sentence substring-no-properties (thing-at-point 'sentence)
	  )
  )
  )

(defun org-annotate-code-info-at-point-unknown ()
  (interactive)
  '(:filename (buffer-file-name)
    :module (file-name-base (buffer-file-name))
    :searchname (line-number-at-pos)
    :name (substring-no-properties (thing-at-point 'word))
    :line (substring-no-properties (thing-at-point 'sentence)))
  )

(defun org-annotate-code-goto-or-insert-twolevels (path L1link L1description L2link L2description &optional line)
  "Locate or insert org-mode heading levels 1 and 2. Return position of second level."
  (let
      (headline1 (org-link-make-string L1link L1description))
      (headline2 (org-link-make-string L2link L2description)
   (with-current-buffer (find-file path)
   ;(set-buffer path)
   (unless (derived-mode-p 'org-mode)
     (org-display-warning
      (format "Capture requirement: switching buffer %S to Org mode"
	      (current-buffer)))
     (org-mode))
   (widen)
   (goto-char (point-min))
   ;; insert first
   (if (re-search-forward (format org-complex-heading-regexp-format
					(regexp-quote headline1)) ;; get line of any level
				nil t)
       (beginning-of-line)
     ;; (goto-char (point-min))
     ;; (org-insert-heading 4 nil t)
     ;; (insert headline1))
   (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     (insert "* " headline1)  ;; default is level1
     (beginning-of-line))
   ;; insert second
   (org-narrow-to-subtree)
   (if (re-search-forward (format org-complex-heading-regexp-format
					(regexp-quote headline2))
			  nil t)
    (end-of-line)
    (end-of-line)
    (org-insert-heading '(4))
    (org-do-demote)
    (insert headline2)
     )
   (widen)
   (insert "\n")
   (if line
       (insert line "\n"))
   (point)
)))

;;;###autoload
(defun org-annotate-code-capture-finding-location ()
  (cond ((eq major-mode python-mode)
	 (let* ((plist (org-annotate-code-info-at-point-python))
		(filename (plist-get plist :filename))
		(name (plist-get plist :name))
		(type (plist-get plist :type))
		(module (plist-get plist :module))
		(searchname (plist-get plist :searchname))
		(L1link (concat "file:" filename))
		(L2link (concat "file:" filename "::" searchname))
		;; (line (plist-get plist :sentence))
		)
	   (org-annotate-code-goto-or-insert-twolevels org-annotate-code-org-file
						    L1link module L2link name))
	 )
	t (let* ((plist (org-annotate-code-info-at-point-unknown))
		(filename (plist-get plist :filename))
		(name (plist-get plist :name))
		(type (plist-get plist :type))
		(module (plist-get plist :module))
		(searchname (plist-get plist :searchname))
		(L1link (concat "file:" filename))
		(L2link (concat "file:" filename "::" searchname))
		(line (plist-get plist :type))
		(level1 (org-link-make-string (concat "file:" filename) module))
		(level2 (org-link-make-string (concat "file:" filename "::" lineno) line))
		)
		(L1link (concat "file:" filename))
		(L2link (concat "file:" filename "::" searchname))
		;; (line (plist-get plist :sentence))
		)
	   (org-annotate-code-goto-or-insert-twolevels org-annotate-code-org-file
						    L1link module L2link name line))
)

(provide 'org-annotate-code)
