;;; org-annotate-code.el --- Annotate code using org-capture
;; Copyright (C) 2020

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

(defun org-annotate-code-info-at-point-python ()
  (interactive)
  (let*
  ((searchmatch 
  (save-excursion
    (python-nav-beginning-of-defun)
    (search-forward-regexp "^ *\\(?:\\(?2:\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)\\)(\\|\\(?2:\\(?1:[[:alnum:]_]*\\) *=\\)\\)" (line-end-position))
    ))
   (searchname (match-string-no-properties 2))
   (name (match-string-no-properties 1))
   )
  (list :name name
    :filename (buffer-file-name)
    :module (file-name-base (buffer-file-name))
    :searchname searchname
    )))

(defun org-annotate-code-info-at-point-unknown ()
  (let* ((filename (buffer-file-name))
	 (module (file-name-nondirectory filename))
	 (lineno (line-number-at-pos))
	 (searchname (format "%s" lineno))
	 (symbol (thing-at-point 'symbol t))
	 (name (if symbol
		   (format "%s at line %s" symbol lineno)
		 (format "line %s" lineno))))
  (list :filename filename
    :module module
    :searchname searchname
    :name name
  )))

(defun org-annotate-code-search-or-create-level (heading id)
  (condition-case nil
      (org-link-search (concat "#" id))
    (error
     (goto-char (point-min))
     (org-insert-heading '(4))
     (insert heading)
     (org-set-property "CUSTOM_ID"  id)))
  )

(defun org-annotate-code-search-or-create-twolevels (heading1 link1 heading2 link2 &optional line)
  "Locate or insert org-mode heading levels 1 and 2. Return position of second level."
  (widen)
  (goto-char (point-min))
  ;; insert first
  (org-annotate-code-search-or-create-level heading1 link1)
  ;; insert second
  (org-narrow-to-subtree)
  (org-annotate-code-search-or-create-level heading2 link2)
  (org-demote)
  (org-narrow-to-subtree)
  (goto-char (point-max))
  (widen)
  (point)
  )

(defcustom org-annotate-code-info-alist '((python-mode . org-annotate-code-info-at-point-python))
  "Custom info parsers"
  :group 'org-annotate-code)

(defun org-annotate-code-predicate-mode-cons (carcon)
  "Helper function to return cdr if mode is car"
(if (derived-mode-p (car carcon)) (cdr carcon)))
(defun org-annotate-code-choose-info-function ()
  "Return approprate function from info alist according to mode"
    (cond ((cl-some 'org-annotate-code-predicate-mode-cons org-annotate-code-info-alist))
     (t 'org-annotate-code-info-at-point-unknown)))

;;;###autoload
(defun org-annotate-code-capture-finding-location (&optional org-file)
  (let* (
	 (plist (funcall (org-annotate-code-choose-info-function)))
	 (filename (plist-get plist :filename))
	 (name (plist-get plist :name))
	 (module (plist-get plist :module))
	 (searchname (plist-get plist :searchname))
	 (link1 (org-link-make-string (concat "file:" filename)))
	 (link2 (org-link-make-string (concat "file:" filename "::" searchname)))
	 (org-file (or org-file org-annotate-code-org-file))
	 (the-buffer (org-capture-target-buffer org-file))
	 )
    (set-buffer the-buffer)
    (org-annotate-code-search-or-create-twolevels module link1 name link2)
    ))


(provide 'org-annotate-code)

