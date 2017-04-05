;;; imap-search.el --- Searching imap using NNIR

;; Copyright (C) 2017 George Moutsopoulos
;; Version: 1.0
;; Author: George Moutsopoulos
;; inspired by gm-nnir.el by raman
;; Package-Requires: ((nnir))
;; Keywords: GMail, IMap, gnus

;;; Commentary:

;; Complete imap search per rfc3501 plus the option to search via X-GM_RAW for GMail.
;; The novelty of this package is, it provides completing methods for the query string.
;; There are currently 2x2 multisearch functions: one for imap and one for gmail,
;; times one for a complete query multisearch input on the minibuffer or an incremental query builder.

;;; Code:

;; bugs
;; 1. imap search default folders does not work for yahoo
;; 2. unicode search
;; http://ding.gnus.narkive.com/GxzTqTkT/nnir-imap-with-gmail-query-encoding

;;
;; requires
;;
(require 'cl)
(require 'nnir)
(add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW")) 
(define-key crm-local-completion-map (kbd "SPC") 'self-insert-command)

;; this list is extracted from the IMap RFC 3501
(defvar gm-nnir-imap-search-criteria
  '( ("BCC" "<string>")
     ("BEFORE" "<date>")
     ("BODY" "<string>")
     ("CC" "<string>")
     ("FROM" "<string>")
     ("HEADER" "<field-name>" "<string>")
     ("KEYWORD" "<flag>")
     ("LARGER" "<n>")
     ("NOT" "<search-key>")
     ("ON" "<date>")
     ("SENTBEFORE" "<date>")
     ("SENTON" "<date>")
     ("SENTSINCE" "<date>")
     ("SINCE" "<date>")
     ("SMALLER" "<n>")
     ("SUBJECT" "<string>")
     ("TEXT" "<string>")
     ("TO" "<string>")
     ("X-GM-RAW" "<string>")
     ("DELETED")
     ("DRAFT")
     ("FLAGGED")
     ("NEW")
     ("RECENT")
     ("UNANSWERED")
     ("UNDELETED")
     ("unseen")
    )
  "IMap search criteria with argument specs.")

;; this list was found by https://support.google.com/mail/answer/7190?hl=en
(defvar gm-nnir-gmail-search-criteria
  '(
    ("From:" "<string>")
    ("To:" "<string>")
    ("Subject:" "<string>")
    ("OR")
    ("AROUND" "<int>" "<string>")
    ("label:" "<string>")
    ("has:attachment")
    ("list:" "(email)")
    ("filename:" "<string>")
    ("in:" "(folder)")
    ("is:important")
    ("is:starred")
    ("is:unread")
    ("is:read")
    ("has:" "<yellow-star>")
    ("cc:" "<string>")
    ("bcc:" "<string>")
    ("after:" "<YYYY/MM/DD>")
    ("before:" "<YYYY/MM/DD>")
    ("older:" "<YYYY/MM/DD>")
    ("newer:" "<YYYY/MM/DD>")
    ("older_than:" "<int d/m/y>")
    ("newer_than:" "<int d/m/y>")
    ("is:chat")
    ("size:" "<bytes>")
    ("larger:" "<bytes>")
    ("smaller:" "<bytes>")
    ("Rfc822msgid:")
    ("has:userlabel")
    ("has:nouserlabel")
    )
  "gmail search criteria")

(defvar imap-search-history nil "Search history across all searches.")
(defvar gmail-search-history nil "Search history across all searches.")
(defvar imap-search-default-groups nil "Default groups to search. Usually all inboxes and sent folders.")
(defvar gmail-search-default-groups nil "Where to make the default gmail search. Usuall GMail's All mail.")

(defun imap-search-read-clause (prompt)
  "Read one IMap search clause with smart prompts."
  (let*
      ((criteria gm-nnir-imap-search-criteria)
       (completion-ignore-case t)
       ;; require-match in criteria
       (key (completing-read prompt criteria nil 'require-match nil nil nil nil))
       (haskey (assoc key criteria))
       (valuetype (cdr (assoc key criteria)))
       (args
        (if valuetype
          (read-from-minibuffer (concat (mapconcat #'identity valuetype " ") ": " ) nil nil nil nil nil t) "")))
    (when (not (equal key "")) (format "%s %s" key args))))

(defun gmail-search-read-clause (prompt)
  "Read one IMap search clause with smart prompts."
  (let*
      ((criteria gm-nnir-gmail-search-criteria)
       (completion-ignore-case t)
       ;; require-match in criteria
       (key (completing-read prompt criteria nil 'require-match nil nil nil nil))
       (haskey (assoc key criteria))
       (valuetype (cdr (assoc key criteria)))
       (args
        (if valuetype
          (read-from-minibuffer (concat (mapconcat #'identity valuetype " ") ": " ) nil nil nil nil nil t) "")))
        (when (not (equal key "")) (format "%s %s" key args))))


(defun imap-multisearch-read-query ()
  "Return query built from a set of clauses."
  (let* (
	(criteria gm-nnir-imap-search-criteria)
        (clause (imap-search-read-clause "Search: "))
	(query clause))
    (while clause
      (setq clause (imap-search-read-clause (concat "Search: " query " ")))
      (if clause (setq query (concat query " " clause)))
      )
    query))
    
(defun gmail-multisearch-read-query ()
  "Return query built from a set of clauses."
  (let* ((criteria gm-nnir-gmail-search-criteria)
        (clause (gmail-search-read-clause "Search: "))
	(query clause))
    (while clause
      (setq clause (gmail-search-read-clause (concat "Search: " query " ")))
      (if clause (setq query (concat query " " clause)))
      )
    query))

(defun nnir-imap-multisearch (arg)
  "Perform an IMap multisearch using the minibuffer once.

Called with a prefix, perform on default folders if known. Beware to put terms in quotations if needed. This function essentially brings autocompletion in a minibuffer multiple read."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (crm-separator "[ ]+")
	   (querylist (completing-read-multiple "Search: " gm-nnir-imap-search-criteria nil nil nil 'imap-search-history))
	   (q (mapconcat 'identity querylist " "))
	   (nnir-imap-default-search-key "imap")) 
    (cond
     ((not arg) (gnus-group-make-nnir-group nil `((nnir-query-spec (query ,q)))))
     (imap-search-default-groups
        (gnus-group-make-nnir-group nil `((nnir-query-spec (query ,q)) (nnir-group-spec ,@gmail-search-default-groups))))
     (t (error "I do not know default imap folders"))
     )))

(defun nnir-gmail-multisearch (arg)
  "Use GMail multi-search syntax.

Search current line provided it belongs to gmail and there was no prefix, otherwise search GMail default search groups, usually All Mail, if known. This function essentially brings autocompletion in a minibuffer multiple read."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (crm-separator "[ ]+")
	   (querylist (completing-read-multiple "Search: " gm-nnir-gmail-search-criteria nil nil nil 'gmail-search-history))
	   (q (mapconcat 'identity querylist " "))
	   (nnir-imap-default-search-key "gmail")
	   )
    (cond
     ((and (string-match-p "gmail" (gnus-group-group-name)) (not arg)) ;; search current line (group)
      (gnus-group-make-nnir-group nil
       `((nnir-query-spec (query ,@q)))))
     (gmail-search-default-groups ;; Search All Mail
      (gnus-group-make-nnir-group nil
       `((nnir-query-spec (query ,@q))
         (nnir-group-spec ,@gmail-search-default-groups))))
     (t (error "Could not guess gmail folder")))))
      
(defun nnir-imap-multisearch-incremental (arg)
  "Multiple criteria IMap search. It searches incrementally and thus the query is strongly checked.

Search current line provided there was no prefix, otherwise search default search groups if known. This function essentially brings autocompletion."
  (interactive "P")
  (let* ((nnir-imap-default-search-key "imap")
	 (q (imap-multisearch-read-query)))
    (add-to-list 'imap-search-history q)
    (cond
     ((not arg)
     (gnus-group-make-nnir-group nil
				 `((nnir-query-spec (query ,q)))))
    (imap-search-default-groups
     (gnus-group-make-nnir-group nil
				 `((nnir-query-spec (query ,q))
				   (nnir-group-spec ,@imap-search-default-groups))))
    (t (error "I could not guess default folders to search.")))))


(defun nnir-gmail-multisearch-incremental (arg)
  "Multiple criteria IMap search. It searches incrementally and thus the query is strongly checked.

Search current line provided there was no prefix, otherwise search default search groups if known. This function essentially brings autocompletion."
  (interactive "P")
  (let* ((nnir-imap-default-search-key "gmail")
        (q (gmail-multisearch-read-query))
	(is-gmail-group (string-match-p "gmail" (gnus-group-group-name)))
	)
    (add-to-list 'gmail-search-history q)
    (cond
     ((and (not arg) is-gmail-group)
     (gnus-group-make-nnir-group nil
				 `((nnir-query-spec (query ,@q)))))
    (gmail-search-default-groups
     (gnus-group-make-nnir-group nil
				 `((nnir-query-spec (query ,@q))
				   (nnir-group-spec ,@ gmail-search-default-groups))))
    (t (error "I could not guess gmail folder to search.")))))

(provide 'imap-search)

;;; local variables:
;;; byte-compile-dynamic: nil
;;; imap-search.el ends here
