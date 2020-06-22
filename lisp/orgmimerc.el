;; allow to send emails from org-mode
(use-package org-mime)
(setq org-mime-library 'semi)  ; mml for gnus, semi for wanderlust
(setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil
				  :with-sub-superscript nil))
(setq org-mime-default-header "#+OPTIONS: latex:t toc:nil ^:nil\n")
(defun org-mime-save ()
    "org htmlize and save to drafts"
    (interactive)
    (org-mime-org-buffer-htmlize)
    (wl-draft-save-and-exit)
)


(require 'wl)
(require 'mime-edit)

;; doesn't work with newer org? But it should have been fixed anyway.
;; (defun my-mime-insert-attachments (files)
;;   (mapc #'mime-edit-insert-file files)
;;   )
;; (defun org-mime-compose-advice-after (body file &rest args)
;;   "To advise org-mime-compose at the end: 
;; 1. insert file attachments."
;;   (let* ((default-directory (file-name-directory file))
;; 	(files (org-element-map
;; 		   (with-temp-buffer
;; 		     (insert body)
;; 		     (org-element-parse-buffer))
;; 		   'link
;;                  (lambda (link)
;;                    (when (string= (org-element-property :type link) "file")
;;                      (file-truename (org-element-property :path link))))))
;; 	)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (my-mime-insert-attachments files)
;;       )
;;     )
;;   )
;; (advice-add 'org-mime-compose :after #'org-mime-compose-advice-after)

;; the following can be used to nicely offset block quotes in email bodies
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

;; this was used because the text/ mime types had inline setting and nill encoding
;; instead of using this, I added the encodings in the mime-file-types variable.
;; see below
;; (defun mime-edit-insert-binary-file-advice-for-encoding (old-fun file encoding)
;;   "Advise around. encoding nil should be a text file"
;;   (if (not encoding)
;;       (mime-edit-insert-text-file file)
;;     (apply old-fun file encoding))
;;   )
;; (advice-add 'mime-edit-insert-binary-file :around #'mime-edit-insert-binary-file-advice-for-encoding)
(with-eval-after-load "mime-edit"
  (add-to-list 'mime-file-types
	       '("\\.txt$\\|\\.pln$"
		 "text" "plain"
		 (("charset" . charset) ("name" . file))
		 "8bit"
		 "attachment"
		 (("filename" . file))
		 ))
  (add-to-list 'mime-file-types
	       '("\\.py$\\|\\.pln$"
		 "text" "x-python"
		 (("charset" . charset) ("name" . file))
		 "7bit"
		 "attachment"
		 (("filename" . file))
		 ))
  (add-to-list 'mime-file-types
	       '("\\.csv$"
		 "text" "csv"
		 (("charset" . charset) ("name" . file))
		 "8bit"
		 "attachment"
		 (("filename" . file))
		 ))
  (add-to-list 'mime-file-types
	       '("\\.tex$\\|\\.latex$"
		 "text" "x-latex"
		 (("charset" . charset) ("name" . file))
		 "8bit"
		 "attachment"
		 (("filename" . file))
		 ))
  )
