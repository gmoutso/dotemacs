(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; ;; esc-esc-esc annoying
(setq-default buffer-quit-function
	      #'(lambda () (message "Are you trying to quit?")))
(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(defun gm/get-filename (&optional filename trailing-slash)
  "Get filename. If in dired, return current line, else ask."
  (or filename
      (if (derived-mode-p 'dired-mode)
	  (if (and trailing-slash
		   (member (dired-get-filename 'verbatim t) '("." "..")))
	      (file-name-as-directory (dired-get-filename nil t))
	    (dired-get-filename nil t)))
      (read-file-name "file :")))

;;
;; using eaf in dired
;;
;; note eaf requires epc python module so conda activate base!
(defvar gm/dired-open-xlsx-use-shr t "Either shr (t) or eaf (nil). How to open an xlsx file from dired.")
(defun gm/dired-open-in-emacs ()
  (interactive)
  (let* ((filename (dired-get-filename))
	 (extension (file-name-extension filename)))
    (cond ((string-equal extension "xlsx") (if gm/dired-open-xlsx-use-shr (gm/shr-open-xlsx filename)
					     (gm/eaf-open-xlsx filename)))
	  ((string-equal extension "ipynb") (gm/shr-open-ipynb filename))
	  (t (eaf-open filename)))))
(general-def dired-mode-map
  "e" 'gm/dired-open-in-emacs)
;;
;; open xlsx files in emacs as html
;;
(defun gm/eaf-open-xlsx (&optional filename)
  (interactive)
  (let* ((filename (or filename (dired-get-filename)))
	 (temp-file (make-temp-file filename nil ".html")))
    (with-temp-buffer
      (gm/xlsx-to-html-string filename (current-buffer))
      (write-region nil nil temp-file))
    (eaf-open temp-file)
    (with-current-buffer (file-name-nondirectory temp-file)
      (make-local-variable 'kill-buffer-hook)
      (setq-local browse-url-temp-file-name temp-file)
      (add-hook 'kill-buffer-hook 'browse-url-delete-temp-file)
      (rename-buffer filename t))))
(defun gm/xlsx-to-html-string (filename &optional output-buffer)
  (let* ((default-directory (file-name-directory filename))
	 (output-buffer (or output-buffer "*xlsx-to-html-output*"))
	 (command (concat
		  "xlsxname=" filename "\n"
		  "htmlname=${xlsxname%.xlsx}.html\n"
		  "libreoffice --headless --convert-to html $xlsxname 2>/dev/null >/dev/null\n"
		  "cat $htmlname\n"
		  "rm $htmlname"))
	 )
    (shell-command command output-buffer "*xlsx-to-html-errors*")))
(defun gm/shr-open-xlsx (&optional filename)
  "Open FILENAME as an html file."
  (interactive)
  (let* ((filename (gm/get-filename filename))
	 (shortname (file-name-nondirectory filename)))
    (with-temp-buffer
      (gm/xlsx-to-html-string filename (current-buffer))
      (shr-render-buffer (current-buffer)))
    (with-current-buffer "*html*"
      (rename-buffer shortname 'unique)
      (read-only-mode t))
    shortname))
(defun gm/org-open-xlsx (&optional filename)
  "Open FILENAME as an org file."
  (interactive)
  (let* ((filename (expand-file-name (gm/get-filename filename)))
	 (shortname (concat (file-name-base filename) ".org"))
	 (command (format "python ~/app/scripts/xlsx2org.py %s" filename)))
    (shell-command command shortname)
    (with-current-buffer shortname
      (org-mode))))

;; (use-package dired-x
;;   :custom
;;   (dired-omit-files "\\`[.]?[#~]")
;;   )

(defun gm/dired-insert-subdirs-using-glob (&optional glob)
  "In a dired buffer insert subdirs using a GLOB eg `*/output'.
"
  (interactive nil 'dired-mode)
  (let* ((glob (or glob (read-string "glob: ")))
	 (paths (f-glob glob))
	 (subdirs (seq-filter 'file-directory-p paths)))
    (dolist (subdir subdirs)
      (dired-maybe-insert-subdir subdir))
    ))
