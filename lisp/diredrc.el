(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; ;; esc-esc-esc annoying
(setq-default buffer-quit-function
	      #'(lambda () (message "Are you trying to quit?")))
(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

;;
;; using eaf in dired
;;
;; note eaf requires epc python module so conda activate base!
(defvar gm/dired-open-xlsx-use-shr t "Either shr (t) or eaf (nil). How to open an xlsx file from dired.")
(defun gm/dired-open-with-eaf ()
  (interactive)
  (let* ((filename (dired-get-filename))
	 (extension (file-name-extension filename)))
    (cond ((string-equal extension "xlsx") (if gm/dired-open-xlsx-use-shr (gm/shr-open-xlsx filename)
					     (gm/eaf-open-xlsx filename)))
	  (t (eaf-open filename)))))
(general-def dired-mode-map
  "e" 'gm/dired-open-with-eaf)
;;
;; open xlsx files in emacs as html
;;
(defun gm/xlsx-to-html-string (filename &optional output-buffer)
  (let ((command (concat
		  "xlsxname=" filename "\n"
		  "htmlname=${xlsxname%.xlsx}.html\n"
		  "libreoffice --headless --convert-to html $xlsxname 2>/dev/null >/dev/null\n"
		  "cat $htmlname\n"
		  "rm $htmlname")))
    (shell-command command output-buffer)))
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
(defun gm/shr-open-xlsx (&optional filename)
  (interactive)
  (let ((filename (or filename (dired-get-filename))))
  (with-temp-buffer
      (gm/xlsx-to-html-string filename (current-buffer))
      (shr-render-buffer (current-buffer)))))


