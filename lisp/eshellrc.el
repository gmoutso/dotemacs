(use-package general)
;; use helm for complete
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (eshell-cmpl-initialize)
;;             (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;;             ))

;; (general-def eshell-mode-map
;;  "M-d" 'helm-eshell-history)

;; (general-define-key
;;  :keymaps 'eshell-mode-map
;;  [remap eshell-pcomplete] 'helm-esh-pcomplete)


(defun gm/bury-copy (filename destination)
  "Copy FILE to DESTINATION, create DESTINATION if necessary."
  (eshell/mkdir "-p" (file-name-directory destination))
  (copy-file filename destination))

(defun gm/buffer-string (buffer)
  "Insert BUFFER here, for use in eshell pipe."
 (with-current-buffer buffer
    (buffer-string)))

(defun gm/buffer-lines (buffer)
  "Split BUFFER content into list of lines, fir use with eshell/for."
(split-string (gm/buffer-string buffer) "\n" t))

(defun gm/tramp-to-kio (name)
  (let ((fullname (expand-file-name name)))
    (if (file-remote-p fullname)
	   (let* ((struc (tramp-dissect-file-name fullname))
		 (localname (tramp-file-name-localname struc))
		 (host (tramp-file-name-host-port struc))
		 (user (tramp-file-name-user struc))
		 (method (tramp-file-name-method struc))
		 (kioclient (cdr (assoc method '(("ssh" . "fish"))))))
	     (concat kioclient "://" user (if user "@") host localname))
      fullname)))

(defun gm/kde-open (&optional filename)
  "Works remotely and local files."
  (interactive)
  (let ((filename (or filename (dired-get-filename nil t) default-directory)))
    ;; (cmd (shell-quote-argument (concat "kde-open5 " (gm/tramp-to-kio filename)))))
    (if filename
	(make-process
         :name "kio-open" :connection-type nil :noquery t
         :buffer nil
	 :command (list  "setsid" "-w" "kde-open5" (gm/tramp-to-kio filename))
	 )
      (message "Cannot guess url to open."))))


(defun gm/konsole ()
  (interactive)
  (if (not (file-remote-p default-directory))
      (call-process "konsole" nil 0 nil "--new-tab")
    (let* ((struc (tramp-dissect-file-name default-directory))
	   (localname (tramp-file-name-localname struc))
	   (host (tramp-file-name-host-port struc))
	   (user (tramp-file-name-user struc))
	   (method (tramp-file-name-method struc)))
      (call-process "konsole" nil 0 nil "--new-tab" "-e" method (concat user (if user "@") host localname))
    )))

