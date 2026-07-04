;;; eshellrc.el --- configuration for shells

;;; Commentary:
;; Custom configuration for eshell and shell modes.

;;; Code:

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

(defun eshell/remote-cd (&optional directory)
  "cd into remote DIRECTORY in eshell as if in remote shell"
  (if (file-remote-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
        (eshell/cd (tramp-make-tramp-file-name
                    (tramp-file-name-method v)
                    (tramp-file-name-user v)
		    'nil
                    (tramp-file-name-host v)
		    'nil
                    (or directory "")
		    (tramp-file-name-hop v)
		    )))
    (eshell/cd directory)))
(defalias 'eshell/rcd 'eshell/remote-cd)
(defalias 'eshell/lcd 'eshell/remote-cd)
(defalias 'eshell/v 'eshell-exec-visual)

(provide 'eshellrc)
;;; eshellrc.el ends here
