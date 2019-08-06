(add-to-list 'org-src-lang-modes '("bein" . python))
(defun org-connect-to-notebook-buffer ()
  (interactive)
  (let* ((buffer-or-name (completing-read "Notebook buffer to connect: "
					  (ein:notebook-opened-buffer-names)))
	 (notebook
          (buffer-local-value 'ein:%notebook% (get-buffer buffer-or-name)))
	 (buffer (current-buffer))
	 (connection (ein:connect-setup notebook buffer))
	 )
    (setq ein:%connect%
          (ein:connect :notebook notebook :buffer buffer)))
  )

(defun org-babel-execute:bein (body params)
  "Execute a block of python code with org-babel by way of
emacs-ipython-notebook's facilities for communicating with
jupyter kernels. 

Org-mode file must have been connected to existing notebook buffer.

This function is called by `org-babel-execute-src-block'"
  (if (null ein:%connect%)
      (org-connect-to-notebook-buffer))
  (let* ((processed-params (org-babel-process-params params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (assoc :result-type processed-params)))
         ;; expand the body with `org-babel-expand-body:template'
         (full-body (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                   params (org-babel-variable-assignments:python params))))
    (ein:shared-output-eval-string full-body)
    (let ((cell (ein:shared-output-get-cell)))
      (ein:wait-until #'(lambda ()
                          (null (slot-value cell 'running)))
                      nil ein:org-execute-timeout)
      (if (and (slot-boundp cell 'traceback)
               (slot-value cell 'traceback))
          (ansi-color-apply (apply #'concat (mapcar #'(lambda (s)
                                                        (format "%s\n" s))
                                                    (slot-value cell 'traceback))))
        (org-babel-ein-process-outputs (slot-value cell 'outputs) processed-params)))))
(provide 'ob-bein)
