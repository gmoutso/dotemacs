;; inspired by
;; https://gist.github.com/ralt/56cac3dbfb9cdb6e36648b5edee2c5ee
(defun gm/make-remote-command ())
(defun eshell-exec-visual-override (&rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* (eshell-interpreter-alist
	 (original-args args)
	 (interp (eshell-find-interpreter (car args) (cdr args)))
	 (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
			    (equal (tramp-file-name-method
				    (tramp-dissect-file-name default-directory))
				   "ssh")))
	 (program (if in-ssh-tramp
		      "ssh"
		    (car interp)))
	 (args (if in-ssh-tramp
		   (let ((dir-name (tramp-dissect-file-name default-directory)))
		     (eshell-flatten-list
		      (list
		       "-t"
		       (let* ((port (tramp-file-name-port dir-name))
			      (user (tramp-file-name-user dir-name))
			      (host (tramp-file-name-host dir-name))
			      (user@host (if user (concat user "@" host) host)))
			 (if port
			     (list "-p" port user@host)
			   (list user@host)))
		       (format
			"export TERM=xterm-256color; cd %s; exec %s"
			(tramp-file-name-localname dir-name)
			(string-join
			 (append
			  (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
			  (cdr args))
			 " ")))))
		 (eshell-flatten-list
		  (eshell-stringify-list (append (cdr interp)
						 (cdr args))))))
	 (term-buf
	  (generate-new-buffer
	   (concat "*"
		   (if in-ssh-tramp
		       (format "%s %s" default-directory (string-join original-args " "))
		       (file-name-nondirectory program))
		   "*")))
	 (eshell-buf (current-buffer)))
    (save-current-buffer
      (switch-to-buffer term-buf)
      (term-mode)
      (set (make-local-variable 'term-term-name) eshell-term-name)
      (make-local-variable 'eshell-parent-buffer)
      (setq eshell-parent-buffer eshell-buf)
      (message "running term-exec %s %s %s nil %s" term-buf program program  args)
      (term-exec term-buf program program nil args)
      (let ((proc (get-buffer-process term-buf)))
	(if (and proc (eq 'run (process-status proc)))
	    (set-process-sentinel proc 'eshell-term-sentinel)
	  (error "Failed to invoke visual command")))
      (term-char-mode)
      (if eshell-escape-control-x
	  (term-set-escape-char ?\C-x))))
  nil)

;; (advice-add 'eshell-exec-visual :override 'eshell-exec-visual-override)
;; (advice-remove 'eshell-exec-visual 'eshell-exec-visual-override)
