;; generic
(use-package vterm
  :bind (:map vterm-mode-map
	      ("C-y" . vterm-yank)
	      ("C-q" . vterm-send-next-key)
	      ("C-SPC" . vterm--self-insert)
	      )
  :config
  (setq vterm-max-scrollback 100000)
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
  :custom
  (vterm-enable-manipulate-selection-data-by-osc52 t) ;; tmux set -g set-clipboard on
  )

(use-package multi-vterm)

;; tmux configuration


;; I need to set TMPDIR in .bashrc before non-interactive exit!?
;; see also https://codeberg.org/olivermead/vterm-tmux
;; (use-package vterm-tmux)

(defconst gm/tmux-user-hosts '("beowulf@ted" "phil" "cloud-tests-gm"))

(defun gm/tmux-select-user-host ()
  (completing-read "[user@]host: " gm/tmux-user-hosts nil t))

(defun gm/vterm-make-command (cmd &optional user+host)
  "Make a command CMD with an ssh if USER+HOST is non nil"
  (if user+host (format "ssh %s -q -t \"%s\"" user+host cmd)
      (format "sh -c \"%s\"" cmd)))

(defun gm/tmux-make-command (tmux-subcmd &optional user+host)
  (gm/vterm-make-command (format "tmux %s" tmux-subcmd)
			 user+host))

(defun gm/vterm-exec (cmd &optional user@host directory buffer-name)
  (let* ((default-directory "~")
	 (full-cmd (concat (if directory (format "cd %s;" directory)) cmd))
	 (vterm-shell (gm/vterm-make-command full-cmd user@host))
	 (buffer-name (or buffer-name (concat "*" (generate-new-buffer-name cmd) "*")))
	 buffer)
    (setq buffer (vterm buffer-name))
    (with-current-buffer buffer
      (multi-vterm-internal))
    (message "started vterm buffer #<%s>" (buffer-name buffer))
  ))

(defun gm/make-user@host (&optional directory)
  (let* ((directory (or directory default-directory))
	 (host (file-remote-p directory 'host))
	 (user (file-remote-p directory 'user))
	 (user@host (if host (if user (format  "%s@%s" user host) host)))
	 )
    user@host
    ))

(defun eshell/vterm-exec (&rest cmd-args)
  (if cmd-args
      (let ((user@host (gm/make-user@host))
	    (directory (file-local-name default-directory)))
	(gm/vterm-exec (string-join cmd-args " ") user@host directory)
	)
    (message "missing args")
    ))
(defalias 'eshell/vterm-run 'eshell/vterm-exec)

(defun gm/tmux-command-to-string (cmd &optional user+host)
  (let ((default-directory "~"))
    (shell-command-to-string (gm/tmux-make-command "list-sessions" user+host))))
  
(defun gm/tmux-get-sessions (&optional user+host)
  "Taken from multi-term-tmux"
  (interactive)
  (let ((output (gm/tmux-command-to-string "list-sessions" user+host)))
    ;; tmux might need extra -t ??
    (if (string-match-p "no server running on" output)
	(error (format "No server is running on %s" user+host)))
    (cl-loop for line in (split-string output "\n" t)
	     collect (car (split-string line ":" t)))))

(defun gm/tmux-select-remote-session (user+host)
  (completing-read "session (default emacs-session): "
		     (gm/tmux-get-sessions user+host)
		     nil nil nil nil "emacs-session"))

(defun gm/tmux-make-buffer-name (user@host session-name)
  ;;(format "*tmux[%s:%s]*" user@host session-name))
  (format "*tmux[%s]*" user@host))

(defun gm/vterm-tmux-create (user@host session-name buffer-name)
  (let ((default-directory "~")
	;; might need preamble ( exec </dev/tty; exec <&1; ... ) ?
	;; see vterm-tmux-get for possible resolutions..
	(vterm-shell (gm/tmux-make-command
		      (format "new -As %s" session-name)
		      user@host)))
    (save-excursion (vterm buffer-name))
    (with-current-buffer buffer-name
      (multi-vterm-internal))))

(defun gm/vterm-tmux (&optional user@host session-name buffer-name)
  (interactive)
  (let* ((user@host (or user@host (gm/tmux-select-user-host)))
	 (session-name (or session-name
			   (gm/tmux-select-remote-session user@host)))
	 (buffer-name (or buffer-name (gm/tmux-make-buffer-name user@host session-name))))
    (if (get-buffer buffer-name)
	(message "Selecting existing tmux buffer.")
      (gm/vterm-tmux-create user@host session-name buffer-name))
    (switch-to-buffer buffer-name)
    ))

(defun gm/vterm-tmux-sync ()
  (interactive)
  (let ((tmux-escaped "printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\"\n")
	(msg "51;A$USER@$HOSTNAME:$(pwd)")
	)
  ;; tmux
  ;; printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  ;; "printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\"
  ;; screen
  ;; printf "\eP\e]%s\007\e\\" "$1"
  ;; shell
  ;; printf "\e]%s\e\\" "$1"
  ;; (vterm-insert "printf \"\\e]\\e\\\\\"")
  ;; \ePtmux;\e\e]%s\007\e\\
  ;; \\ePtmux;\\e\\e]%s\\007\\e\\\\
  (vterm-insert (format tmux-escaped msg))
  ))

(defun gm/vterm_printf (msg)
  (let ((ifthenelse
	 "if [ -n \"$TMUX\" ] && ([ \"${TERM%%-*}\" = \"tmux\" ] || [ \"${TERM%%-*}\" = \"screen\" ]); then
        # Tell tmux to pass the escape sequences through
        printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\" \"$1\"
    elif [ \"${TERM%%-*}\" = \"screen\" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf \"\\eP\\e]%s\\007\\e\\\\\" \"$1\"
    else
        printf \"\\e]%s\\e\\\\\" \"$1\"
    fi
"))
  (vterm-insert (replace-regexp-in-string "\\$1" msg ifthenelse))
  ))

(defun gm/vterm-sync ()
  (interactive)
  (let ((ifthenelse
	 "if [ \"$TERM\" = \"screen\" ] && [ -n \"$TMUX\" ]; then %s; else %s; fi\n")
	(tmux "printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\"")
	(notmux "printf \"\\e]%s\\e\\\\\"")
	(msg "51;A$USER@$HOSTNAME:$(pwd)")
	)
  ;; tmux
  ;; printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  ;; "printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\"
  ;; screen
  ;; printf "\eP\e]%s\007\e\\" "$1"
  ;; shell
  ;; printf "\e]%s\e\\" "$1"
  ;; (vterm-insert "printf \"\\e]\\e\\\\\"")
  ;; \ePtmux;\e\e]%s\007\e\\
  ;; \\ePtmux;\\e\\e]%s\\007\\e\\\\
  (vterm-insert (format (format ifthenelse tmux notmux) msg msg))
  ))
