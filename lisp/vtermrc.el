;; generic
(use-package vterm
  :bind (:map vterm-mode-map
	      ("C-y" . vterm-yank)
	      ("C-q" . vterm-send-next-key)
	      ("C-SPC" . vterm--self-insert)
	      )
  :config (setq vterm-max-scrollback 100000)
  :custom
  (vterm-enable-manipulate-selection-data-by-osc52 t) ;; tmux set -g set-clipboard on
  )

(use-package multi-vterm)

;; tmux configuration


;; I need to set TMPDIR in .bashrc before non-interactive exit!?
;; see also https://codeberg.org/olivermead/vterm-tmux
(use-package vterm-tmux)

(defconst gm/tmux-user-hosts '("beowulf@ted" "phil" "ted" "beowulf@phil"
			       "test-docdb" "test-migration-gm"))

(defun gm/tmux-select-user-host ()
  (completing-read "[user@]host: " gm/tmux-user-hosts nil t))

(defun gm/tmux-make-command (cmd &optional user+host)
  (let ((tmux (if user+host (format "ssh %s -q -t tmux" user+host)
		;; is extra -t needed for list-sessions?
		"tmux")))
    (format "%s %s" tmux cmd)))

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
  (format "*tmux[%s:%s]*" user@host session-name))

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
