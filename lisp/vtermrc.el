(use-package vterm
    :bind (:map vterm-mode-map ("C-y" . vterm-yank))
    :config (setq vterm-max-scrollback 100000))
(use-package multi-vterm)
(use-package vterm-tmux)
(defun gm/vterm-tmux-remote-choose-session (user+host)
  (let ((tmuxls (multi-term-tmux-sessions user+host)))
  (completing-read "session (default emacs-session): " tmuxls nil nil nil nil "emacs-session")))
(defun gm/vterm-tmux-remote-choose-userhost ()
  (completing-read "[user@]host: " '("beowulf@ted" "phil" "ted" "beowulf@phil") nil t))
(defun gm/vterm-tmux-open (&optional user@host session-name buffer-name)
  (interactive)
  (let* ((user@host (or user@host (gm/vterm-tmux-remote-choose-userhost)))
	 (session-name (or session-name
			   (gm/vterm-tmux-remote-choose-session user@host)))
	 (term-name (or buffer-name (format "*tmux-%s:%s" user@host session-name)))
	 (default-directory "~")
	 (vterm-shell (format "ssh %s -t tmux new -As %s" user@host session-name))
	 (buffer-name (or buffer-name (format "*tmux-%s:%s*" user@host session-name))))
    (vterm buffer-name)
    (message buffer-name)
    (with-current-buffer buffer-name
      (multi-vterm-internal))
    buffer-name
    )
  )

