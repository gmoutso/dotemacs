(use-package vterm
    :bind (:map vterm-mode-map ("C-y" . vterm-yank))
    :config (setq vterm-max-scrollback 100000))
(use-package multi-vterm)

(use-package vterm-tmux)
(defun gm/vterm-tmux-remote (&optional user@host session-name buffer-name)
  (interactive)
  (let* ((user@host (or user@host (read-string "[user@]host: ")))
	 (session-name (or session-name
			   (multi-term-tmux-remote-choose-session user@host)))
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

