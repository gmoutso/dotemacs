;; I need to set TMPDIR in .bashrc before non-interactive exit!
(load-library "multi-term-tmux")
(defun gm/multi-term-tmux-remote-open (&optional user+host session-name buffer-name)
  "Input: provide USER+HOST, SESSION-NAME."
  (interactive)
  (let* ((user+host (or user+host (gm/multi-term-tmux-remote-choose-userhost)))
	 (session-name (or session-name
			   (gm/multi-term-tmux-remote-choose-session user+host)))
	 (term-name (or buffer-name (format "tmux[%s:%s]" user+host session-name))))
    (multi-term-tmux-remote-open user+host session-name term-name)))
(defun gm/multi-term-tmux-remote-choose-session (user+host)
  (let ((tmuxls (multi-term-tmux-sessions user+host)))
  (completing-read "session (default emacs-session): " tmuxls nil nil nil nil "emacs-session")))
(defun gm/multi-term-tmux-remote-choose-userhost ()
  (completing-read "[user@]host: " '("beowulf@ted" "phil" "ted" "beowulf@phil"
				     "test-docdb" "gm_aws_migration") nil t))
(defalias 'gm/tmux-open 'gm/multi-term-tmux-remote-open)

;; https://github.com/cuspymd/tramp-term.el
(defun gm/term-tmux-set-dir ()
  (interactive)
  (term-send-raw-string
   "printf \"\\033Ptmux;\\033\\033AnSiTh %s\\n\\033\\033AnSiTu %s\\n\\033\\033AnSiTc %s\\n\\033\\\\\" $HOSTNAME $USER $PWD\n"))
(defun gm/term-set-dir ()
  (interactive)
  (term-send-raw-string
   "printf \"\\033AnSiTh %s\\n\\033AnSiTu %s\\n\\033AnSiTc %s\\n\" $HOSTNAME $USER $PWD\n"))
