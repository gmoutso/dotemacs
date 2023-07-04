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
(defun gm/term-define-set-eterm-dir ()
  ;; https://github.com/cuspymd/tramp-term.el
  (term-send-raw-string "function set-eterm-dir {
    echo -e \"\\033AnSiTu\" \"$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"$HOSTNAME\"
    history -a
}
"))
(defun gm/term-set-eterm-dir ()
  (interactive)
 (gm/term-define-set-eterm-dir)
 (term-send-raw-string "set-eterm-dir
"))
(defun gm/term-tmux-dir ()
  (interactive)
  (term-send-raw-string
   "printf \"\\033Ptmux;\\033\\033AnSiTh %s\\n\\033\\033AnSiTu %s\\n\\033\\033AnSiTc %s\\n\\033\\\\\" $HOSTNAME $USER $PWD"))
(defun gm/tramp-term-initialize ()
  "Send bash commands to set up tramp integration for HOSTNAME."
  (gm/term-define-set-eterm-dir)
    (term-send-raw-string "PROMPT_COMMAND=\"${PROMPT_COMMAND:+$PROMPT_COMMAND ;} set-eterm-dir\"
clear
"))



