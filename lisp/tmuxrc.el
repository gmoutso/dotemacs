(use-package ob-tmux
  ;; Install package automatically (optional)
  :ensure t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")	;
     (:session . "sim")	; The default tmux session to send code to
     (:socket  . nil)))		; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  (org-babel-tmux-terminal "xterm")
  (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location "/usr/bin/tmux"))

;; REMOTE_SOCKET=$(ssh remote-machine  'tmux ls -F "#{socket_path}"' | head -1)
;; echo $REMOTE_SOCKET
;; ssh remote-machine -tfN \
;;     -L ~/.tmux-local-socket-remote-machine:$REMOTE_SOCKET
;; #+BEGIN_SRC tmux :socket ~/.tmux-local-socket-remote-machine :session hello:pane1
;; echo hello from remote machine
;; #+END_SRC
