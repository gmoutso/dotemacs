;; -*- lexical-binding: t; -*-
(use-package tmux-control
  ;; :straight (tmux-control :type git :host github :repo "csheaff/tmux-control")
  ;; :quelpa (tmux-control :fetcher github :repo "csheaff/tmux-control")
  :custom
  ;; Connection defaults for `M-x tmux-control-connect' — these are examples;
  ;; set them to your own host / socket / session.
  (tmux-control-default-host "cloud-tests-gm")          ; an SSH host alias, or nil for local
  (tmux-control-default-socket-name "default")
  (tmux-control-default-session "0"))
;; export TMUX_TMPDIR="${HOME}/.tmux-sock"
