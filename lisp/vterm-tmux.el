;;; vterm-tmux.el --- Connect to TMux sessions anywhere  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author: Oliver J. Mead <olivermead@olivermead.xyz>
;; Keywords: terminals, convenience
;; Version: 0.4.0
;; Package-Requires (vterm multi-vterm)
;; URL: https://github.com/OliverMead/vterm-tmux

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An extension of `multi-vterm.el', providing persistent terminal access
;; through TMux (optionally over SSH using TRAMP).

;;; Dependencies
;; `vterm'
;; `multi-vterm'

;;; Commands:
;; This package provides /two/ commands:
;;    `vterm-tmux' Attach to a tmux session.
;; The raw prefix argument will indicate that the `default-directory' must
;; be set before attaching to the session. This operation supports
;; TRAMP paths.
;;    `vterm-tmux-project' Attach to a project-local (project.el) tmux session.
;; The session name will be based on the project root. The buffer name will be
;; based on the project root and the current host.
;; (Both should be unique to the project)

;;; Util Functions:
;; This package provides:
;;    `vterm-tmux-default-binding' Which adds `vterm-tmux-project' to `project-prefix-map'.
;; Calling this function will bind '<project-prefix> t' to call `vterm-tmux-project'.

;;; Custom Variables:
;; This package may be customised through the following variables:
;; `vterm-tmux-default-session' The default name for a tmux session
;; `vterm-tmux-default-hostname' The default (host)name for local connections
;;     (this does not have to be valid on a network, it is only used for the buffer name)
;; `vterm-tmux-connection-method' The tmux command used to connect to the session
;; `vterm-tmux-env' A list of environment variables to explicitly pass to the shell
;; `vterm-tmux-buffer-name-format' Function of HOSTNAME and SESSION to return the string to be
;;     used as the buffer name for terminals created by this package.

;;; Acknowledgements:
;; Todd Goodall <tgoodall@utexas.edu>,
;; For inspiration in the form of (the seemingly defunct) multi-tmux.el
;; The good people at the Emacs IRC channel,
;; for their encouragement.

;;; Code:

(require 'vterm)
(require 'multi-vterm)

(defgroup vterm-tmux nil
  "Connect to TMux sessions anywhere."
  :group 'multi-vterm)

(defcustom vterm-tmux-default-session "emacs-session"
  "The default name for a tmux session created (or opened) with multi-tmux."
  :type 'string
  :group 'vterm-tmux)

(defcustom vterm-tmux-default-hostname "localhost"
  "The default (host)name for local connections.
This does not need to be a valid hostname on your network."
  :type 'string
  :group 'vterm-tmux)

(defcustom vterm-tmux-connection-method "new -As"
  "The tmux command used to connect to the session.
The default will dynamically create or connect to [session name]."
  :type 'string
  :group 'vterm-tmux)

(defcustom vterm-tmux-env '("TERM=xterm-256color")
  "A list of environment variables to explicitly pass to the shell.
\(multi-hopping may lose some environment)"
  :type 'list
  :group 'vterm-tmux)

(defun vterm-tmux-default-buffer-name-format (hostname session)
  "Default buffer name formatter for `vterm-tmux'.
HOSTNAME: the hostname of the buffer's TMux session.
SESSION: the name of the buffer's TMux session."
  (format "%s-tmux-%s" hostname session))

(defcustom vterm-tmux-buffer-name-format
  #'vterm-tmux-default-buffer-name-format
  "Function of HOSTNAME, SESSION to generate the buffer name of each session."
  :type 'symbol
  :group 'vterm-tmux)

(defmacro vterm-tmux-env ()
  "Macro to create a shell's list of environment variables from variable `vterm-tmux-env'."
  `(mapconcat 'identity vterm-tmux-env " "))

(defun tmux-session-split (session-list)
  "Parse SESSION-LIST (tmux list-sessions output) for session names."
  (cl-loop for line in (split-string session-list "\n" t)
           collect (car (split-string line ":" t))))

(defun vterm-tmux-list-sessions ()
  "List (tmux) sessions running, respecting `default-directory'."
  (let ((sessions (shell-command-to-string "tmux list-sessions")))
    (if (string-match-p (regexp-quote "no server running on") sessions)
        nil
      (tmux-session-split sessions))))

(defun vterm-tmux-get (term session)
  "Attach to a (possibly remote) tmux session.
TERM indicates the name for the new terminal
SESSION indicates the tmux session to attach to"
  (let ((vterm-buffer-name-string (concat "*" term "*"))
        (tmux-command (concat "( exec </dev/tty; exec <&1;"
                              (vterm-tmux-env) " tmux "
                              vterm-tmux-connection-method " "
                              session " )")))
    (if (get-buffer vterm-buffer-name-string)
        (switch-to-buffer vterm-buffer-name-string)
      (with-temp-buffer
        (let* ((vterm-shell (concat vterm-shell " -c \'" tmux-command "\'"))
               )
          (message vterm-shell)
          (vterm vterm-buffer-name-string)
          (with-current-buffer vterm-buffer-name-string
            (if (ignore-errors (file-remote-p default-directory)) ;; temporary fix
                (vterm-send-string (concat tmux-command "\n")))
            (multi-vterm-internal))
          vterm-buffer-name-string)))))

(defun vterm-tmux (&optional dir session-opt bufname)
  "(Interactively) attach to tmux session SESSION-OPT in directory DIR.
The raw prefix argument indicates that an alternative dir should be used,
conforming to TRAMP file-name syntax (including multi-hop)"
  (interactive
   (list (setq host-inp
               (if current-prefix-arg
                   (read-directory-name
                    "In Directory: ")
                 nil))
         (let*
             ((default-directory (or host-inp default-directory))
              (coll (vterm-tmux-list-sessions)))
           (setq session-inp
                 (completing-read
                  "Session: " coll nil nil nil nil
                  vterm-tmux-default-session)))
         nil))

  (let* ((default-directory (file-name-directory (or dir default-directory)))
         (filename-tramp (when (tramp-tramp-file-p default-directory)
                           (tramp-dissect-file-name default-directory)))
         (hostname (if filename-tramp
                       (tramp-file-name-host filename-tramp)
                     vterm-tmux-default-hostname))
         (session (or session-opt vterm-tmux-default-session))
         (name (or bufname (funcall vterm-tmux-buffer-name-format
                                    hostname session))))
    (vterm-tmux-get name session)))

(defun vterm-tmux-project ()
  "Open vterm-tmux for the current project at the project root.
The buffer name will be unique to the project."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (vterm-tmux nil (concat vterm-tmux-default-session default-directory))))

(defun vterm-tmux-default-binding ()
  "Bind 't' in `project-prefix-map' to `vterm-tmux-project'."
  (define-key project-prefix-map (kbd "t") 'vterm-tmux-project))

(provide 'vterm-tmux)
;;; vterm-tmux.el ends here
