;;; ev-tramp.el --- Rsync files between hosts with path translation -*- lexical-binding: t; -*-

;; Author: George Moutsopoulos
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.4") (s "1.12"))
;; Keywords: tools, convenience
;; URL:

;;; Commentary:

;; Provides commands for rsyncing files and directories between local and
;; remote hosts, with automatic path translation based on configurable
;; mappings.  Includes a transient UI (`ev-rsync-tree') for interactive
;; rsync operations and `ev-tramp-here' for opening files on remote hosts.

;;; Code:

(require 'tramp)
(require 'cl-lib)
(require 'transient)
(require 's)

;;
;; Host selection
;;

(defvar ev-tramp-hosts-favorites '("~/" "ted:" "beowulf@ted:" "phil:" "beowulf@phil:")
  "Preferred rsync host candidates (shown first in completion).")

(defun ev-tramp--ssh-hosts ()
  "Return host candidates from tramp's SSH completion sources.
Formats entries as \"host:\" or \"user@host:\"."
  (let (hosts)
    (cl-loop for (func file) in (tramp-get-completion-function "ssh")
             do (cl-loop for entry in (ignore-errors (funcall func file))
                         do (pcase entry
                              (`(,user ,host)
                               (when (and host (not (string-empty-p host)))
                                 (push (if (and user (not (string-empty-p user)))
                                           (format "%s@%s:" user host)
                                         (format "%s:" host))
                                       hosts))))))
    (nreverse hosts)))

(defun ev-tramp--all-hosts ()
  "Return combined list of preferred hosts and SSH-known hosts, deduplicated."
  (delete-dups (append ev-tramp-hosts-favorites (ev-tramp--ssh-hosts))))

(defun ev-tramp-read-host (prompt initial)
  "Read a host with completion using PROMPT, defaulting to INITIAL.
Free-form input is allowed for hosts not in the list."
  (completing-read prompt (ev-tramp--all-hosts) nil nil initial))

;;
;; Host/path parsing
;;

(defun ev-tramp--user@host (host)
  "Return \"user@host:\" or \"host:\" if HOST is remote, nil otherwise.
Remoteness is detected from a colon in HOST.
Strips any `/ssh:' tramp prefix."
  (when (string-match-p ".*:" host)
    (replace-regexp-in-string "\\(/ssh:\\)?\\([^:]*\\):.*" "\\2:" host)))

(defun ev-tramp--hostname (host)
  "Extract bare hostname from HOST (e.g. \"user@host:\" → \"host\").
Returns nil if HOST is local.  Handles `/ssh:' tramp prefix."
  (when (string-match-p ":" host)
    (replace-regexp-in-string "^\\(/ssh:\\)?\\([^@:]*@\\)?\\([^:]*\\):.*" "\\3" host)))

;;
;; Path translation
;;

(defvar ev-tramp-path-mappings
  '(("workspace" "/home/moutsopoulosg/spaces/workspace" ("ted" "phil" "ace") "/spool/workspace/moutsopoulosg")
    ("workspace" "/home/moutsopoulosg/spaces/workspace" ("cloud-tests-gm")   "work/moutsopoulosg"))
  "Path translation rules for `ev-tramp--this'.
Each entry is (MATCH-REGEX LOCAL-PATH HOSTS REMOTE-PATH).
MATCH-REGEX is matched against the file path.
LOCAL-PATH and REMOTE-PATH define the bidirectional mapping.
HOSTS is a list of hostnames this rule applies to, or t for all.")

(defun ev-tramp--convert-path (filelocal hostname)
  "Convert FILELOCAL path for HOSTNAME.
If HOSTNAME is non-nil, target is remote; if nil, target is local.
Uses `ev-tramp-path-mappings' for translation rules."
  (cl-loop for (match local-path hosts remote-path) in ev-tramp-path-mappings
           when (string-match-p match filelocal)
           do (cond
               ;; going to a matching remote host: local → remote
               ((and hostname
                     (or (eq hosts t)
                         (member hostname hosts)))
                (cl-return (replace-regexp-in-string
                            (regexp-quote local-path) remote-path filelocal t t)))
               ;; coming back to local: remote → local
               ((and (not hostname)
                     (string-match-p (regexp-quote remote-path) filelocal))
                (cl-return (replace-regexp-in-string
                            (regexp-quote remote-path) local-path filelocal t t))))
           finally return filelocal))

(defun ev-tramp--this (filename host)
  "Convert FILENAME to the equivalent path on HOST.
HOST is a local (\"~/\") or remote (\"user@host:\") target.
FILENAME may be a local or tramp remote path.
Path translations are defined in `ev-tramp-path-mappings'."
  (let* ((user@host (ev-tramp--user@host host))
         (absolute-path (expand-file-name filename))
         (filelocal (file-local-name absolute-path))
         (hostname (ev-tramp--hostname host))
         (converted (ev-tramp--convert-path filelocal hostname))
         (homeless (replace-regexp-in-string "^/home/[^/]*/" "" converted))
         (path-prefix (if user@host
                          (format "/ssh:%s" user@host)
                        "~/"))
         (target (concat path-prefix homeless)))
    target))

;; Keep old name as alias for compatibility
(defalias 'gm/ev-tramp--this #'ev-tramp--this)

;;
;; Open file on remote host
;;

;;;###autoload
(defun ev-tramp-here ()
  "Open the current file/dir on a remote host via tramp."
  (interactive)
  (let* ((filename (expand-file-name (or buffer-file-name dired-directory default-directory)))
         (host (ev-tramp-read-host "Host: " nil))
         (user@host (ev-tramp--user@host host))
         (is-remote-file (file-remote-p filename)))
    (let ((target (ev-tramp--this filename host)))
      (if (eq major-mode 'eshell-mode)
          (cd target)
        (find-file target)))))

;;
;; Rsync tree transient
;;

(defvar rsync-tree-history nil
  "History for rsync command editing.")

(defvar ev-rsync--directory nil "Current rsync directory.")
(defvar ev-rsync--from nil "Current rsync source host.")
(defvar ev-rsync--to nil "Current rsync destination host.")

(defvar ev-rsync--transform t "Whether to apply path transformation.")

(defun ev-rsync--transform-path (dirname host)
  "Transform DIRNAME + HOST into an rsync-compatible shell path.
Applies `ev-tramp--this' and strips the tramp `/ssh:' prefix."
  (let ((path (ev-tramp--this dirname host)))
    (replace-regexp-in-string "^/ssh:" "" path t nil)))

(defun ev-rsync--resolve-path (dirname host)
  "Resolve DIRNAME + HOST to final rsync path, respecting transform toggle."
  (if ev-rsync--transform
      (ev-rsync--transform-path dirname host)
    (if (ev-tramp--user@host host)
        (concat host (replace-regexp-in-string "^~/" "" dirname))
      dirname)))

(defun ev-rsync--format-value (value)
  "Format VALUE for display in transient, showing <not set> if nil."
  (if value
      (propertize value 'face 'transient-value)
    (propertize "<not set>" 'face 'transient-inactive-value)))

(defun ev-rsync--from-description ()
  "Description for From host showing current value."
  (format "From: %s" (ev-rsync--format-value ev-rsync--from)))

(defun ev-rsync--to-description ()
  "Description for To host showing current value."
  (format "To:   %s" (ev-rsync--format-value ev-rsync--to)))

(defun ev-rsync--dir-description ()
  "Description for directory showing current value."
  (format "Dir:  %s" (ev-rsync--format-value ev-rsync--directory)))

(defun ev-rsync--transform-description ()
  "Description for transform toggle."
  (format "Transform paths: %s"
          (if ev-rsync--transform
              (propertize "on" 'face 'transient-value)
            (propertize "off" 'face 'transient-inactive-value))))

(defun ev-rsync--hosts-heading ()
  "Heading for Hosts group, including resolved path preview."
  (if (and ev-rsync--from ev-rsync--to ev-rsync--directory)
      (let ((from (ev-rsync--resolve-path ev-rsync--directory ev-rsync--from))
            (to (ev-rsync--resolve-path ev-rsync--directory ev-rsync--to)))
        (format "Hosts  %s → %s"
                (propertize from 'face 'font-lock-string-face)
                (propertize to 'face 'font-lock-string-face)))
    "Hosts"))

(transient-define-suffix ev-rsync--set-from ()
  "Set the source host."
  :transient t
  (interactive)
  (setq ev-rsync--from (ev-tramp-read-host "From: " ev-rsync--from)))

(transient-define-suffix ev-rsync--set-to ()
  "Set the destination host."
  :transient t
  (interactive)
  (setq ev-rsync--to (ev-tramp-read-host "To: " ev-rsync--to)))

(transient-define-suffix ev-rsync--set-directory ()
  "Set the directory to rsync."
  :transient t
  (interactive)
  (setq ev-rsync--directory
        (file-local-name (read-directory-name "Directory: " ev-rsync--directory))))

(transient-define-suffix ev-rsync--toggle-transform ()
  "Toggle path transformation via ev-tramp--this."
  :transient t
  :description #'ev-rsync--transform-description
  (interactive)
  (setq ev-rsync--transform (not ev-rsync--transform)))

(transient-define-suffix ev-rsync--run ()
  "Build and execute the rsync command."
  (interactive)
  (unless (and ev-rsync--from ev-rsync--to)
    (user-error "Both From and To hosts must be set"))
  (let* ((args (transient-args 'ev-rsync-tree))
         (confirm (member "--confirm" args))
         (rsync-args (remove "--confirm" args))
         (from (ev-rsync--resolve-path ev-rsync--directory ev-rsync--from))
         (to (ev-rsync--resolve-path ev-rsync--directory ev-rsync--to))
         (command (concat "rsync " from " " to " " (s-join " " rsync-args)))
         (output-buffer "*rsync*"))
    (when confirm
      (setq command (read-string "rsync: " command 'rsync-tree-history)))
    (message command)
    (let ((default-directory (expand-file-name "~/")))
      (async-shell-command command output-buffer))))

;;;###autoload
(transient-define-prefix ev-rsync-tree (&optional init-directory)
  "Rsync a folder between two hosts with the same tree structure.

By using a base directory ev-rsync--directory it rsyncs from ev-rsync--from
to ev-rsync--to.  If ev-rsync--transform is set, it will also translate to
host-specific parent folders."
  [:description ev-rsync--hosts-heading
   ("f" ev-rsync--from-description ev-rsync--set-from)
   ("t" ev-rsync--to-description   ev-rsync--set-to)
   ("d" ev-rsync--dir-description  ev-rsync--set-directory)
   ("T" ev-rsync--toggle-transform)]
  ["Flags"
   ("-a" "Archive + verbose + compress (-avz)" "-avz")
   ("-r" "Recursive + verbose + compress (-rvz)" "-rvz")
   ("-n" "Dry run" "-n")
   ("-u" "Update (skip newer on receiver)" "-u")
   ("-m" "Prune empty dirs" "-m")
   ("-R" "Relative (preserve full path on dest)" "--relative")
   ("-P" "Progress + partial (resume transfers)" "--progress --partial")
   ("-c" "Checksum (skip based on checksum, not mod-time)" "--checksum")
   ("-L" "Follow symlinks (copy referent)" "--copy-links")
   ("-H" "Preserve hard links" "--hard-links")
   ("-p" "Preserve permissions" "--perms")
   ("-C" "CVS/auto-ignore (.git, *.o, etc)" "--cvs-exclude")]
  ["Delete"
   ("-D" "Delete extraneous from dest" "--delete")
   ("-X" "Delete excluded from dest" "--delete-excluded")]
  ["Exclude / Include"
   ("-e" "Exclude build artifacts (.dexy .cache *.pyc etc)"
    "--exclude \".dexy/\" --exclude \".cache/\" --exclude \"*.pyc\" --exclude \"README.md\" --exclude \".trash\"")
   ("-x" "Exclude dotfiles at root" "--exclude=\"/.*\"")
   ("-o" "Output dirs only"
    "-m --include=\"*/\" --include=\"*output/***\" --exclude=\"*\"")]
  ["Actions"
   ("-E" "Confirm (edit command before running)" "--confirm")
   ("R" "Run rsync" ev-rsync--run)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (setq ev-rsync--directory
        (file-local-name (or init-directory default-directory)))
  (setq ev-rsync--from nil)
  (setq ev-rsync--to nil)
  (setq ev-rsync--transform t)
  (transient-setup 'ev-rsync-tree))

(provide 'ev-tramp)
;;; ev-tramp.el ends here
