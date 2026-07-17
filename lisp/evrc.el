;;; evrc.el --- ev configurations

;;; Commentary:
;; Custom configuration file.

;;; Code:

(require 'helm)
(use-package s)
(require 'transient)
(require 'cl-lib)

;;
;; host selection (used by tramp and rsync)
;;
(defconst ev-rsync-hosts '("~/" "ted:" "beowulf@ted:" "phil:" "beowulf@phil:")
  "Preferred rsync host candidates (shown first in completion).")

(defun ev-rsync--ssh-hosts ()
  "Return host candidates from tramp's SSH completion sources.
Formats entries as \"host:\" or \"user@host:\"."
  (require 'tramp)
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

(defun ev-rsync--all-hosts ()
  "Return combined list of preferred hosts and SSH-known hosts, deduplicated."
  (delete-dups (append ev-rsync-hosts (ev-rsync--ssh-hosts))))

(defun ev-rsync--read-host (prompt initial)
  "Read a host with completion using PROMPT, defaulting to INITIAL.
Free-form input is allowed for hosts not in the list."
  (completing-read prompt (ev-rsync--all-hosts) nil nil initial))

;;
;; open with tramp this
;;
(defun ev-rsync--user@host (host)
  "Return \"user@host:\" or \"host:\" if HOST is remote, nil otherwise.

Remoteness is detected from a colon in HOST.
Strips any `/ssh:' tramp prefix."
  (when (string-match-p ".*:" host)
    (replace-regexp-in-string "\\(/ssh:\\)?\\([^:]*\\):.*" "\\2:" host)))

(defun ev-rsync--hostname (host)
  "Extract bare hostname from HOST (e.g. \"user@host:\" → \"host\").
Returns nil if HOST is local.  Handles `/ssh:' tramp prefix."
  (when (string-match-p ":" host)
    (replace-regexp-in-string "^\\(/ssh:\\)?\\([^@:]*@\\)?\\([^:]*\\):.*" "\\3" host)))

(defconst ev-tramp-path-mappings
  '(;; Each entry: (path-regex local-path host-group remote-path)
    ;; host-group is a list of hostnames, or t for all remote hosts
    ("workspace" "/home/moutsopoulosg/spaces/workspace" ("ted" "phil" "ace") "/spool/workspace/moutsopoulosg")
    ("workspace" "/home/moutsopoulosg/spaces/workspace" ("cloud-tests-gm")   "work/moutsopoulosg"))
  "Path translation rules for `gm/ev-tramp--this'.
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

(defun gm/ev-tramp--this (filename host)
  "Convert FILENAME to the equivalent path on HOST.

HOST is a local (\"~/\") or remote (\"user@host:\") target.
FILENAME may be a local or tramp remote path.
Path translations are defined in `ev-tramp-path-mappings'."
  (let* ((user@host (ev-rsync--user@host host))
         (absolute-path (expand-file-name filename))
         (filelocal (file-local-name absolute-path))
         (hostname (ev-rsync--hostname host))
         (converted (ev-tramp--convert-path filelocal hostname))
         (homeless (replace-regexp-in-string "^/home/[^/]*/" "" converted))
         (path-prefix (if user@host
                          (format "/ssh:%s" user@host)
                        "~/"))
         (target (concat path-prefix homeless)))
    target))

(defun ev-tramp-here ()
  "Open the current file/dir in an evalue host."
  (interactive)
  (let* ((filename (expand-file-name (or buffer-file-name dired-directory default-directory)))
	 (host (ev-rsync--read-host "Host: " nil))
	 (user@host (ev-rsync--user@host host))
	 (is-remote-file (file-remote-p filename)))
    (let ((target (gm/ev-tramp--this filename host)))
      (if (eq major-mode 'eshell-mode)
	  (cd target)
	(find-file target)))
    ))

;;
;; rsync tree to/from remote hosts
;;

(defvar rsync-tree-history nil
  "History for rsync command editing.")

(defvar ev-rsync--directory nil "Current rsync directory.")
(defvar ev-rsync--from nil "Current rsync source host.")
(defvar ev-rsync--to nil "Current rsync destination host.")

(defvar ev-rsync--transform t "Whether to apply gm/ev-tramp--this transformation.")

(defun ev-rsync--transform-path (dirname host)
  "Transform DIRNAME + HOST into an rsync-compatible path.
Applies `gm/ev-tramp--this' and strips the tramp `/ssh:' prefix."
  (let ((path (gm/ev-tramp--this dirname host)))
    (replace-regexp-in-string "^/ssh:" "" path t nil)))

(defun ev-rsync--resolve-path (dirname host)
  "Resolve DIRNAME + HOST to final rsync path, respecting transform toggle."
  (if ev-rsync--transform
      (ev-rsync--transform-path dirname host)
    (if (ev-rsync--user@host host)
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
  (setq ev-rsync--from (ev-rsync--read-host "From: " ev-rsync--from)))

(transient-define-suffix ev-rsync--set-to ()
  "Set the destination host."
  :transient t
  (interactive)
  (setq ev-rsync--to (ev-rsync--read-host "To: " ev-rsync--to)))

(transient-define-suffix ev-rsync--set-directory ()
  "Set the directory to rsync."
  :transient t
  (interactive)
  (setq ev-rsync--directory
        (file-local-name (read-directory-name "Directory: " ev-rsync--directory))))

(transient-define-suffix ev-rsync--toggle-transform ()
  "Toggle path transformation via ev-tramp-this."
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

(transient-define-prefix ev-rsync-tree (&optional init-directory)
  "Rsync a folder between two hosts with the same tree structure."
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


;;
;; unison
;;
(defun ev-unison ()
  (interactive)
  (let* ((default-directory "~/.unison/")
	(pfd (file-expand-wildcards "*prf"))
	(candidates (mapcar (lambda (x) (replace-regexp-in-string ".prf" "" x nil t)) pfd))
	(selection (completing-read "PRF: " candidates nil t))
	;; (switches (split-string-and-unquote args)
	(termbuf (make-term "unison" "/home/moutsopoulosg/.local/bin/unison" nil selection "-auto")))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

;;
;; ssh port link
;;
(defconst helm-source-ssh-port (helm-build-sync-source "remote"
				 :candidates '(
					       ("8889 george bastille phil"  "phil" "8889")
					       ("8898 beowulf banks ted"  "ted" "8898")
					       ("8889 george bastille ted" "ted" "8889")
					       ("8888 george banks ted" "ted" "8888")
					       ("8899 beowulf clarke ted" "ted" "8899")
					       ("8897 beowulf drake ted"  "ted" "8897")
					       ("8888 george banks phil"  "phil" "8888")
					       ("flower beowulf ted" "ted" "5555"))
				 ))

(defun ev-ssh-port ()
  "Forward ssh port from emacs."
  (interactive)
  (let* ((default-directory "~/")
	 (selection (helm :sources helm-source-ssh-port
			  :buffer "*helm ssh-port*"))
	 (host (nth 0 selection))
	 (port (nth 1 selection))
	 ;; (args (concat "-NfL " port ":localhost:" port " " host))
	 )
    ;; (async-shell-command (concat "ssh -NfL " port ":localhost:" port " " host))
    (start-process "ssh-forward-port" nil "ssh" "-NfL" (concat port ":localhost:" port) host)
    ))

;; (use-package gtags
;;   :custom
;;   (gtags-rootdir "/home/moutsopoulosg/dev/master/python"))

;; workspace links
(defvar gm/workspace-root "~/spaces/workspace" 
  "Root directory containing workspace folders.")
;; org links for workspace folders
(defun org-workspace-follow (path)
  (find-file (format "%s%s" (file-name-as-directory gm/workspace-root) path)))
(defun org-workspace-complete ()
  (concat "workspace:"(file-relative-name (read-file-name "File: " (file-name-as-directory gm/workspace-root)) gm/workspace-root)))
(org-link-set-parameters "workspace"
			 :follow 'org-workspace-follow
			 :complete 'org-workspace-complete)
;; advice to project.el on workspace folders
(defun gm/project-find-workspace-projects (dir)
  "Treat DIR as a project root if it's under my massive Project workspace.

Returns a project root like `~/spaces/workspace/X/'
"
  (let* ((parent-root (expand-file-name gm/workspace-root))
         (relative-path (file-relative-name dir parent-root)))
    (when (not (string-prefix-p ".." relative-path))
      (let ((subfolder (car (split-string relative-path "/"))))
        (cons 'transient (file-name-as-directory (abbreviate-file-name (expand-file-name subfolder parent-root))))))))
(add-hook 'project-find-functions #'gm/project-find-workspace-projects)

;; selecting projects in workspace
(defun gm/find-workspace-file ()
 "Jump to project folder or find file with preview."
(interactive)
(let ((default-directory gm/workspace-root))
 (consult-fd)))
(defun gm/find-workspace-folder () 
  "Select a subfolder in projects root and open in dired, sorted by date." 
  (interactive) 
  (let* ((default-directory (file-name-as-directory (expand-file-name gm/workspace-root)))
	 (folders (sort (directory-files default-directory nil "^[^.]" t)
			(lambda (a b)
			  (time-less-p
			   (file-attribute-modification-time (file-attributes (expand-file-name b)))
			   (file-attribute-modification-time (file-attributes (expand-file-name a)))))))
	 (choice (completing-read "Project: "
				  (lambda (str pred action)
				    (if (eq action 'metadata)
					'(metadata (display-sort-function . identity)
						   (cycle-sort-function . identity)
						   (category . file))
				      (complete-with-action action folders str pred)))
				  nil t)))
    (dired (expand-file-name choice))))


;; (use-package ggtags
;;   :custom
;;   (ggtags-navigation-mode nil)
;;   (ggtags-enable-navigation-keys nil))

;; (defun ev-ggtags ()
;;   "Find global tags in dev/master/python from anywhere. Use C-u to specify tag."
;;   (interactive)
;;   (let ((default-directory "/home/moutsopoulosg/dev/master/python/")
;; 	(current-prefix-arg '(4)))
;;   (call-interactively 'ggtags-find-definition)))

(defun gm/ev-find-definition ()
  "Find global tags from anywhere"
  (interactive)
  (visit-tags-table "/home/moutsopoulosg/dev/master/TAGS" t)
  (helm-etags-select nil))

(use-package magit-worktree)
(defun gm/get-worktrees ()
  (cl-loop for el in (magit-list-worktrees)
	   collect (cons (nth 2 el) (nth 0 el))
	   ))

(defconst gm/helm-source-worktree-root-dirs
  (helm-build-sync-source "worktree"
    :candidates 'gm/get-worktrees))

(defun gm/ev-change-worktree (arg)
  "Find file but in another worktree. With ARG keep current file."
  (interactive "P")
  (let ((filename (expand-file-name (or buffer-file-name dired-directory default-directory)))
	(worktree-path (helm gm/helm-source-worktree-root-dirs))
	(from-string (project-root (project-current)))
	(func (if arg 'find-file 'find-alternate-file))
	)
    (funcall func (replace-regexp-in-string from-string worktree-path filename nil t))))
(defalias  'gm/ev-switch-worktree 'gm/ev-change-worktree)

;; (setq run-banks-kernel-on-phil-command
;;       "PYTHONPATH=/home/moutsopoulosg/dev/master/python; PATH=/home/moutsopoulosg/miniconda/bin:\$PATH; source activate banks; ipython kernel -f kernel-emacs-remote.json")
;; (defun start-banks-kernel-on-phil
;;     (start-process "remote-banks-kernel" "*remote-banks-kernel*"
;; 		   "ssh" "phil" run-banks-kernel-on-phil-command))
;; (defun connect-banks-kernel-on-phil
;; (run-python "ipython console --ssh phil --existing ~/.ipython/profile_default/security/kernel-emacs-remote.json"))

(defun gm/ev-replace-commentary-tex-chars-buffer ()
  (interactive)
  (dolist (rep '(
		 ("[\\]*%" "\\\\%")
		 ("–" "--")
		 ("[\\]*&" "\\\\&")
		 ("’" "'")
		 ("“" "``")
		 ("”" "''")
		 ("…" "\\\\ldots")
		 ))
  (replace-regexp-in-region (nth 0 rep) (nth 1 rep) (point-min) (point-max))
  ))

(provide 'evrc)
;;; evrc.el ends here
