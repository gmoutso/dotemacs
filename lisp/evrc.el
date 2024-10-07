(use-package s)

;;
;; helm rsync tree to/from ted
;;
(defconst helm-source-ev-hosts
  (helm-build-sync-source "Host"
    :candidates (list "~/" "ted:" "beowulf@ted:" "phil:" "beowulf@phil:")))
(defun helm-ev-host (&optional prompt)
  "Choose ev host name."
  (let ((prompt (or prompt "Host: ")))
    (helm :sources helm-source-ev-hosts
	  :prompt prompt
	  :buffer "*helm-ev-host*")))
(defconst helm-source-rsync-tree-args (helm-build-sync-source "arguments"
					:candidates (list
						     "-avz" "-rvz" "-n" "-u" "--delete" "--delete-excluded"
						     "--exclude \".dexy/\" --exclude \".cache/\" --exclude \"*.pyc\" --exclude \"README.md\" --exclude \".trash\""
						     "--exclude=\"/.*\""
						     "-m --include=\"*/\" --include=\"*output/***\" --exclude=\"*\"")
					:action (lambda (candidate) (helm-marked-candidates))))
(defvar rsync-tree-history nil)

;;
;; rsync a whole tree
;;
(defun ev-rsync-tree (&optional init-directory)
  "Rsync a folder in two different homes that have the same tree structure."
  (interactive)
  (let* ((init-directory (file-local-name (or init-directory default-directory dired-directory)))
	 (dirname (read-directory-name "Directory: " init-directory))
	 (from (helm-ev-host "From: "))
	 (to (helm-ev-host "To: "))
	 (args (helm :sources helm-source-rsync-tree-args
		     :buffer "*helm rsync args*")))
    (ev-do-rsync dirname from to args nil t)
  ))

(defun ev-do-rsync (dirname from to args &optional output-buffer confirm)
  "Rsync convenience that translates dirname to evalue hostnames.

FROM and TO are hosts, eg `beowulf@ted' or `~/', and DIRNAME can be any local or remote directory that `ev-tramp-this' recognizes, eg in dev/master or workspace.
"
  (let* ((from (ev-tramp-this dirname from))
	 (from (replace-regexp-in-string "^/ssh:" "" from t nil))
	 (to (ev-tramp-this dirname to))
	 (to (replace-regexp-in-string "^/ssh:" "" to t nil))
	 (command (concat (concat "rsync "  from " " to " ") (s-join " " args)))
	 (output-buffer (or output-buffer "*rsync*")))
    (if confirm (setq command (read-string "rsync: " command 'rsync-tree-history)))
    (message command)
    (let ((default-directory (expand-file-name "~/")))
      (async-shell-command command output-buffer))
    ))

;;
;; open with tramp this
;;
(defun gm/remote-host (host)
  "Return hostname or nil. 

Remoteness is detected from a semi-column `:' in HOST.
If remote, returns hostname removing any ssh protocol."
  (if (string-match-p ".*:" host) (replace-regexp-in-string "\\(/ssh:\\)?\\([^:]*\\):.*" "\\2:" host)))

(defun ev-tramp-this (filename host)
  "Convert filename to tramp version on evalue space.

   HOST is either a local or remote path, eg `~/somewhere' or `beowulf@ted:'.
   HOST can accept tramp names like `/ssh:beowulf@ted:somewhere'.
   Remoteness is detected from a semi-column `:' in HOST.
   the FILENAME might be /home/someones/ with or without host info."
  (let* ((is-remote-host (gm/remote-host host))
	 (is-remote-file (file-remote-p filename)) ; /ssh:host:
	 (absolute-path (expand-file-name filename))
	 (filelocal (file-local-name absolute-path))
	 (converted (cond ((and
				 (string-match-p "workspace" filelocal)
				 is-remote-host)
				(replace-regexp-in-string
				 "/home/moutsopoulosg/workspace"
				 "/spool/workspace"
				 filelocal))
			       ((and
				 (string-match-p "workspace" filelocal)
				 (not is-remote-host))
				(replace-regexp-in-string
				 "/spool/workspace"
				 "/home/moutsopoulosg/workspace"
				 filelocal))
			       (filelocal)))
	 (homeless (replace-regexp-in-string "^/home/[^/]*/" "" converted))
	 (path-prefix (if is-remote-host
			  (format "/ssh:%s" is-remote-host)
			"~/"))
	 (target (concat path-prefix homeless))
	 )
    target))

(defun ev-tramp-here ()
  "Open the current file/dir in an evalue host."
  (interactive)
  (let* ((filename (expand-file-name (or buffer-file-name dired-directory default-directory)))
	 (host (helm-ev-host "Host: "))
	 (is-remote-host (gm/remote-host host))
	 (is-remote-file (file-remote-p filename)))
    (let ((target (ev-tramp-this filename host)))
      (if (eq major-mode 'eshell-mode)
	  (cd target)
	(find-file target)))
    ))

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
(defvar workspace-folder "~/workspace/moutsopoulosg")
(defun org-workspace-follow (path)
  (find-file (format "%s%s" (file-name-as-directory workspace-folder) path)))
(defun org-workspace-complete ()
  (concat "workspace:"(file-relative-name (read-file-name "File: " (file-name-as-directory workspace-folder)) workspace-folder)))
(org-link-set-parameters "workspace"
			 :follow 'org-workspace-follow
			 :complete 'org-workspace-complete)

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
	(from-string (projectile-project-root))
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
