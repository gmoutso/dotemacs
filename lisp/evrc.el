(use-package s)

;;
;; helm rsync tree to/from ted
;;
(defconst helm-source-rsync-tree-hosts
  (helm-build-sync-source "Host"
    :candidates (list "~/" "ted:" "beowulf@ted:" "phil:" "beowulf@phil:")))
;; (defvar (defvar helm-rsync-tree-directory-history nil
	  ;; "History of helm-rsync-tree directories."))
(defconst helm-source-rsync-tree-common-dirs (helm-build-sync-source "Directory"
					       :candidates (list "~/dev/master/python/ev/reporting/latex/reports/"
					"~/dev/master/python/ev/reporting/latex/templates/"			 )
					;; :history helm-rsync-tree-directory-history
					))
(defconst helm-source-rsync-tree-args (helm-build-sync-source "arguments"
					:candidates (list
						     "-avz" "-rvz" "-n" "-u" "--delete" "--delete-excluded"
						     "--exclude \".dexy/\" --exclude \".cache/\" --exclude \"*.pyc\" --exclude \"README.md\" --exclude \".trash\""
						     "--exclude=\"/.*\""
						     "-m --include=\"*/\" --include=\"*output/***\" --exclude=\"*\"")
					:action (lambda (candidate) (helm-marked-candidates))))
(defvar rsync-tree-history nil)
(defun this-directory-or-master (init-directory)
  "If init-directory not provided, return this directory if in dev/master else dev/master"
  (or init-directory  ;; if provided use this
			     (if (string-prefix-p (expand-file-name "~/dev/master/")  default-directory)  ;; if current directory in dev/master
				 default-directory "~/dev/master/")))


;;
;; rsync a whole tree
;;
(defun ev-rsync-tree (arg &optional init-directory)
  "Rsync a folder in two different homes that have the same tree structure.
   With a prefix argument it will ask from dev/master."
  (interactive "P")
  (let* ((home-folder (expand-file-name "~/"))
      	 (init-directory (if arg (expand-file-name "~/dev/master/")
			 (file-local-name (or init-directory default-directory dired-directory))))
	 (dirname (read-directory-name "Directory: " init-directory))
	 (from (helm :sources helm-source-rsync-tree-hosts
		     :prompt "From: "
		     :buffer "*helm host from*"))
	 (from (ev-tramp-this dirname from))
	 (from (replace-regexp-in-string "^/ssh:" "" from t nil))
	 (to (helm :sources helm-source-rsync-tree-hosts
		   :prompt "To: "
		   :buffer "*helm host to*"))
	 (to (ev-tramp-this dirname to))
	 (to (replace-regexp-in-string "^/ssh:" "" to t nil))
	 (args (helm :sources helm-source-rsync-tree-args
		     :buffer "*helm rsync args*"))
	(command (concat (concat "rsync "  from " " to " ") (s-join " " args)))
	(command (read-string "rsync: " command 'rsync-tree-history)))
    ;; (generate-new-buffer "*rsync*")
    ;; (with-current-buffer "*rsync*"
    ;;   (goto-char (point-max))
    ;;   (insert (concat "$ " command "\n"))
    ;;   )
    (message command)
    (let ((default-directory home-folder))
    (async-shell-command command "*rsync*"))
  ))

;;
;; open with tramp this
;;
(defun ev-tramp-this (filename host)
  "Convert filename to tramp version on evalue space.

   host is either ~/ or of the form [user@]host: eg beowulf@ted:
   the filename might start with /home/someones/ or some other absolute path from /"
  (let* ((is-remote-host (string-match-p ".*:" host))
	 (is-remote-file (file-remote-p filename))
	 (filelocal (expand-file-name (file-local-name filename))))
    (concat (if is-remote-host "/ssh:") host (replace-regexp-in-string "^/home/[^/]*/" "" filelocal))))


(defun gm/match-replace-gather-select (origin mrlist)
  "MRLIST is a list of triplets of the form (MATCH REGEX REPLACE)."
  (helm :sources (helm-build-sync-source "hosts"
		   :candidates (cl-loop for (match regex replace) in mrlist
							  if (string-match-p match origin) collect
							  (replace-regexp-in-string regex replace origin))
		   :fuzzy-match t)
	:buffer "*helm evalue*"))

(defun ev-tramp-here ()
  "Open the current file/dir in an evalue host."
  (interactive)
  (let* ((filename (expand-file-name (or buffer-file-name dired-directory default-directory)))
	 (host (helm :sources helm-source-rsync-tree-hosts
		     :prompt "Host: "
		     :buffer "*helm host from*"))
	 (is-remote-host (string-match-p ".*:" host))
	 (is-remote-file (file-remote-p filename)))
    ;; special conditions
    (if (string-match-p "workspace" filename)
	(cond ((and (not is-remote-file) is-remote-host) (setq filename (replace-regexp-in-string "/home/moutsopoulosg/workspace" "/spool/workspace" filename)))
	      ((and (not is-remote-host) is-remote-file) (setq filename (replace-regexp-in-string "/spool/workspace" "/home/moutsopoulosg/workspace" filename)))))
    ;; end special conditions
    (find-file (ev-tramp-this filename host))
    ))

;;
;; unison
;;
(defun ev-unison ()
  (interactive)
  (let* ((default-directory "~/.unison/")
	(pfd (file-expand-wildcards "*prf"))
	(candidates (mapcar (lambda (x) (replace-regexp-in-string ".prf" "" x nil t)) pfd))
	(selection (helm :sources (helm-build-sync-source "name"
				    :candidates candidates)
			 :input "w"
			 :buffer "*helm unison*"))
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
  (interactive)
  (let* ((default-directory "~/")
	 (selection (helm :sources helm-source-ssh-port
			  :buffer "*helm ssh-port*"))
	 (host (first selection))
	 (port (second selection))
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
