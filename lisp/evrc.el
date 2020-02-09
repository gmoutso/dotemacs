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
						     "-m --include=\"*/\" --include=\"*output/***\" --exclude=\"*\"")
					:action (lambda (candidate) (helm-marked-candidates))))
(defvar rsync-tree-history nil)
(defun this-directory-or-master (init-directory)
  "If init-directory not provided, return this directory if in dev/master else dev/master"
  (or init-directory  ;; if provided use this
			     (if (string-prefix-p (expand-file-name "~/dev/master/")  default-directory)  ;; if current directory in dev/master
				 default-directory "~/dev/master/")))

(defun ev-tramp-this (filename host)
  "Convert filename to tramp version on evalue space.

   host is of the form ~/ or beowulf@ted:
   filename must be in a /home/someones/ and can be tramped"
  (let* ((filename (if (file-remote-p filename)
		       (file-local-name filename)
		     (expand-file-name filename)
		     ))
	 (is-remote-host (string-match-p ".*:" host))
	 (host (if is-remote-host
		   (concat "/ssh:" host)
		    host))
	 (filename (replace-regexp-in-string "^/home/[^/]*/" host filename nil nil))
	 )
    (expand-file-name filename)
    ))

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
    (let (default-directory home-folder)
    (async-shell-command command "*rsync*"))
  ))

;;
;; open with tramp this
;;
(defun ev-tramp-here (arg &optional init-directory)
  "Open the current file/dir in an evalue host."
  (interactive)
  (let* ((filename (or buffer-file-name dired-directory default-directory))
	 (host (helm :sources helm-source-rsync-tree-hosts
		     :prompt "Host: "
		     :buffer "*helm host from*"))
	 (filename (ev-tramp-this filename host))
	 )
    (find-file filename)
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
			 :buffer "*helm unison*"))
	;; (switches (split-string-and-unquote args)
	(termbuf (make-term "unison" "/home/moutsopoulosg/.local/bin/unison" nil selection)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

;;
;; ssh port link
;;
(defconst helm-source-ssh-port (helm-build-sync-source "remote"
				 :candidates '(("8889 george ted" "ted" "8889")
					       ("8888 george ted" "ted" "8888")
					       ("8899 beowulf ted" "ted" "8899")
					      ("8898 beowulf ted"  "ted" "8898")
					      ("8888 george phil"  "phil" "8888")
					      ("8889 george phil"  "phil" "8889")
					      ("flower ted" "ted" "5555"))
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
