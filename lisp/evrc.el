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
						     "-avz --exclude \".dexy/\" --exclude \".cache/\" --exclude \"*.pyc\" --exclude \"README.md\" --exclude \".trash\"")
					:action (lambda (candidate) (helm-marked-candidates))))
(defvar rsync-tree-history nil)
(defun this-directory-or-master (init-directory)
  "If init-directory not provided, return this directory if in dev/master else dev/master"
  (or init-directory  ;; if provided use this
			     (if (string-prefix-p (expand-file-name "~/dev/master/")  default-directory)  ;; if current directory in dev/master
				 default-directory "~/dev/master/")))

(defun ev-rsync-tree (arg &optional init-directory)
  "Rsync a folder in two different homes that have the same tree structure.
   With a prefix argument it will suggest where to begin the selection of folder.
   By default it will begin in the dev/master or the current directory in dev/master."
  (interactive "P")
  (let* ((home-folder (expand-file-name "~/"))
      	 (init-directory (if arg (helm :sources helm-source-rsync-tree-common-dirs
	 			:prompt "Directory: "
	 			:buffer "*helm directory*")
	 		   (this-directory-or-master init-directory)))
	 (dirname 
	 	    (read-directory-name "Directory: " init-directory))

	 (from (helm :sources helm-source-rsync-tree-hosts
		     :prompt "From: "
		     :buffer "*helm host from*"))
	 (from (replace-regexp-in-string home-folder from dirname nil t))
	 (to (helm :sources helm-source-rsync-tree-hosts
		   :prompt "To: "
		   :buffer "*helm host to*"))
	 (to (replace-regexp-in-string home-folder to dirname nil t))
	 (args (helm :sources helm-source-rsync-tree-args
		     :buffer "*helm rsync args*"))
	(command (concat (concat "echo "  from " " to " ") (s-join " " args)))
	(command (read-string "rsync: " command 'rsync-tree-history)))
    (generate-new-buffer "*rsync*")
    (with-current-buffer "*rsync*"
      (goto-char (point-max))
      (insert (concat "$ " command "\n"))
      )
    (async-shell-command command "*rsync*")
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
	(termbuf (make-term "unison" "unison" nil selection)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

;;
;; ssh port link
;;
(defconst helm-source-ssh-port (helm-build-sync-source "remote"
				 :candidates '(("ted:8889" "ted" "8889")
					       ("ted:8888" "ted" "8888")
					       ("ted:8899" "ted" "8899")
					      ("ted:8898"  "ted" "8898")
					      ("phil:8888"  "phil" "8888")
					      ("phil:8889"  "phil" "8889"))
				 ))

(defun ev-ssh-port ()
  (interactive)
  (let* ((default-directory "~/")
	 (selection (helm :sources helm-source-ssh-port
			  :buffer "*helm ssh-port*"))
	 (host (first selection))
	 (port (second selection)))
    (async-shell-command (concat "ssh -NfL " port ":localhost:" port " " host))
    ))




