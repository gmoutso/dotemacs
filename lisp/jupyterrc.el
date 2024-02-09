;; (conda-env-activate "emacs")
;; use (jupyter-command "kernelspec" "list" "--json" "--log-level=40")
;; /home/moutsopoulosg/.emacs.d/elpa/jupyter-20220419.1852/jupyter-kernelspec.el:64
(use-package jupyter
  :after (ob-jupyter ob-python)
  :config
  (setq jupyter-api-authentication-method 'ask)
  (setq jupyter-eval-use-overlays nil)
  (setq org-babel-default-header-args:jupyter-python '((:session . "/jpy:localhost#8888:py")
                                                       (:kernel . "conda-env-edge-py")
                                                       (:async . "yes")
						       (:pandoc t)))
  (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names)
  (setq ob-async-no-async-languages-alist '("jupyter-python"))
  (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python")))

(use-package jupyter-tramp)
(require 'ob-jupyter)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) (python . t) (emacs-lisp . t) (dot . t) (plantuml . t)
   (jupyter . t)))
(conda-with-env "emacs"
  (org-babel-jupyter-make-local-aliases))

;;
;; jupyter repl
;;
(general-def jupyter-repl-interaction-mode-map
  "C-c C-p" 'jupyter-repl-pop-to-buffer)
(general-unbind jupyter-repl-interaction-mode-map "C-c C-r")

(general-def jupyter-server-kernel-list-mode-map
  "C-c C-c" 'jupyter-server-kernel-list-launch-kernel
  )


;; https://github.com/nnicandro/emacs-jupyter/issues/366
;; garbled errors
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)


;;
;; helm REPLs for python buffers
;;
;; there are three options offered:
;; 1) load a new kernel and reply by choosing kernel spec
;; 2) load a new repl for an existing running kernel
;; 3) switch to an existing repl
;;
;; (2) make new repl for existing kernel
(defun gm/helm-candidates-kernels ()
  "Get alist of live kernel names to kernel ids"
    (with-helm-current-buffer
      (let* ((server (jupyter-current-server))
	    (kernels (jupyter-api-get-kernel server)))
	(cl-loop
	 with names = nil
	 for kernel across kernels
	 collect
	 (cl-destructuring-bind
	     (&key name id last_activity execution_state
		   connections &allow-other-keys)
	     kernel
	   (let* ((time (jupyter-decode-time last_activity))
		  (name (or (jupyter-server-kernel-name server id)
			    (let ((same (cl-remove-if-not
					 (lambda (x) (string-prefix-p name x)) names)))
                              (when same (setq name (format "%s<%d>" name (length same))))
                              (push name names)
                              name)))
		  (activity (jupyter-format-time-low-res time))
		  (conns (number-to-string connections))
		  (state execution_state)
		  (info (propertize (format "(%s, %s, %s connections)" state activity conns)
				    'face 'shadow)))
	     (cons (format "%-30s id:%s %s" name (propertize id 'face 'fixed-pitch) info) id)))))))
(defun gm/helm-action-kernels-create-repl-name (id &optional guess)
  "Name the repl when creating a new repl for existing kernel"
    (let* ((server (jupyter-current-server))
	   (buffername (buffer-name))
	   (name/byserver (plist-get (jupyter-api-get-kernel server id) :name))
	   (name/bykernelnames (jupyter-server-kernel-name server id))
	   (default (or name/bykernelnames (file-name-sans-extension buffername) name/byserver)))
    (if (not guess) (read-string (format "REPL Name (%s): " default) nil nil default) default)))
(defun gm/helm-action-kernels-create-repl (id)
  (let ((server (jupyter-current-server))
	(replname (gm/helm-action-kernels-create-repl-name id))
	(associate (y-or-n-p "associate?")))
    (jupyter-connect-server-repl server id replname associate nil t)))
(setq gm/helm-source-jupyter-server-kernel-list
      (helm-build-sync-source
	  "Live Kernels"
	:candidates 'gm/helm-candidates-kernels
	:action 'gm/helm-action-kernels-create-repl))
;;
;; (1)

;; (defun gm/helm-candidates-specs ()
;;   "Get alist of spec display names to specs"
;;     (with-helm-current-buffer
;; (let* ((server (jupyter-current-server))
;;        (specs (jupyter-server-kernelspecs server)))
;;   (cl-loop with names = nil
;; 	for spec in specs
;; 	collect
;; 	(cons (plist-get (cddr spec) :display_name) (car spec))))))

(defun gm/helm-candidates-specs ()
  "Get alist of spec display names to specs"
    (with-helm-current-buffer
(let* ((server (jupyter-current-server))
       (specs (jupyter-kernelspecs server)))
  (cl-loop with names = nil
	for spec in specs
	collect
	(cons (plist-get (jupyter-kernelspec-plist spec) :display_name) spec)))))

(defun gm/helm-action-specs-create-new-kernel-name (spec &optional ask)
  "Name the repl when creating a new repl for existing kernel"
  (let (default (file-name-sans-extension (buffer-name)))
    (if ask
	(read-string "REPL Name: " nil nil default)
      default)))

(defun gm/helm-action-specs-create-kernel-and-repl (spec)
  "Create kernel based on spec, create repl, name both and associate."
  (let* ((server (jupyter-current-server))
	 (kernel-name spec)
	 (kernelname (gm/helm-action-specs-create-new-kernel-name spec t))
	 (replname kernelname)
	 (associate (y-or-n-p "associate?")))
    (jupyter-run-server-repl server spec replname associate nil t)
    (jupyter-server-name-client-kernel jupyter-current-client kernelname)))
(setq gm/helm-source-jupyter-server-spec-list
      (helm-build-sync-source
	  "Specs"
	:candidates 'gm/helm-candidates-specs
	:action 'gm/helm-action-specs-create-kernel-and-repl))
;; (3)
(defun gm/jupyter-server-repl-get-kernel-name-or-id (buffer)
  (let* ((server (jupyter-current-server))
	 (client (with-current-buffer buffer jupyter-current-client))
	 (manager (slot-value client 'manager))
	 (kernel (slot-value manager 'kernel))
	 (id (slot-value kernel 'id)))
    (or (jupyter-server-kernel-name server id) id)))
(defun gm/helm-candidate-repl-format (buffer)
  (format "%-25s %s" (buffer-name buffer)
	  (propertize (gm/jupyter-server-repl-get-kernel-name-or-id buffer) 'face 'fixed-pitch)))
(defun gm/helm-candidates-repls ()
  "Get ALIST of buffer names to buffers of live repls."
  (with-helm-current-buffer
  (let ((buffers (jupyter-repl-available-repl-buffers)))
    (mapcar (lambda (b) (cons (gm/helm-candidate-repl-format b) b)) buffers))
  ))
(defun gm/helm-action-repls-pop-associate (buffer)
  (let ((client (buffer-local-value 'jupyter-current-client buffer)))
    (if (and (eq (jupyter-kernel-language-mode client) major-mode) (y-or-n-p "associate?"))
	(jupyter-repl-associate-buffer client))
    (pop-to-buffer buffer)
  ))
(setq gm/helm-source-jupyter-server-repl-list
      (helm-build-sync-source
	  "REPLs"
	:candidates 'gm/helm-candidates-repls
	:action 'gm/helm-action-repls-pop-associate))
(defun gm/jupyter-get-server ()
  (let ((server (jupyter-current-server current-prefix-arg)))
    (jupyter-api-ensure-authenticated server)
    server)
  )
(defun gm/jupyter-kernels ()
  (interactive)
  (gm/jupyter-get-server)
  (helm :sources '(gm/helm-source-jupyter-server-repl-list
		   gm/helm-source-jupyter-server-spec-list
		   gm/helm-source-jupyter-server-kernel-list)))
;;
;; helm python whos
;;
(defun gm/jupyter-repl-python-whos ()
  (let* ((jupyter-current-client (or jupyter-current-client
				     (jupyter-org-with-src-block-client jupyter-current-client)))
	 (code "import io\nfrom contextlib import redirect_stdout\nf = io.StringIO()\nwith redirect_stdout(f):\n    %whos\nf.getvalue()")
	 (value (jupyter-eval code))
	 (value (read (princ value))))
    value))
(defun gm/jupyter-repl-python-whos-trimmed ()
  (let ((value (gm/jupyter-repl-python-whos)))
  (with-temp-buffer
    (insert value)
    (goto-line 1)
    (kill-whole-line 2)
    (buffer-string))))
(defun gm/jupyter-page-object (strobject)
  (jupyter-eval-string (format "%%page %s" strobject)))
(defun gm/jupyter-whos ()
  (interactive)
  (let ((data (gm/jupyter-repl-python-whos-trimmed)))
  (helm :sources (helm-build-in-buffer-source "whos"
		   :data data
		   :display-to-real (lambda (line) (nth 0 (split-string line)))
		   :action '(("insert" . insert)
			     ("definition" . gm/org-find-definition)
			     ("show" . gm/jupyter-page-object))
		   ))))


(defun gm/ipynb-to-html-with-nbconvert (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (shell-command-on-region (point-min) (point-max)
       "/home/moutsopoulosg/anaconda3/envs/egan/bin/jupyter-nbconvert --to html --log-level ERROR --stdout --stdin"
       nil 'no-mark)
    (buffer-string)
    ))

(defun gm/shr-open-ipynb (&optional filename)
  "Open ipynb file as html.

Opens either file name at point (if in dired), current file (if .ipynb) or via find-file. Creates two buffers, html and shr."
  (interactive)
  (let* ((filename (cond
		   (filename filename)
		   ((derived-mode-p 'dired-mode) (dired-file-name-at-point))
		   ((and (buffer-file-name)
			 (string-equal (file-name-extension buffer-file-name) "ipynb"))
		    buffer-file-name)
		   (t (read-file-name "ipynb file: "))))
	 (shortname (file-name-nondirectory filename))
	 (html-buffer (generate-new-buffer
		       (concat (file-name-sans-extension shortname) ".html"))))
    (with-current-buffer html-buffer
      (insert (gm/ipynb-to-html-with-nbconvert filename))
      (html-mode))
    (shr-render-buffer html-buffer)
    (with-current-buffer "*html*"
      (rename-buffer shortname 'unique)
      (read-only-mode t))
    ))

;; allow editing jupyter-src src blocks without errors
(defun gm/advice-org-babel-edit-prep:jupyter (func info)
  "Allow editing jupyter blocks when session is set to 'none'."
  (let* ((params (nth 2 info))
         (session (alist-get :session params))
	 (kernel (alist-get :session params)))
    (if (string-equal session "none")
	nil
      (funcall func info)
      )))
(advice-add 'org-babel-edit-prep:jupyter :around 'gm/advice-org-babel-edit-prep:jupyter)
;; (advice-remove 'org-babel-edit-prep:jupyter 'gm/advice-org-babel-edit-prep:jupyter)
;; To ensure python src blocks are opened in python-ts-mode
(setf (alist-get "jupyter-python" org-src-lang-modes nil nil #'equal) 'python-ts)
(setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts)

;; Decorating Jupyter blocks
;; make dataframe output not have a RESULTS drawer by adding org-table property to results
;; you need to manually add :table t in block
(defun gm/jupyter-org-table-string-maybe (func type value params)
  "Add org-table property to pandoc output if table in PARAMS"
  ;; type: 'html
  ;; value: long html sting
  ;; params: includes ((:pandoc t) (:tangle . yes))
  ;; 
  (let ((result (funcall func type value params)))
    (if (and (eq (org-element-type result) 'pandoc)
	     (alist-get :table params))
	(let ((vls (car (cdr result))))
	  (let ((newresult
		(list 'pandoc (list :text (jupyter-org-table-string (plist-get vls :text))
			    :type (plist-get vls :type)
			    :value (plist-get vls :value)
			    ))))
	    newresult)
	  )
      result)))
(advice-add 'jupyter-org-export-block-or-pandoc :around #'gm/jupyter-org-table-string-maybe)
;; (advice-remove 'jupyter-org-export-block-or-pandoc 'gm/jupyter-org-table-string-maybe)

;; (defun gm/jupyter-server-cull-kernel-names (&optional server)
;;   "Ensure all names in `jupyter-server-kernel-names' map to existing kernels.
;; If SERVER is non-nil only check the kernels on SERVER, otherwise
;; check all kernels on all existing servers.

;; Override. If I have not forwarded the port, I don't want my names culled."
;;   (message "arg0: %s" jupyter-server-kernel-names)
;;   (let ((servers (if server (list server)
;;                    (jupyter-gc-servers)
;;                    (jupyter-servers))))
;;     (message "arg1: %s" jupyter-server-kernel-names)
;;     (unless server
;;       ;; Only remove non-existing servers when culling all kernels on all
;;       ;; servers.
;;       (let ((urls (mapcar (lambda (x) (oref x url)) servers)))
;;         (cl-callf2 cl-remove-if-not (lambda (x) (member (car x) urls))
;;                    jupyter-server-kernel-names))
;;       (message "arg2: %s" jupyter-server-kernel-names)
;;       )
;;     (dolist (server servers)
;;       (when-let* ((names (assoc (oref server url) jupyter-server-kernel-names)))
;;         (setf (alist-get (oref server url)
;;                          jupyter-server-kernel-names nil nil #'equal)
;;               (cl-loop
;;                for kernel across (jupyter-api-get-kernel server)
;;                for name = (assoc (plist-get kernel :id) names)
;;                when name collect name)))))
;;   (message "arg3: %s" jupyter-server-kernel-names))
;; (advice-add 'jupyter-server-cull-kernel-names :override #'gm/jupyter-server-cull-kernel-names)
;; (advice-remove 'jupyter-server-cull-kernel-names 'gm/jupyter-server-cull-kernel-names)

;; Issues with jupyter-emacs
;;
;; issue: shutdown kernel when killing repl
;; https://github.com/emacs-jupyter/jupyter/commit/1daf4463c13402b5ee6be883ed8903812688247a
;; issue: websocket gets disconnected, cannot reconnect
;; https://github.com/emacs-jupyter/jupyter/issues/395
(defun gm/jupyter-disconnect-and-kill-repl ()
  (interactive nil 'jupyter-repl-interaction-mode)
  (jupyter-disconnect jupyter-current-client)
  (jupyter-repl-interaction-mode -1)
  (kill-buffer)
  )
;; %debug uses minibuffer
;; https://github.com/emacs-jupyter/jupyter/issues/184
;; https://github.com/emacs-jupyter/jupyter/issues/35#issuecomment-497039866
;; login endpoint is missing when no token/password
(defun gm/jupyter-api-request-xsrf-cookie-error-advice (func &rest args)
  (condition-case-unless-debug nil
      (apply func args)
    (jupyter-api-http-error nil)))
(advice-add 'jupyter-api-request-xsrf-cookie :around #'gm/jupyter-api-request-xsrf-cookie-error-advice)

