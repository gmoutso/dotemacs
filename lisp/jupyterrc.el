;;; jupyterrc.el --- (conda-env-activate "emacs")

;;; Commentary:
;; Custom configuration file.

;;; Code:

;; (conda-env-activate "emacs")
;; use (jupyter-command "kernelspec" "list" "--json" "--log-level=40")
;; /home/moutsopoulosg/.emacs.d/elpa/jupyter-20220419.1852/jupyter-kernelspec.el:64
(use-package jupyter
  :after org
  ;; :after (ob-jupyter ob-python)
  :custom
  (jupyter-org-auto-connect nil)
  (jupyter-api-authentication-method 'ask)
  (jupyter-eval-use-overlays nil)
  (jupyter-use-zmq nil)
  :config
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
						  (:pandoc . t)
						  (:exports . "results")
						  (:tangle . "yes")))
  (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names)
  (setq ob-async-no-async-languages-alist '("jupyter-python"))
  (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python")))

(use-package jupyter-tramp)
(require 'ob-jupyter)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) (python . t) (emacs-lisp . t) (dot . t) (plantuml . t)
   (jupyter . t)))

;; why is this needed here. It is also an org-mode hook?
;; (conda-with-env "emacs"
;;   (org-babel-jupyter-make-local-aliases))
(org-babel-jupyter-make-local-aliases) ;; will it find them??

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
;; (defun gm/advice-org-babel-edit-prep:jupyter (func info)
;;   "Allow editing jupyter blocks when session is set to 'none'."
;;   (let* ((params (nth 2 info))
;;          (session (alist-get :session params))
;; 	 (kernel (alist-get :session params)))
;;     (if (string-equal session "none")
;; 	nil
;;       (funcall func info)
;;       )))
;; (advice-add 'org-babel-edit-prep:jupyter :around 'gm/advice-org-babel-edit-prep:jupyter)
;; (advice-remove 'org-babel-edit-prep:jupyter 'gm/advice-org-babel-edit-prep:jupyter)
;; superceded by jupyter-org-auto-connect
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
  (condition-case nil
      (apply func args)
    (jupyter-api-http-error nil)))
(advice-add 'jupyter-api-request-xsrf-cookie :around #'gm/jupyter-api-request-xsrf-cookie-error-advice)
;; (advice-remove 'jupyter-api-request-xsrf-cookie 'gm/jupyter-api-request-xsrf-cookie-error-advice)

;; https://github.com/emacs-jupyter/jupyter/issues/607
(defun my/jupyter-org-results-drawer-pre-blank-fix (element)
    "Advice to ensure the RESULTS drawer has a :pre-blank 0 property.
This prevents 'wrong-type-argument wholenump nil' errors in newer Org versions."
    (if (and element (eq (org-element-type element) 'drawer))
        (progn
          (org-element-put-property element :pre-blank 0)
          element)
      element))
  (advice-add 'jupyter-org-results-drawer 
              :filter-return 
              #'my/jupyter-org-results-drawer-pre-blank-fix)

(use-package jupytertoolsrc)

(provide 'jupyterrc)
;;; jupyterrc.el ends here
