(conda-env-activate "emacs")
;; use (jupyter-command "kernelspec" "list" "--json" "--log-level=40")
;; /home/moutsopoulosg/.emacs.d/elpa/jupyter-20220419.1852/jupyter-kernelspec.el:64
(use-package jupyter
  :after (ob-jupyter ob-python)
  :config
  (setq jupyter-api-authentication-method 'password)
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

;;
;; jupyter repl
;;
(general-def jupyter-repl-interaction-mode-map
  "C-c C-p" 'jupyter-repl-pop-to-buffer)

;; https://github.com/nnicandro/emacs-jupyter/issues/366
;; garbled errors
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)


;;
;; helm REPLs for python buffers
;;
(defun gm/helm-candidates-get-jupyter-server-kernel-list ()
  "Get alist of live kernel names to kernel ids"
    (with-helm-current-buffer
(let ((server (jupyter-current-server)))
(cl-loop
   with names = nil
   for kernel across (jupyter-api-get-kernel server)
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
            (state execution_state))
       (cons name id)))))))
(defun gm/helm-action-ask-for-repl-name () (read-string "REPL Name: " nil nil (buffer-name)))
(defun gm/helm-action-jupyter-server-kernel-repl (id)
  (jupyter-connect-server-repl (jupyter-current-server) id
			       (gm/helm-action-ask-for-repl-name) t nil t))
(setq gm/helm-source-jupyter-server-kernel-list
      (helm-build-sync-source
	  "Kernel list"
	:candidates 'gm/helm-candidates-get-jupyter-server-kernel-list
	:action 'gm/helm-action-jupyter-server-kernel-repl))
(defun gm/helm-candidates-get-jupyter-server-specs ()
  "Get alist of kernel spec display names to kernel environment names"
    (with-helm-current-buffer
(let* ((server (jupyter-current-server current-prefix-arg))
       (specs (jupyter-server-kernelspecs server)))
  (cl-loop with names = nil
	for spec in specs
	collect
	(cons (plist-get (cddr spec) :display_name) (car spec))))))
(defun gm/helm-action-jupyter-server-spec-repl (kernel)
  (jupyter-run-server-repl (jupyter-current-server current-prefix-arg) kernel
			   (gm/helm-action-ask-for-repl-name) t nil t))
(setq gm/helm-source-jupyter-server-spec-list
      '((name . "Spec list")
        (candidates . gm/helm-candidates-get-jupyter-server-specs)
	(action . gm/helm-action-jupyter-server-spec-repl)))
(defun gm/helm-candidates-jupyter-get-repls ()
  "Get ALIST of buffer names to buffers of live repls."
  (with-helm-current-buffer
  (let ((buffers (jupyter-repl-available-repl-buffers major-mode)))
    (mapcar (lambda (b) (cons (buffer-name b) b)) buffers))
  ))
(defun gm/helm-action-jupyter-server-repl-associate (buffer)
  (let ((client (buffer-local-value 'jupyter-current-client buffer)))
  (jupyter-repl-associate-buffer client)
  ))
(setq gm/helm-source-jupyter-server-repl-list
      (helm-build-sync-source
	  "REPLs"
	:candidates 'gm/helm-candidates-jupyter-get-repls
	:action 'gm/helm-action-jupyter-server-repl-associate))
(defun gm/jupyter-kernels ()
  (interactive)
  (helm :sources '(gm/helm-source-jupyter-server-repl-list
		   gm/helm-source-jupyter-server-spec-list
		   gm/helm-source-jupyter-server-kernel-list
		   )))
