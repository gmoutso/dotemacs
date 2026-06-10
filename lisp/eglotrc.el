(use-package eglot
  :custom
  (eglot-max-file-watches nil)
  (eglot-watch-files-outside-project-root nil)
  :init
  (setq eglot-max-file-watches nil
	eglot-watch-files-outside-project-root nil)
  :config
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                                 . ,(eglot-alternatives
                                     '(
				       ("/home/moutsopoulosg/conda_envs/emacs/bin/basedpyright-langserver" "--stdio")
				       ("pyright-langserver" "--stdio")
				       "~/anaconda3/envs/pylsp/bin/pylsp"
				       "pyls"
				       "jedi-language-server"
				       "ruff-lsp"
				       ))))
  )

(defun gm/which-current-eglot-server ()
  (interactive)
  (process-command (jsonrpc--process (eglot-current-server))))

(defun gm/eglot-project-one-level-down (dir)
  "Treat DIR as a project root if it's under my massive Project workspace."
  (let* ((parent-root (expand-file-name "~/spaces/workspace/"))
         (relative-path (file-relative-name dir parent-root)))
    (when (not (string-prefix-p ".." relative-path))
      (let ((subfolder (car (split-string relative-path "/"))))
        (cons 'transient (expand-file-name subfolder parent-root))))))
(add-hook 'project-find-functions #'gm/eglot-project-one-level-down)
;; (setq-default eglot-workspace-configuration
;;               '((:pyright . (:analysis (:autoSearchPaths t
;;                                         :useLibraryCodeForTypes t
;;                                         :diagnosticMode "openFilesOnly")))))
