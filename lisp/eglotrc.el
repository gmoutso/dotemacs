;;; eglotrc.el --- Eglot LSP configuration

;;; Commentary:
;; Custom configuration file.

;;; Code:

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
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))

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

(defvar gm/eglot-ensure-projects-names '("py310" "evsim" "evlisp")
  "Which project names to start eglot")
(defun gm/eglot-ensure ()
  (let (project-name (project-name (project-current)))
    (if (member project-name gm/eglot-ensure-projects-names)
	(eglot-ensure))))
(add-hook 'python-mode-hook 'gm/eglot-ensure)
(add-hook 'python-ts-mode-hook 'gm/eglot-ensure)

(provide 'eglotrc)
;;; eglotrc.el ends here
