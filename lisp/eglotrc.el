(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) "pyright-langserver" "--stdio"))
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                                 . ,(eglot-alternatives
                                     '(
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
