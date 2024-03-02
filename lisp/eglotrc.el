(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                                 . ,(eglot-alternatives
                                     '(
				       ;;("/home/moutsopoulosg/.emacs.d/.cache/lsp/npm/pyright/bin/pyright-langserver" "--stdio")
				       "pylsp"
				       "pyls"
				       "jedi-language-server"
				       ("pyright-langserver" "--stdio")
				       "ruff-lsp"
				       ))))
  )
