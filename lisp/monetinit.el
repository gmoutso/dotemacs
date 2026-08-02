(require 'fontsrc)
(require 'diredrc)
(require 'verticorc)
(require 'consultrc)
(require 'embarkrc)
(require 'projectsrc) ;; project-root etc not autoloaded.
;; (require 'acerc) too much helm
(use-package variousrc)
;; (use-package condarc
;;   :custom
;;   (conda-anaconda-home "/home/moutsopoulosg/miniforge3/")
;;   )
(use-package eglotrc
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                `((python-mode python-ts-mode)
;;                                  . ,(eglot-alternatives
;;                                      '(
;; ("/home/moutsopoulosg/conda_envs/emacs/bin/basedpyright-langserver" "--stdio")
;; 				       ("pyright-langserver" "--stdio")
;; 				       "~/anaconda3/envs/pylsp/bin/pylsp"
;; 				       "pyls"
;; 				       "jedi-language-server"
;; 				       "ruff-lsp"
;; 				       ))))
  )
(use-package pythonrc
  ;; :custom
  ;; (blacken-executable "/home/moutsopoulosg/conda_envs/emacs/bin/black")
  )
(use-package pycellrc
  ;; :custom
  ;; (code-cells-convert-ipynb-style
  ;;  '(("/home/moutsopoulosg/conda_envs/bastille/bin/jupytext" "--update" "--to" "ipynb")
  ;;    ("/home/moutsopoulosg/conda_envs/bastille/bin/jupytext" "--to" "py:percent")
  ;;    code-cells--guess-mode code-cells-convert-ipynb-hook))
  )
(require 'general)
;; lisp configuration files
;; (require 'helmrc)
(require 'keys)
(require 'windowsrc)
(require 'diminishrc)
(require 'hydrarc)
(require 'imagesrc)
(require 'latexrc)
;; (require 'texify)
(use-package jupyterrc
  :custom
  (jupyter-executable "~/miniforge3/bin/jupyter-lab"))
(require 'completerc)
(require 'copilotrc)
(require 'orgrc)
(require 'tanglerc)
(require 'ghostelrc)
;; (use-package ecarc)
(require 'themerc)

;; Install and configure password-store
(use-package password-store-menu)

