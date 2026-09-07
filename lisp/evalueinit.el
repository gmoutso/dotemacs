;; (load-library "orgmimerc")  ; requires wl ; does not work?  -*- lexical-binding: t; -*-
(require 'fontsrc)
;; (load-library "eafrc")
;; (load-library "activitywatchrc")
;; (load-library "badpersprc")
;; (load-library "goodpersprc")
;; ;; dired
;; (require 'helm)
(require 'diredrc)
;; (load-library "helmrc")
(require 'verticorc)
(require 'consultrc)
(require 'embarkrc) ;; after consult..
(require 'acerc)
(require 'projectsrc)
;; (load-library "cpprc")
;; (load-library "ya-cppref")
;; (setq ya-cppref-path-to-doc-root "/usr/share/cppreference/doc/html/")
;; (load-library "latexrc")
;; (load-library "lsprc")
(use-package ecarc)
(use-package condarc
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/home/moutsopoulosg/anaconda3/bin:/home/moutsopoulosg/anaconda3/condabin"))
  (setq exec-path (append exec-path '("/home/moutsopoulosg/anaconda3/bin" "/home/moutsopoulosg/anaconda3/condabin")))
  :custom
  (conda-anaconda-home "/home/moutsopoulosg/anaconda3/")
  (conda-env-home-directory "/home/moutsopoulosg/anaconda3/")  ; was in separate setq
  )
(use-package pythonrc
  :custom
  (blacken-executable "/home/moutsopoulosg/conda_envs/emacs/bin/black")
  )
(use-package eglotrc
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
(require 'pytestrc)
(use-package pycellrc
  :custom
  (code-cells-convert-ipynb-style
   '(("/home/moutsopoulosg/conda_envs/bastille/bin/jupytext" "--update" "--to" "ipynb")
     ("/home/moutsopoulosg/conda_envs/bastille/bin/jupytext" "--to" "py:percent")
     code-cells--guess-mode code-cells-convert-ipynb-hook))
  )
(require 'orgrc)
(require 'tanglerc)
(require 'evalueoxrc)
(require 'emamuxrc)
;; (load-library "einrc")
(use-package jupyterrc
  :custom
  (jupyter-executable "~/anaconda3/bin/jupyter"))
;; ;; auto-complete
;; (load-library "autocompleterc")
(require 'completerc)
;; ;; zotero integration
;; (load-library "org-zotxt")
(require 'modelinerc)
(require 'diminishrc)
(require 'keys)
(require 'windowsrc)
(require 'hydrarc)
(require 'evrc)
(require 'variousrc)
(require 'imagesrc)
(require 'termrc)
(require 'term-tmuxrc)
(require 'vtermrc)
(require 'eshell-vterm)
(require 'ghostelrc)
(require 'copilotrc)
(require 'agentrc)
(require 'themerc)
