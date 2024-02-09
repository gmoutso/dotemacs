;; lsprc -- Summary
;;; Commentary:
;; None
;; Code

;;
;; lsp
;;
(use-package lsp-mode
  :hook
  (python-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-python-ms
  :custom
  (lsp-python-ms-extra-paths 
   '("/home/moutsopoulosg/dev/master/python"
     "/home/moutsopoulosg/anaconda3/envs/blade/lib/python2.7"
     "/home/moutsopoulosg/anaconda3/envs/blade/lib/python2.7/site-packages"
     )))

(use-package lsp-pyright
  ;; :custom
  ;; (lsp-pyright-multi-root nil)
  )

;; (add-to-list 'lsp-disabled-clients 'pyright)

(use-package lsp-ui
  ;; :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (focus-follows-mouse nil)
  )


(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
