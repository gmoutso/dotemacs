;; lsprc -- Summary
;;; Commentary:
;; None
;; Code

;;
;; lsp
;;


(use-package lsp-mode
  ;; :hook
  ;; (python-base-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration))

(use-package lsp-python-ms
  :custom
  (lsp-python-ms-extra-major-modes '(python-ts-mode))
  (lsp-python-ms-python-executable "~/anaconda3/envs/banks/bin/python")
  )

(use-package lsp-pyright
  :custom
  ;; (lsp-pyright-multi-root nil)
  (lsp-pyright-python-executable-cmd (expand-file-name "~/anaconda3/envs/emacs/bin/python"))
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

;; to make lsp work in eg org
;; fixes error: "lsp is not a valied checker"
;; (flycheck-define-generic-checker 'lsp
;;     "A syntax checker using the Language Server Protocol (LSP)
;; provided by lsp-mode.
;; See https://github.com/emacs-lsp/lsp-mode."
;;     :start #'lsp-diagnostics--flycheck-start
;;     :modes '(lsp-placeholder-mode) ;; placeholder
;;     :predicate (lambda () lsp-mode)
;;     :error-explainer (lambda (e)
;;                        (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
;;                               (lsp-cpp-flycheck-clang-tidy-error-explainer e))
;;                              (t (flycheck-error-message e)))))
