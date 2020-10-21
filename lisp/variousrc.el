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

;; output of (org-babel-get-src-block-info)
;; ("jupyter-python" "import pandas as pd
;; " ((:colname-names) (:rowname-names) (:result-params "replace") (:result-type . value) (:results . "replace") (:exports . "code") (:noweb . "yes") (:comments . "yes") (:tangle . "yes") (:kernel . "conda-env-blade-py") (:session . "/jpy:localhost#8888:dynacomp") (:pandoc . "t") (:async . "yes") (:hlines . "no") (:cache . "no") (:eval . "never-export")) "" nil 697 "(ref:%s)")
;; output of (org-babel-tangle-comment-links)
;; ("[[[[file:~/workspace/moutsopoulosg/drawdownchar/JackVar/dynacomp.org::*Summary][Summary]]][]]" " ends here")

