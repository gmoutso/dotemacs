(defun gm-helm-gtags--create-autoimport (cand)
  (let (filename (helm-gtags--extract-file-and-line cand))
    cand))

;; (helm-gtags--base-directory)

(defvar gm-helm-source-gtags-tags
  (helm-build-in-buffer-source "Jump to definitions"
    ;; :init 'helm-gtags--tags-init
    ;; :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    ;; :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action 'gm-helm-gtags--create-autoimport))


(helm-gtags-find-tag 'tag)
(list (helm-gtags--read-tagname 'tag)))
(helm-gtags--common '(gm-helm-source-gtags-tags) "get_mdb")

(use-package lsp-mode
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp))
