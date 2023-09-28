;; (require 'helm-config)
(require 'general)
(use-package helm
  :custom
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp)
  (helm-scroll-amount 8)
  :config
  (helm-autoresize-mode t)
  (helm-mode nil)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-r" . helm-recentf)
   ("C-x C-b" . helm-mini)
   ("C-x C-l" . helm-locate)
   ("C-x /" . helm-find)
   ("C-h a" . helm-apropos)
   ("C-x C-f" . helm-find-files)
   ("C-x r b" . helm-filtered-bookmarks))
  )

(use-package helm-descbinds
  :config
  (helm-descbinds-mode 1))

(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*epc")
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*anaconda-mode")
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*pyright")
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*lsp-log")
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*mspyls")
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*jupyter-traceback")
(add-to-list 'helm-boring-buffer-regexp-list "TAGS")
;; (add-to-list helm-white-buffer-regexp-list "*jupyter-repl.*")

(use-package helm-projectile
  :init
  (setq helm-projectile-fuzzy-match nil))
(helm-projectile-on)
;; (setq completion-styles `(basic partial-completion emacs22 initials
;;                                 ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

;; (defun my-recentf-show-details (file)
;;   (append
;;    (list
;;     (file-name-nondirectory file))
;;    (list
;;     (format "(in `%s')" (file-name-directory file)))
;;    ))
;; (defun my-recentf-one-by-one-filter (candidate) 
;;          (my-recentf-show-details(candidate) . candidate)) 
;; (helm-make-source "Recentf" 'helm-recentf-source 
;;         :fuzzy-match helm-recentf-fuzzy-match 
;;         :filter-one-by-one #'my-recentf-one-by-one-filter)
;; (helm :sources 'helm-source-recentf
;;         :ff-transformer-show-only-basename nil
;;         :buffer "*helm recentf*")

(general-def helm-buffer-map "M-d" 'helm-buffer-run-kill-persistent)

(setq gm/helm-source-tabspaces-buffers
      (helm-make-source "Workspace Buffers" 'helm-source-buffers
      :buffer-list (lambda () (mapcar 'buffer-name (tabspaces--buffer-list)))))
(defun gm/helm-switch-to-workspace-buffers ()
  (interactive)
  (let ((buffer-list (mapcar 'buffer-name (tabspaces--buffer-list))))
    (helm :sources gm/helm-source-tabspaces-buffers)))
(general-def
  :keymaps 'tabspaces-mode-map
  ;; :prefix "C-c TAB" if without remap
  [remap tabspaces-switch-to-buffer] (cons "tabspace buffer" 'gm/helm-switch-to-workspace-buffers))
(global-set-key  (kbd "C-x <up>") 'gm/helm-switch-to-tab-line-tab-buffer)
