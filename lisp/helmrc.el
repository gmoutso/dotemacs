;;; helmrc.el --- ;; helm configuration

;;; Commentary:
;; Custom configuration file.

;;; Code:


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

;;
;; tab-line-mode
;;
(defun gm/tab-line-bury-marked-buffers-action (_ignore)
  (let* ((bufs (helm-marked-candidates))
         (killed-bufs (cl-count-if 'bury-buffer bufs)))
    (when (buffer-live-p helm-buffer)
      (with-helm-buffer
        (setq helm-marked-candidates nil
              helm-visible-mark-overlays nil)))
    (message "Bury %s buffer(s)" killed-bufs)))
(defun gm/tab-line-bury-marked-buffers-run-action ()
  "Run bury buffer action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'gm/tab-line-bury-marked-buffers-action)))
(put 'gm/tab-line-bury-marked-buffers-run-action 'helm-only t)
(defclass gm/helm-source-tab-line-buffers (helm-source-buffers) ())
(defun gm/helm-switch-to-tab-line-tab-buffer ()
    (interactive) 
    (let* ((candidates (gm/tab-line-buffer-names)) ;; note needs to call this outside helm
	   (source (helm-make-source "Window buffers" 'gm/helm-source-tab-line-buffers
		     :buffer-list (lambda () candidates)
		     :action (helm-make-actions
			      "Bury buffers" 'gm/tab-line-bury-marked-buffers-action))))
      (helm-add-action-to-source "Bury buffers" 'gm/tab-line-bury-marked-buffers-action source)
      (helm :sources source)))
(global-set-key  (kbd "C-x <up>") 'gm/helm-switch-to-tab-line-tab-buffer)

(provide 'helmrc)
;;; helmrc.el ends here
