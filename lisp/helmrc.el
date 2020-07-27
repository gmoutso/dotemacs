(require 'helm-config)
(require 'general)

(setq ; helm-quick-update t ; obsolete? do not display invisible candidates
      ; helm-split-window-inside-p t ; split inside window
      ; helm-always-two-windows t ;display two windows in one frame
      ; helm-autoresize-mode t;  uses two windows in this frame.
      ; helm-split-window-default-side 'below ; 'same below above left right
      ; helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount 8
      ;helm-ff-file-name-history-use-recentf t ; history action in helm-find-files 
      )
(helm-mode)
(helm-descbinds-mode)
(helm-autoresize-mode)
(add-to-list 'helm-boring-buffer-regexp-list "*epc")
(add-to-list 'helm-boring-buffer-regexp-list "*anaconda-mode*")
(add-to-list 'helm-boring-buffer-regexp-list "TAGS")
(require 'helm-projectile)
(helm-projectile-on)
(setq completion-styles `(basic partial-completion emacs22 initials
                                ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

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
