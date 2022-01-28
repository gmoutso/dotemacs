;; ;; Powerline
;; (require 'powerline)
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.
;; (powerline-default-theme)
;; (set-face-foreground 'powerline-active1 "white")
;; (set-face-foreground 'mode-line "dark orange")

;; smart mode line
(require 'spaceline-config)
(require 'spaceline-segments)
(spaceline-emacs-theme)
;(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-buffer-position-off)
;; (setq spaceline-face-func '(cond
;;      ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
;;      ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
;;      ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
;;      ((eq 'highlight face) (if active    
;;                                (funcall spaceline-highlight-face-func)
;;                              'powerline-inactive1))))
; (set-face-foreground 'powerline-active1 "white")

;; diminish: hide some minor modes
(require 'delight)
(delight '(
	    (abbrev-mode " Abv" abbrev)
;           (smart-tab-mode " \\t" smart-tab)
;           (eldoc-mode nil "eldoc")
;           (rainbow-mode)
;           (overwrite-mode " Ov" t)
(emacs-lisp-mode "Elisp" :major)
(my-keys-mode " mK" "keys")
(visual-line-mode nil "simple")
(buffer-face-mode nil "face-remap")
(org-cdlatex-mode " tex" "org")
(undo-tree-mode nil "undo-tree")
(helm-mode nil "helm-mode")
))
;; (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
;;   "Ensure that powerline's major mode names are delighted."
;;   (let ((inhibit-mode-name-delight nil))
;;     ad-do-it))


;; mode-icons
;(setq mode-icons-change-mode-name nil)
;(require 'mode-icons)
;(mode-icons-mode 1)
