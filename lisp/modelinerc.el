;; ;; Powerline
;; (require 'powerline)
;; (global-visual-line-mode 1) ; 1 for on, 0 for off.
;; (powerline-default-theme)
;; (set-face-foreground 'powerline-active1 "white")
;; (set-face-foreground 'mode-line "dark orange")

;; smart mode line
(require 'spaceline-config)
(require 'spaceline-segments)
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

(spaceline-define-segment gm/conda-venv
  (format "conda:%s " conda-env-current-name)
  :when (and (bound-and-true-p conda-env-current-name) conda-env-current-name)
  :face 'spaceline-python-venv
  )
(spaceline-define-segment gm/projectile-root
  (let ((project-name (projectile-project-name)))
      (unless (or (string= project-name "-")
                  (string= project-name (buffer-name)))
        (format "proj:%s " project-name)))
  :when (fboundp 'projectile-project-name))
(spaceline-toggle-gm/conda-venv-on)
(spaceline-toggle-python-env-off)
(spaceline-emacs-theme '(gm/conda-venv 'gm/projectile-root))

;; diminish: hide some minor modes
(require 'delight)
(delight '(
	    (abbrev-mode " Abv" abbrev)
;           (smart-tab-mode " \\t" smart-tab)
;           (eldoc-mode nil "eldoc")
;           (rainbow-mode)
;           (overwrite-mode " Ov" t)
(emacs-lisp-mode "Elisp" :major)
;; (my-keys-mode " mK" "keys")
(my-keys-mode nil "keys")
(visual-line-mode nil "simple")
(buffer-face-mode nil "face-remap")
;; (org-cdlatex-mode " tex" "org")
(org-cdlatex-mode nil "org")
(undo-tree-mode nil "undo-tree")
(helm-mode nil "helm-mode")
(company-mode nil "company")
(which-key-mode nil "which-key")
(org-z-mode nil "org-z")
(activity-watch-mode nil)
(yas-minor-mode nil t)
(yas-global-mode nil t)
(eldoc-mode nil "eldoc")
(projectile-mode nil "projectile")
))

;; (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
;;   "Ensure that powerline's major mode names are delighted."
;;   (let ((inhibit-mode-name-delight nil))
;;     ad-do-it))


(defun gm/projectile-default-mode-line ()
  "Report project name and type in the modeline."
  (let ((project-name (projectile-project-name))
        (project-type (projectile-project-type)))
    (format "%s[%s]"
            projectile-mode-line-prefix
            (or project-name "-"))))
(setq projectile-mode-line-function 'gm/projectile-default-mode-line)


;; mode-icons
;(setq mode-icons-change-mode-name nil)
;(require 'mode-icons)
;(mode-icons-mode 1)
