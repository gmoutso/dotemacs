(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Projectile
;;
;; Remember needs a .projectile at root folder of each project.
;; Projectile is useful for jumping files, searching symbols, etc
;; in a project folder hierarchy and costs nothing
;;
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(add-to-list 'projectile-globally-ignored-directories "html")
(add-to-list 'projectile-globally-ignored-directories "tmp")
;; do not show projectile always on modeline if not in a project
(setq projectile-mode-line
      '(:eval (if (projectile-project-p)
                  (format " Proj[%s]"
                          (projectile-project-name))
                "")))

;; CEDET
;;
(global-ede-mode t)
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(global-semantic-idle-local-symbol-highlight-mode 1)
(global-semantic-idle-summary-mode 1)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
;(require 'semantic/bovine/clang)

;; if you want to enable support for gnu global
;(when (cedet-gnu-global-version-check t)
;  (semanticdb-enable-gnu-global-databases 'c-mode)
;  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;(when (cedet-ectag-version-check)
;  (semantic-load-enable-primary-exuberent-ctags-support))

(defun my-semantic-ia-fast-jump-other-window ()
  "Jump to the tag referred to by the code at POINT by opening other window first. Uses semantic-ia-fast-jump."
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (semantic-ia-fast-jump (point)))

; combined with eval-after-load to delay evaluation until the relevant keymap is loaded
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map   (kbd "C-c i j") 'my-semantic-ia-fast-jump-other-window)
     (define-key c++-mode-map (kbd "C-c i j") 'my-semantic-ia-fast-jump-other-window)
     (define-key c++-mode-map (kbd "C-c i t") 'semantic-analyze-proto-impl-toggle))
  )


;; Show main source buffer when using GDB
(setq gdb-show-main t)

;;
;; FlyCheck
;;
;; c++ complete and checking
;(eval-after-load "flycheck" (lambda () (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; insert include paths
(defun setup-flycheck-cpp-projectile-path ()
  (interactive)
  (let ((root (concat (ignore-errors (projectile-project-root)) "include/")))
    (when (file-exists-p root)
      (add-to-list
       (make-variable-buffer-local 'flycheck-gcc-include-path)
       root))))
(add-hook 'c++-mode-hook 'setup-flycheck-cpp-projectile-path)
;(global-flycheck-mode 1)
