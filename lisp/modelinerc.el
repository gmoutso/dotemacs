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
