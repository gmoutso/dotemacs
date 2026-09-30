;;; projectsrc.el --- configs for project management  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom configuration for project management with Projectile.

;;; Code:

;; (use-package projectile
;;   :bind
;;   (:map projectile-mode-map
;;    ("C-c p" . projectile-command-map))
;;   :custom
;;   (projectile-completion-system 'helm)
;;   :config
;;   (projectile-global-mode)
;;   )

;; evalue specific staff should go into evrc.el

(load-library "rsync-project")

(use-package project
  :bind (:map project-prefix-map
              ("v" . magit-project-status))
  :config
  ;; Update the 'C-x p p' switch project menu option for 'v'
  (setq project-switch-commands
        (mapcar (lambda (entry)
                  (if (eq (car entry) 'project-vc-dir)
                      '(magit-project-status "Magit")
                    entry))
                project-switch-commands)))

(provide 'projectsrc)
;;; projectsrc.el ends here
