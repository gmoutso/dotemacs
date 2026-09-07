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

(provide 'projectsrc)
;;; projectsrc.el ends here
