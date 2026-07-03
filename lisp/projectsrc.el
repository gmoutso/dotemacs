;;; projectsrc.el --- configs for project management
(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'helm)
  :config
  (projectile-global-mode)
  )

(load-library "rsync-project")

(provide 'projectsrc)
;;; projectsrc.el ends here
