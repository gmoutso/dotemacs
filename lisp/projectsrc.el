;;; projectsrc.el --- configs for project management

;;; Commentary:
;; Custom configuration for project management with Projectile.

;;; Code:

(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'helm)
  :config
  (defun gm/workspace-projects (dir)
  "Treat DIR as a project root if it's under my massive Project workspace."
  (let* ((parent-root (expand-file-name "~/spaces/workspace/"))
         (relative-path (file-relative-name dir parent-root)))
    (when (not (string-prefix-p ".." relative-path))
      (let ((subfolder (car (split-string relative-path "/"))))
        (cons 'transient (expand-file-name subfolder parent-root))))))
  (add-hook 'project-find-functions #'gm/workspace-projects)
  (projectile-global-mode)
  )

(load-library "rsync-project")

(provide 'projectsrc)
;;; projectsrc.el ends here
