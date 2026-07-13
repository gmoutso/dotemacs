;;; eglotrc.el --- Eglot LSP configuration

;;; Commentary:
;; Custom configuration file.

;;; Code:

(use-package eglot
  :custom
  (eglot-max-file-watches nil)
  (eglot-watch-files-outside-project-root nil)
  :init
  (setq eglot-max-file-watches nil
	eglot-watch-files-outside-project-root nil)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))

(defun gm/which-current-eglot-server ()
  (interactive)
  (process-command (jsonrpc--process (eglot-current-server))))

(defvar gm/eglot-ensure-projects-names nil
  "Which project names to start eglot automatically.")
(add-hook 'savehist-save-hook
          (lambda ()
            (add-to-list 'savehist-additional-variables 'gm/eglot-ensure-projects-names)))
(defun gm/eglot-ensure-project-add  ()
  "add current project to known eglot projects."
  (interactive)
  (add-to-list 'gm/eglot-ensure-projects-names (project-root (project-current)))
  (gm/eglot-ensure)
  )
(defun gm/eglot-ensure-project-remove ()
  "remove current project from known eglot projects."
  (interactive)
  (setq gm/eglot-ensure-projects-names (remove (project-root (project-current)) gm/eglot-ensure-projects-names))
  )
(defun gm/eglot-ensure ()
  (let (name (project-root (project-current)))
    (if (member name gm/eglot-ensure-projects-names)
      (eglot-ensure))))
(add-hook 'python-base-mode-hook 'gm/eglot-ensure)

(provide 'eglotrc)
;;; eglotrc.el ends here
