;;; spacelinerc.el --- spacelinerc config  -*- lexical-binding: t; -*-

(require 'spaceline-config)
(require 'spaceline-segments)
;(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-buffer-position-off)

(spaceline-define-segment gm/conda-venv
  (format "conda:%s " conda-env-current-name)
  :when (and (bound-and-true-p conda-env-current-name) conda-env-current-name)
  :face 'spaceline-python-venv
  )

(spaceline-define-segment gm/project-root-segment
			  (let ((project-name (project-name (project-current))))
			    (unless (or (string= project-name "-")
					(string= project-name (buffer-name)))
			      (format "proj:%s " project-name))))

(spaceline-toggle-gm/conda-venv-on)
(spaceline-toggle-python-env-off)
(spaceline-emacs-theme '(gm/conda-venv))
