;; for the good (aka original) perspective mode
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x p"))
  :config
  (persp-mode))

(add-hook 'kill-emacs-hook #'persp-state-save)
