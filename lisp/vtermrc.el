(use-package vterm
    :bind (:map vterm-mode-map ("C-y" . vterm-yank))
    :config (setq vterm-max-scrollback 100000))
(use-package multi-vterm)
