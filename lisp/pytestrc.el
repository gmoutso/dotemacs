(add-hook 'python-mode-hook
          (lambda ()
            (when-let ((r (locate-dominating-file default-directory ".pyroot")))
              (setq python-pytest-executable
                    (concat "PYTHONPATH=" r " " "pytest")))))
(add-hook 'python-ts-mode-hook
          (lambda ()
            (when-let ((r (locate-dominating-file default-directory ".pyroot")))
              (setq python-pytest-executable
                    (concat "PYTHONPATH=" r " " "pytest")))))
