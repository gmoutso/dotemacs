;; for the bad persp-mode

(defvar persp-mode-functions-to-advise
    '(next-buffer
      previous-buffer
      helm-mini
      helm-imenu-in-all-buffers
      helm-multi-occur-all)
    "List of functions which need additional advising when using `persp-mode'.")

  (defun persp-mode-wrapper (wrapped-buffer-command &rest r)
    "Wrapper for commands which need advising for use with `persp-mode'.
Only for use with `advice-add'."
    (with-persp-buffer-list () (apply wrapped-buffer-command r)))

  (defun persp-mode-setup-advice ()
    "Adds or removes advice on functions in `persp-mode-functions-to-advise'."
    (cl-loop for func in persp-mode-functions-to-advise
             do (if persp-mode
                    (advice-add func :around #'persp-mode-wrapper)
                  (advice-remove func #'persp-mode-wrapper))))

  (add-hook 'persp-mode-hook #'persp-mode-setup-advice)
