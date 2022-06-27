(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  ;; do not make default readers:
  :config
  (advice-remove 'dired-find-file 'eaf--dired-find-file-advisor)
  (advice-remove 'dired-find-alternate-file 'eaf--dired-find-file-advisor)
  (advice-remove 'find-file 'eaf--find-file-advisor)
  )
(use-package eaf-browser
  :after eaf
  ;; :config
  ;; (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
  )
(use-package eaf-pdf-viewer
  :after eaf
  ;; :config
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  )
