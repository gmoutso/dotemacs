(require 'auth-source)
(let* ((auth (car (auth-source-search
	       :host "gemini")))
       (password (funcall (plist-get auth :secret))))
  (setq gptel-gemini-key password)
  )
(add-to-list 'load-path "~/.emacs.d/lisp/gptel")
(use-package gptel)
(use-package gptel-gemini)
(use-package gptel-transient)
(use-package gptel-curl)

(setq
 gptel-model "gemini-pro"
 gptel-backend (gptel-make-gemini "Gemini"
                 :key gptel-gemini-key
                 :stream t))
