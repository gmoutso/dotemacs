(use-package general)
;; use helm for complete
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (eshell-cmpl-initialize)
;;             (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;;             ))

;; (general-def eshell-mode-map
;;  "M-d" 'helm-eshell-history)

;; (general-define-key
;;  :keymaps 'eshell-mode-map
;;  [remap eshell-pcomplete] 'helm-esh-pcomplete)
