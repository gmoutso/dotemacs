;; company
(use-package company
  :init
  (global-company-mode)
  (add-to-list 'company-backends 'company-anaconda)
  :custom
  (company-idle-delay 2)
  )
