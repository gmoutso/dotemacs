(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-math)
(require 'yasnippet)
(yas-global-mode 1)
(global-auto-complete-mode t)

;; spelling with FlySpell
;;
(dolist (mode '(org-mode-hook LaTeX-mode-hook text-mode-hook))
  (add-hook mode 'turn-on-flyspell))
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

(add-to-list 'ac-modes 'latex-mode) 
(add-to-list 'ac-modes 'org-mode)

(defun ac-LaTeX-org-mode-setup ()
(setq ac-sources
(append '(ac-source-math-latex-tags ac-source-latex-commands)
 ac-sources)) )

;; autocomplete for C++
;;
(defun ac-c-header-setup ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories "/usr/include")
  )
(add-hook 'c++-mode-hook 'ac-c-header-setup)

(defun my:add-semantic-to-ac()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-ac)

;(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(add-hook 'org-mode-hook 'ac-LaTeX-org-mode-setup)

;(setq ac-math-unicode-in-math-p t)
(defun ac-complete-math ()
  (interactive)
  (auto-complete '(ac-source-math-latex-tags ac-source-latex-commands)))
;(setq ac-auto-start nil)
(setq ac-delay 0.1)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, prcessing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will. yasnippet must be loaded first
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")
;; Remove Yasnippet's default tab key binding
;(define-key yas-minor-mode-map (kbd "<tab>") nil)
;(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
;(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

