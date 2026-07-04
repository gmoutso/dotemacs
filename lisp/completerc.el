;;; completerc.el --- load yas company etc           -*- lexical-binding: t; -*-

;;; Code:


;; Copyright (C) 2017  George Moutsopoulos

;; Author: George Moutsopoulos <moutsopoulosg@evaluex003>

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; company
;; (use-package company
;;   :init
;;   (global-company-mode)
;;   (add-to-list 'company-backends 'company-anaconda)
;;   :custom
;;   (company-idle-delay 2)
;;   )


(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword) # too limited
  (add-hook 'completion-at-point-functions #'cape-tex)
    )

;; cape-abbrev: Complete abbreviation (add-global-abbrev, add-mode-abbrev).
;; cape-dabbrev: Complete word from current buffers. See also dabbrev-capf.
;; cape-dict: Complete word from dictionary file.
;; cape-elisp-block: Complete Elisp in Org or Markdown code block.
;; cape-elisp-symbol: Complete Elisp symbol.
;; cape-emoji: Complete Emoji.
;; cape-file: Complete file name.
;; cape-history: Complete from Eshell, Comint or minibuffer history.
;; cape-keyword: Complete programming language keyword.
;; cape-line: Complete entire line from current buffer.
;; cape-rfc1345: Complete Unicode char using RFC 1345 mnemonics.
;; cape-sgml: Complete Unicode char from SGML entity, e.g., &alpha.
;; cape-tex: Complete Unicode char from TeX command, e.g. \hbar.

(provide 'completerc)
;;; completerc.el ends here