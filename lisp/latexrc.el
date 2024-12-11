;; only start server for okular comms when in latex mode
(add-hook 'LaTeX-mode-hook 'server-start)

;; use pdflatex instead of latex
(setq TeX-PDF-mode t)

;; fix emacs problem on not parsing pdflatex errors
; temp change
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;; Standard emacs/latex config
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

;; cdlatex load
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

; enable auto-fill mode, nice for text
;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
;; do not ask to start server
;; (setq TeX-source-specials-view-start-server t)
;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called
; temp change
;(custom-set-variables '(LaTeX-command "latex -synctex=1") )

;; autosave
(setq TeX-save-query nil) ;;autosave before compiling
;;The following makes C-c-c not ask, Adds C-c-a for asking
;; (setq TeX-command-force "")
;; (add-hook 'LaTeX-mode-hook
;; '(lambda()
;; (define-key LaTeX-mode-map "C-c C-a" ; 'a' for ask, change to anything you want
;; (lambda (arg) (interactive "P")
;; (let ((TeX-command-force nil))
;; (TeX-command-master arg))))))

;; we always use master files
(setq-default TeX-master t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Okular as the pdf viewer
;; (setq TeX-view-program-selection
;;  '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;(defun pdfokular ()
;   (add-to-list 'TeX-output-view-style
;                 '("^pdf$" "." "okular %o %(outpage)")))
;(add-hook  'LaTeX-mode-hook  'pdfokular  t) ; AUCTeX LaTeX mode

;; ;; Latex Autocomplete
;; ;(autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
;; ;(add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; ;(yas/initialize)
;; ;; default TAB key is occupied by auto-complete
;; (global-set-key (kbd "C-o") 'yas/expand)
;; ;; default hotkey C-c &amp; C-s is still valid
;; ;(global-set-key (kbd "C-c ; s") 'yas/insert-snippet)
;; ;; give yas/dropdown-prompt in yas/prompt-functions a chance
;; ;(require 'dropdown-list)

;; (require 'auto-complete-config)
;; (require 'auto-complete-auctex)
;; (load-library "texify")
