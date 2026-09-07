;;; ghostelrc.el --- Hydra configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom configuration file.

;;; Code:


(use-package ghostel
  :bind (
	 ("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

;; Make eshell-visual-commands run in a Ghostel buffer.
(use-package ghostel-eshell
  :hook (eshell-load . ghostel-eshell-visual-command-mode))
;; Run all compile commands in a Ghostel buffer.
(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))
;; Replace comint's built-in ansi-color-process-output with Ghostel's VT parser.
(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

;; directory tracking works out of the box!
;; if [[ "$INSIDE_EMACS" = 'ghostel' ]]; then
;;     # Open a file in Emacs from the terminal
;;     e()   { ghostel_cmd find-file-other-window "$@"; }

;;     # Open dired in another window
;;     dow() { ghostel_cmd dired-other-window "$@"; }

;;     # Open magit for the current directory
;;     gst() { ghostel_cmd magit-status-setup-buffer "$(pwd)"; }
;; fi

(provide 'ghostelrc)
;;; ghostelrc.el ends here
