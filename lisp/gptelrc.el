;;; gptelrc.el --- --- BACKEND 1: GitHub Copilot (Company Official) ---

;;; Commentary:
;; Custom configuration file.

;;; Code:

(use-package gptel
  :ensure t
  :custom-face
  (gptel-response-highlight ((t (:background "#44475a" :foreground "#f8f8f2"))))
  (gptel-context-deletion-face ((t (:background "#ff5555" :foreground "#282a36" :weight bold))))
  (gptel-context-highlight-face ((t (:background "#6272a4" :foreground "#f8f8f2" :slant italic))))
  :config
  (setq gptel-model 'gpt-4o)
  ;; --- BACKEND 1: GitHub Copilot (Company Official) ---
  (defun my/get-copilot-token ()
    (string-trim (shell-command-to-string "gh auth token")))
  ;; (setq my/copilot-backend
  ;;       (gptel-make-github "Copilot"
  ;; 			   :header (lambda () `(("Authorization" . ,(concat "Bearer " (my/get-copilot-token)))))))
  (setq my/copilot-backend
	(gptel-make-openai "GitHub Copilot"
          :host "api.githubcopilot.com"
          :endpoint "/chat/completions"
          :stream t
          :key (lambda ()
                 (string-trim (shell-command-to-string "gh auth token")))
          :models '(gpt-4o gpt-4-turbo)))
  ;; --- BACKEND 2: Gemini (Your AI Studio Key) ---
  (setq my/gemini-backend
        (gptel-make-gemini "Gemini"
          :key (lambda () 
                 (let ((match (car (auth-source-search :host "generators.world" :user "apikey"))))
                   (if-let ((secret (plist-get match :secret)))
                       (if (functionp secret) (funcall secret) secret)
                     (error "Check your .authinfo.gpg!"))))
          :stream t))
  ;; Start with Copilot as default for safety
  (setq gptel-backend my/copilot-backend))


(use-package agent-shell
  :ensure t
  :config
  ;; Load the GitHub-specific module
  (require 'agent-shell-github)

  ;; 1. Point to the ACP-aware command
  ;; Note: Ensure 'gh' is in your PATH and you have the copilot extension installed
  (setq agent-shell-github-acp-command '("gh" "copilot" "chat" "--acp"))

  ;; 2. Optional: If your company uses a specific model or enterprise endpoint
  ;; (setq agent-shell-github-arguments '("--model" "gpt-4o")))
  )

(provide 'gptelrc)
;;; gptelrc.el ends here