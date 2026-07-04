;;; copilotrc.el --- https://github.com/copilot-emacs/copilot.el

;;; Commentary:
;; Custom configuration file.

;;; Code:

;; https://github.com/copilot-emacs/copilot.el
(use-package quelpa)
(use-package quelpa-use-package)
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :bind (:map copilot-mode-map
	      ("M-C-<return>" . copilot-complete)
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("M-f" . copilot-accept-completion-by-word)
              ("M-n" .  copilot-next-completion)
	      ("M-e" .  copilot-accept-completion-by-sentence)
	      ("M-}" .  copilot-accept-completion-by-paragraph)
              ("M-p" . copilot-previous-completion)
	      )
  :custom
  (copilot-node-executable "/home/moutsopoulosg/anaconda3/envs/test_node/bin/node")
  (copilot-idle-delay nil)
  (copilot-chat-use-agent-mode t)
  (copilot-chat-enable-semantic-search t)
  :config
  ;; Trigger completion manually with M-C-<return> (or choose your own)
  (define-key copilot-mode-map (kbd "M-C-<return>") #'copilot-complete)
  ;; Standard navigation inside the ghost text overlay
  (define-key copilot-mode-map (kbd "<tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "TAB") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "M-f") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") #'copilot-previous-completion)
  (add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python")))

(use-package gh-copilot-chat
  :custom
  (copilot-chat-frontend 'org)
  ;; :quelpa (copilot-chat :fetcher github :repo "chep/copilot-chat.el" :files ("*.el"))
  :bind (:map gh-copilot-chat-prompt-mode-map
              ("C-c C-c" . gh-copilot-chat-prompt-send)))

(provide 'copilotrc)
;;; copilotrc.el ends here