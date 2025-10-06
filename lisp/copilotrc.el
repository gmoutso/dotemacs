;; https://github.com/copilot-emacs/copilot.el
(use-package quelpa)
(use-package quelpa-use-package)
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :custom
  (copilot-node-executable "/home/moutsopoulosg/anaconda3/envs/test_node/bin/node"))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python"))


(defun gm/new-tab-copilot-chat (&rest args)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "copilot"))

(use-package copilot-chat
  :quelpa (copilot-chat :fetcher github :repo "chep/copilot-chat.el" :files ("*.el"))
  :custom
  (copilot-chat-frontend 'org)
  :config
   ;(advice-add 'copilot-chat-display :before 'gm/new-tab-copilot-chat)
  :bind (
	 :map copilot-chat-prompt-mode-map
	      ("C-c C-c" . copilot-chat-prompt-send)
	      )
  )
