;;;; ecarc.el --- Emacs package for handling ecarc files -*- lexical-binding: t; -*-

;;; Commentary:

;; ecarc.el is an Emacs package that provides functionality for the Editor Code Assistant.
;; https://github.com/editor-code-assistant/eca-emacs

;;; Code:

;; /home/moutsopoulosg/.config/eca/config.json
(use-package eca
  :ensure t
  :defer t
  ;; Bind to the common 'C-c a' (AI) prefix map or global shortcuts
  :bind (("C-c C-a" . hydra-eca/body)
	 ("C-c i" . hydra-eca/body)
	 )      ; Open ECA hydra menu
  ;; :hook ((python-mode . eca)             ; Automatically trigger server for Python
  ;;        (emacs-lisp-mode . eca))        ; Automatically trigger server for Elisp
  :config
  ;; Customise the chat window behavior if desired
  (setq eca-chat-window-side 'right
        eca-chat-window-width 0.35))

(pretty-hydra-define hydra-eca (:exit t :quit-key "q" :title "ECA - AI Assistant")
  ("Chat"
   (
    ;; ("c" eca-chat-open "open/focus chat") requires argument
    ("n" eca-chat-new "new chat")
    ("t" eca-chat-talk "talk (quick prompt)")
    ("s" eca-chat-select "select chat")
    ("R" eca-chat-resume "resume chat")
    ("w" eca-chat-toggle-window "toggle window"))
   "Chat Actions"
   (("C" eca-chat-clear "clear chat")
    ("S" eca-chat-stop-prompt "stop prompt")
    ("m" eca-chat-select-model "select model")
    ("v" eca-chat-select-variant "select variant")
    ("a" eca-chat-cycle-agent "cycle agent")
    ("T" eca-chat-toggle-trust "toggle trust"))
   "Rewrite"
   (("r" eca-rewrite "rewrite region")
    ("y" eca-rewrite-accept "accept rewrite")
    ("x" eca-rewrite-reject "reject rewrite")
    ("d" eca-rewrite-diff "diff rewrite")
    ("M" eca-rewrite-merge "merge rewrite")
    ("c" eca-complete "complete")
    )
   "Server"
   (("e" eca "start eca")
    ("E" eca-stop "stop eca")
    ("!" eca-restart "restart eca")
    ("V" eca-version "version")
    ("D" eca-doctor "doctor"))))

(provide 'ecarc)
;;; ecarc.el ends here
