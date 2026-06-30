;;; verticorc.el --- Complete Vertico/Consult/Embark configuration

;;; Commentary:
;; This is a complete replacement for Helm functionality using the
;; Vertico/Marginalia/Orderless/Embark/Consult stack.

;;; Code:

;; ============================================================================
;; Core Vertico Configuration
;; ============================================================================

(use-package vertico
  :custom
  (vertico-cycle t)                     ; Cycle at top/bottom
  (vertico-resize nil)                  ; Fixed height
  (vertico-count 15)                    ; Show 15 candidates
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("?" . minibuffer-completion-help))
  :config
  ;; Enable vertico-multiform for per-command/category display
  (vertico-multiform-mode)
  
  ;; Configure display per command
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-line buffer)
          (execute-extended-command unobtrusive)))
  
  ;; Configure display per completion category
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer))))

;; ;; Option 1: Additional bindings
;; (keymap-set vertico-map "?" #'minibuffer-completion-help)
;; ;; (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
;; ;; (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
;; ;; Option 2: Replace `vertico-insert' to enable TAB prefix expansion.
;; (keymap-set vertico-map "TAB" #'minibuffer-complete)


;; ============================================================================
;; Marginalia - Rich annotations in minibuffer
;; ============================================================================

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; ============================================================================
;; Orderless - Powerful completion style
;; ============================================================================

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Allow escaping space with backslash
  (orderless-component-separator #'orderless-escapable-split-on-space)
 ;; (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring					;
  )

;; ============================================================================
;; Consult - Powerful search and navigation commands
;; ============================================================================

(use-package consult
  :demand t
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x C-r" . consult-recent-file)
         
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         
         ;; Other bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("C-h a" . consult-apropos))
  
  :hook (completion-list-mode . consult-preview-at-point-mode)
  
  :init
  ;; Optionally configure the register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  
  ;; Optionally tweak the register preview window
  (advice-add #'register-preview :override #'consult-register-window)
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  ;; Optionally configure preview
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")
  
  ;; Configure buffer filtering (replaces helm-boring-buffer-regexp-list)
  (setq consult-buffer-filter
        '("\\` "
          "\\`\\*Completions\\*\\'"
          "\\`\\*Flymake log\\*\\'"
          "\\`\\*Semantic SymRef\\*\\'"
          "\\`\\*tramp/.*\\*\\'"
          "\\`\\*epc"
          "\\`\\*anaconda-mode"
          "\\`\\*pyright"
          "\\`\\*lsp-log"
          "\\`\\*mspyls"
          "\\`\\*jupyter-traceback"
          "\\`TAGS\\'")))

;; ============================================================================
;; Embark - Contextual actions on completion candidates
;; ============================================================================

(use-package embark
  :bind
  (
   ("C-." . embark-act)
   ;("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark integration with Consult
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; Savehist - Persist history for better sorting
;; ============================================================================

(use-package savehist
  :init
  (savehist-mode))

;; ============================================================================
;; Minibuffer settings
;; ============================================================================

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode 1))

;; ============================================================================
;; Projectile Integration
;; ============================================================================

(use-package consult-project-extra
  :custom (consult-project-function #'consult-project-extra-project-fn) ;; Optional but recommended for a more consistent UI
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;; ============================================================================
;; Custom Functions - Helm Replacements
;; ============================================================================

;; Org images navigation (replacement for helm-org-images)
(defun gm/consult-org-images ()
  "Jump to image links in current org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (consult-line "\\[\\[file:.*\\.\\(png\\|jpg\\|jpeg\\|gif\\)\\]"))

;; Python definitions (simplified replacement for helm-occur-pydef)
(defun gm/consult-python-defs ()
  "Jump to Python definitions (def/class) in current buffer."
  (interactive)
  (consult-line "^\\s-*\\(def\\|class\\|async def\\) "))

(defun gm/consult-python-defs-all ()
  "Jump to Python definitions across all project buffers."
  (interactive)
  (consult-line-multi '((lambda () (project-buffers (project-current))))
                      "^\\s-*\\(def\\|class\\|async def\\) "))

;; Better alternative: use consult-imenu for Python
(defun gm/consult-python-symbols ()
  "Navigate Python symbols using imenu (better than grep)."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (consult-imenu)
    (user-error "Not in a Python buffer")))

;; ============================================================================
;; Additional Consult Sources Configuration
;; ============================================================================

;; Customize consult-buffer sources
;; (with-eval-after-load 'consult
;;   ;; Add hidden buffers source (narrowed with 'h')
;;   (defvar consult--source-hidden-buffer
;;     `(:name "Hidden Buffer"
;;       :narrow ?h
;;       :category buffer
;;       :face consult-buffer
;;       :history buffer-name-history
;;       :state ,#'consult--buffer-state
;;       :items ,(lambda ()
;;                 (consult--buffer-query
;;                  :sort 'visibility
;;                  :as #'buffer-name
;;                  :predicate (lambda (buf)
;;                               (string-prefix-p " " (buffer-name buf))))))
;;     "Hidden buffer candidate source for `consult-buffer'.")
  
;;   ;; Reorder consult-buffer sources if needed
;;   (setq consult-buffer-sources
;;         '(consult--source-hidden-buffer
;;           consult-source-modified-buffer
;; 	  consult-source-buffer
;; 	  consult-source-recent-file
;; 	  consult-source-file-register
;; 	  consult-source-bookmark
;; 	  consult-source-project-buffer-hidden
;; 	  consult-source-project-recent-file-hidden)))

;; ============================================================================
;; Helpful Keybindings Summary
;; ============================================================================

;; In minibuffer with vertico:
;;   C-j/C-k or ↑/↓  - Navigate candidates
;;   RET             - Select candidate
;;   TAB             - Complete prefix (if available)
;;   C-.             - Embark act (context actions)
;;   C-;             - Embark dwim (smart action)
;;   M-.             - Preview (in consult commands)
;;   M-A             - Cycle marginalia annotations
;;   ?               - Help

;; Main commands:
;;   C-x b           - consult-buffer (replaces helm-mini)
;;   C-x C-r         - consult-recent-file (replaces helm-recentf)
;;   C-x r b         - consult-bookmark (replaces helm-filtered-bookmarks)
;;   M-x             - Just use M-x with vertico (replaces helm-M-x)
;;   C-x C-f         - find-file with vertico (replaces helm-find-files)
;;   M-s l           - consult-line (better than helm-occur)
;;   M-s r           - consult-ripgrep (better than helm-projectile-grep)
;;   M-g i           - consult-imenu (replaces helm-imenu)
;;   C-h a           - consult-apropos (replaces helm-apropos)

;; (use-package consult-xref)

;; compatibility
(use-package helm
 ;; bind M-x to helm-M-x and make sure helm-mode is off!
 :bind
 (("M-x" . helm-M-x)
  ("C-h a" . helm-apropos)))
(require 'helm-tags)

(require 'embark)
(require 'consult-eglot)
(require 'consult-eglot-embark)
(require 'consult-xref)

(provide 'verticorc)
;;; verticorc-complete.el ends here
