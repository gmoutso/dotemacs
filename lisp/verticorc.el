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
)

;; ;; Option 1: Additional bindings
;; (keymap-set vertico-map "?" #'minibuffer-completion-help)
;; ;; (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
;; ;; (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
;; ;; Option 2: Replace `vertico-insert' to enable TAB prefix expansion.
;; (keymap-set vertico-map "TAB" #'minibuffer-complete)

(use-package vertico-multiform
  :custom
  (vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-line buffer)
          (execute-extended-command unobtrusive)))
  
  ;; Configure display per completion category
  (vertico-multiform-categories
        '((file grid)
          (consult-grep buffer))) 
  )


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


(provide 'verticorc)
;;; verticorc.el ends here
