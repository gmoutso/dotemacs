;;; embarkrc.el --- ============================================================================  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom configuration file.

;;; Code:

;; ============================================================================
;; Embark - Contextual actions on completion candidates
;; ============================================================================

(use-package embark
  :bind
  (
   ("C-." . embark-act)
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


;;
;; embark for eglot lsb symbols
;;

(defun gm/eglot-python-symbol-meta-act (name container file line &optional action)
  "Act on a workspace symbol's metadata"
  (let* ((project-file (gm/relative-pyroot-filename file))
	 (module (replace-regexp-in-string "/" "." (file-name-sans-extension project-file)))
	 (import-str (format "from %s import %s" module name))
	 (compact-name (format "%s:%s" module name)))
   (pcase action
      ('import (gm/insert-above import-str))
      ('symbol (insert name))
      ('copy-import (kill-new import-str)
		    (message "Copied: %s" import-str))
      ('copy-name (kill-new name)
		  (message "Copied: %s" name))
      ('copy-module (kill-new module)
		  (message "Copied: %s" module))
      ('copy (kill-new compact-name)
	     (message "Copied: %s" compact-name))
      ('debug (message (format "%s in %s from file %s:%s and action %s" name module file line action)))
      (_ (kill-new compact-name))))
  )

(defun gm/eglot-python-symbol-candidate-act (candidate &optional action property)
  "Insert a Python import statement for the consult-eglot-symbols CANDIDATE.

  CANDIDATE is a string with properties coming from the lsp.
"
  (let* ((property (or property 'eglot--lsp-workspaceSymbol)) ;; also 'consult--candidate
	 (symbol-info (get-text-property 0 property candidate))
         (name (plist-get symbol-info :name))
         (location (plist-get symbol-info :location))
         (uri (plist-get location :uri))
         (file (eglot-uri-to-path uri))
	 (line (plist-get (plist-get (plist-get location :range) :start) :line))
	 (container (plist-get symbol-info :containerName)))
    (gm/eglot-python-symbol-meta-act name container file line action)))

(defun gm/consult-eglot-symbol-import (candidate)
  "Insert a Python import statement for CANDIDATE."
  (gm/eglot-python-symbol-candidate-act candidate 'import 'consult--candidate)
  )

(defun gm/consult-eglot-symbol-copy (candidate)
  "Insert a Python import statement for CANDIDATE."
  (gm/eglot-python-symbol-candidate-act candidate 'copy 'consult--candidate)
  )

(defun gm/eglot-xref-symbol-import (candidate)
  "Insert a Python import statement for CANDIDATE."
  (gm/eglot-python-symbol-candidate-act candidate 'import 'eglot--lsp-workspaceSymbol)
  )

(defun gm/eglot-xref-symbol-copy (candidate)
  "Insert a Python import statement for CANDIDATE."
  (gm/eglot-python-symbol-candidate-act candidate 'copy 'eglot--lsp-workspaceSymbol)
  )



(with-eval-after-load 'consult-eglot-embark
(defvar-keymap consult-eglot-embark-map
  :doc "Example keymap with a few file actions"
  :parent embark-general-map
    "w" #'gm/consult-eglot-symbol-copy
    "i" #'gm/consult-eglot-symbol-import)
  (add-to-list 'embark-keymap-alist
               '(consult-eglot-symbols . consult-eglot-embark-map))
  )
(with-eval-after-load 'embark
(defvar-keymap eglot-xref-symbols-map
  :doc "Example keymap with a few file actions"
  :parent embark-general-map
    "w" #'gm/eglot-xref-symbol-copy
    "i" #'gm/eglot-xref-symbol-import)
(add-to-list 'embark-keymap-alist
             '(eglot-indirection-joy . eglot-xref-symbols-map))
;; (add-to-list 'embark-keymap-alist
             ;; '(consult-xref . eglot-xref-symbols-map))
  )

(provide 'embarkrc)
;;; embarkrc.el ends here