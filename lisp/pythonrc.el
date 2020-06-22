;; pythonrc -- Summary
;; configuration for python
;;; Commentary:
;; None
;; Code

;; TODO
;; 1. Formalise gtags or simplify to etags
;; 2. Document flycheck or simplify to flymake
;; 3. project-wise loading of components
;; 4. modern imenu and python-x
(python-x-setup)
(use-package general)
(use-package jupyter
  :config
  (setq jupyter-eval-use-overlays nil))

;;
;; identifying buffers
;;
(defun is-notebook-p ()
  (and (boundp 'ein:notebook-mode) ein:notebook-mode))
(defun is-python-p ()
  (derived-mode-p 'python-mode))

;;
;; yas
;;
(use-package yas-minor-mode
  :hook python-mode
  :config
  (yas-reload-all)
  (setq yas-triggers-in-field t))

;;
;; lsp
;;
(use-package lsp-python-ms
  ;; :hook lsp-mode ; what to put here?
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-python-ms)
  ;;                         (lsp)))  ; or lsp-deferred
  :custom
  (lsp-python-ms-extra-paths 
   '(;"/home/moutsopoulosg/miniconda/envs/blade/bin"
     "/home/moutsopoulosg/dev/master/python"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python27.zip"
     "/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/plat-linux2"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/lib-tk"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/lib-old"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/lib-dynload"
   					;"/home/moutsopoulosg/.local/lib/python2.7/site-packages"
     "/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/site-packages"
   					;"/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/site-packages/IPython/extensions"
   					;"/home/moutsopoulosg/.ipython"
     )
			     ))

(use-package lsp-ui
  ;; :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil))

(use-package lsp-mode
  ; :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;
;; intellisense hook
;;
(defvar my-python-intellisense `my-python-load-intellisense-select "Which function to use for intellisense.")
(defun my-python-load-intellisense-select ()
  "Select lsp or anaconda according to py or ipynb"
  (cond ((file-remote-p default-directory) nil)
	((is-notebook-p) (anaconda-mode))
	(t (lsp-deferred))))
(defun my-load-intellisense ()  "Load python intellisense" (funcall my-python-intellisense))
(add-hook 'python-mode-hook 'my-load-intellisense)
(add-hook 'ein:notebook-mode-hook 'my-load-intellisense)

;;
;; anaconda
;;
(use-package anaconda-mode
  :config
  (conda-env-autoactivate-mode)
  (anaconda-eldoc-mode)
  :custom
  (conda-anaconda-home "/home/moutsopoulosg/miniconda/"))

;;
;; flycheck
;;
(use-package flycheck
  :custom
  (flycheck-python-flake8-executable "python")
  (flycheck-flake8rc "~/.emacs.d/lisp/flakerc")
  )

(use-package python
  :init
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  :config
  (helm-gtags-mode)  ;; gtags --gtagslabel=pygments with ~/.globalrc coppied
  ;; :custom
  ;; (python-shell-interpreter "ipython") (python-shell-interpreter-args "-i")
  ;; (python-shell-interpreter "python")  ; ipython does not not exist eg for pydoc
  ;; (python-shell-extra-pythonpaths
  ;; '("/home/moutsopoulosg/dev/master/python" "/home/moutsopoulosg/Documents/python/modules"))
)

;;
;; jupyter repl
;;
(general-def jupyter-repl-interaction-mode-map
  "<M-return>" 'jupyter-send-fold-or-section-and-step
  "C-c C-p" 'jupyter-repl-pop-to-buffer)
;; (general-def ein:notebook-mode-map 'ein:shared-output-pop-to-buffer)

;;
;; python keys
;;
(general-def python-mode-map
  "C-c C-p" 'my-run-python)
;; (general-def python-mode-map
;;   "<M-return>" (general-predicate-dispatch 'python-shell-send-fold-or-section-and-step
;; 	   jupyter-repl-interaction-mode 'jupyter-send-fold-or-section-and-step  ; maybe this should be local
;; 	   (is-notebook-p) 'ein:worksheet-execute-cell-and-goto-next-km)  ; maybe this should be local
;;   "C-c C-p"  (general-predicate-dispatch 'my-run-python
;; 	       jupyter-repl-interaction-mode 'jupyter-repl-pop-to-buffer
;; 	       (is-notebook-p) 'ein:shared-output-pop-to-buffer)
;; )

(defun my-python-line-mode-hook ()
  (cond ((is-notebook-p) (linum-mode -1))
	((is-python-p) (linum-mode 1)
	 (line-number-mode t)
	 (column-number-mode t))))
(add-hook 'python-mode-hook 'my-python-line-mode-hook)

;; conda
(use-package conda
  :hook eshell python
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode)
  )

(defun my-run-python (&optional new)
  "Runs or switches to python shell"
  (interactive)
  (run-python)
  (python-shell-switch-to-shell)
)

(defun python-move-down-and-newline ()
  "Move to next line, creating if needed."
  (python-nav-end-of-statement)
  (cua-set-mark)(cua-set-mark)
  (if (eobp) (newline))
  (python-nav-forward-statement)
  (python-nav-end-of-statement)
  )

(defun python-shell-send-current-statement ()
  "Send current statement to Python shell and print result, then move down.
   Taken from elpy-shell-send-current-statement"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
    (python-shell-send-string (buffer-substring beg end)))
  (python-move-down-and-newline)
  )

(defun open-in-pycharm ()
  "Open visited file in pycharm."
  (interactive)
  (shell-command (concat "/home/moutsopoulosg/app/pycharm-community-2017.1.3/bin/pycharm.sh "
" ~/dev/master/ --line " (format "%s" (line-number-at-pos)) " " (buffer-file-name))))

;;
;; python-x
;;
(setq pythonx-imenu-expression '(("Sections" "^ *# *---[ \n\t#]*\\(.*\\)" 1)))
(defun pythonx-imenu-index-function ()
  "Appends the imenu index created from default function with the imenu index created from expression."
  (let ((mode-imenu (python-imenu-create-index))
        (custom-imenu (imenu--generic-function pythonx-imenu-expression)))
    (append custom-imenu mode-imenu)))
(defun pythonx-imenu-merge-hook ()
  "Set up imenu for python-x."
  (unless (is-notebook-p) (setq imenu-create-index-function 'pythonx-imenu-index-function)))
(add-hook 'python-mode-hook 'pythonx-imenu-merge-hook)

(defun my-inferior-python-autoreload-hook ()
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2")
  )
(add-hook 'inferior-python-mode-hook 'my-inferior-python-autoreload-hook)


(defun jupyter-send-fold-or-section-and-step ()
  "Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries.

A code \"section\" is delimited in both directions, and in order, by:

- The nearest section delimiter (see `python-section-delimiter') contained
  within the current fold.
- The nearest fold delimiter (see `folding-mode-marks-alist').
- The buffer boundaries.

`folding-mode' doesn't need to be enabled, but the same marks are used to
define code boundaries. See `folding-add-to-marks-list' for customization.
Nested folds and sections are included: section delimiters contained within a
nested fold are ignored.

When the region to be evaluated is longer than a single line and less than a
screenful, the region is temporarily highlighted according to
`python-section-highlight'."
  (interactive)
  (let ((start (python-section-search t))
	(end (python-section-search nil)))
    (when python-section-highlight
      (python--vhl-full-lines start end 1 1))
    (jupyter-eval-region start end)
    (python-forward-fold-or-section)))

;; (defun my-run-existing-jupyter ()
;;   "Run latest jupyter notebook kernel."
;;   (interactive)
;;     (let ((python-shell-interpreter "jupyter-console")
;; 	  (python-shell-interpreter-args "--simple-prompt --existing")) ;;cannot find json! --ssh ted"))
;;       (run-python)
;;       (python-shell-switch-to-shell)
;;       )
;;     )

;; (defun my-run-existing-ipython-with-connect-info (connect-info)
;;   (interactive "sConnect-info:" connect-info)
;;   (let (
;; 	 (python-shell-interpreter "ipython")
;; 	(python-shell-interpreter-args
;; 	 (concat "console --existing connect-info.json --ssh ted"))) ;;cannot find json! --ssh ted"))
;;     (with-temp-file "connect-info.json" (insert connect-info))
;;       (run-python)
;;       (python-shell-switch-to-shell)
;;       )
;;   )


;;inteligent send region or line to python shell
;; (defun python-shell-send-region-or-line nil
;;   "Sends from python-mode buffer to a python shell, intelligently.
;; If a region is selected, then send region (also deselect region).
;; Else, send the current line (also move a line down)."
;;   (interactive)
;;   (cond ((region-active-p)
;; 	 (setq deactivate-mark t)
;; 	 (python-shell-send-region (region-beginning) (region-end))
;; 	 )
;; 	(t
;; 	 (python-shell-send-current-statement)
;; 	 ;(next-line) (move-beginning-of-line nil)
;; 	 )))


;; change to tkagg in matplotlib, set ion
;; this is useful for virtualenv that lack qt or others
;; (defun python-shell-mpl-use-tk ()
;;   (interactive)
;;   (python-shell-send-string "
;; import matplotlib
;; matplotlib.use('tkagg')
;; import matplotlib.pyplot as plt
;; plt.ion()
;; print('plt ion with tkagg')" )
;;   (message "plt is interactive with tk backend"))

;; (require 'importmagic)
;; (define-key importmagic-mode-map (kbd "C-c C-l") 'importmagic-fix-symbol-at-point)
;; (defun setup-importmagic ()
;;   (interactive)
;;   (conda-env-activate conda-env-current-name)
;;   (importmagic-mode 1)
;;   (importmagic--async-add-dir "/home/moutsopoulosg/dev/master/python"))
;; ;; (add-hook 'python-mode-hook 'setup-importmagic)
;; (defadvice importmagic--query-imports-for-statement-and-fix (after send-import-statement (statement) activate) (python-shell-send-string statement))

