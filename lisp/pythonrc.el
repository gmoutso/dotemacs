;; pythonrc -- Summary
;; configuration for python
;;; Commentary:
;; None
;; Code

;; TODO
;; 1. Formalise gtags or simplify to etags
;; 2. Document flycheck or simplify to flymake
;; 3. project-wise loading of components
(use-package general)

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
(use-package lsp-mode
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-python-ms
  ;; :hook lsp-mode ; what to put here?
  :custom
  (lsp-python-ms-extra-paths 
   '("/home/moutsopoulosg/dev/master/python"
     "/home/moutsopoulosg/anaconda3/envs/blade/lib/python2.7"
     "/home/moutsopoulosg/anaconda3/envs/blade/lib/python2.7/site-packages"
     )))

(use-package lsp-pyright
  ;; :custom
  ;; (lsp-pyright-multi-root nil)
  )

;; (add-to-list 'lsp-disabled-clients 'pyright)

(use-package lsp-ui
  ;; :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (focus-follows-mouse nil)
  )


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
  ;; (conda-env-autoactivate-mode)
  (anaconda-eldoc-mode)
  )

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
  ;; :config
  ;; (helm-gtags-mode)  ;; gtags --gtagslabel=pygments with ~/.globalrc coppied
  ;; :custom
  ;; (python-shell-interpreter "ipython") (python-shell-interpreter-args "-i")
  ;; (python-shell-interpreter "python")  ; ipython does not not exist eg for pydoc
  ;; (python-shell-extra-pythonpaths
  ;; '("/home/moutsopoulosg/dev/master/python" "/home/moutsopoulosg/Documents/python/modules"))
)

;;
;; python keys
;;
(defun backward-symbol () (interactive) (forward-symbol -1))
(general-def python-mode-map
  "C-c C-p" 'my-run-python
  ;; navigation
  "C-<right>" 'forward-symbol
  "C-<left>" 'backward-symbol
  "M-p" 'python-nav-backward-statement
  "M-n" 'python-nav-forward-statement
  "C-<down>" 'forward-paragraph
  "C-<up>" 'backward-paragraph
  "M-}" 'forward-paragraph
  "M-{" 'backward-paragraph
  "M-[" 'python-nav-backward-block
  "M-]" 'python-nav-forward-block
  "M-e" 'python-nav-forward-defun
  "M-a" 'python-nav-backward-defun  ; kde stole this
  ; "M-a" 'python-nav-backward-block
  ; "M-e" 'python-nav-forward-block
  "C-M-u" 'python-nav-backward-up-list
   )

;; (general-def python-mode-map
;;   "<M-return>" (general-predicate-dispatch 'python-shell-send-fold-or-section-and-step
;; 	   jupyter-repl-interaction-mode 'jupyter-send-fold-or-section-and-step  ; maybe this should be local
;; 	   (is-notebook-p) 'ein:worksheet-execute-cell-and-goto-next-km)  ; maybe this should be local
;;   "C-c C-p"  (general-predicate-dispatch 'my-run-python
;; 	       jupyter-repl-interaction-mode 'jupyter-repl-pop-to-buffer
;; 	       (is-notebook-p) 'ein:shared-output-pop-to-buffer)
;; )

(defun my-python-line-mode-hook ()
  (cond ((is-notebook-p) (display-line-numbers-mode 0))
	((is-python-p) (display-line-numbers-mode t)
	 (display-line-numbers-mode t)
	 (column-number-mode t))))
(add-hook 'python-mode-hook 'my-python-line-mode-hook)

(use-package conda
  :hook eshell python
  :custom
  (conda-anaconda-home "/home/moutsopoulosg/anaconda3/")
  (conda-env-home-directory "/home/moutsopoulosg/anaconda3/")  ; was in separate setq
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (add conda-project-env-name in .dir-locals), include:
  ;; (conda-env-autoactivate-mode) or (eval . (conda-env-autoactivate-mode)) or add to python hook
  )
(add-hook 'python-mode-hook 'conda-env-autoactivate-mode)

(defun my-run-python (&optional new)
  "Runs or switches to python shell"
  (interactive)
  (let ((python-shell-interpreter "ipython")
        (python-shell-interpreter-args "-i --simple-prompt"))
	(run-python))
  (python-shell-switch-to-shell)
  (setq-local tab-width 4)
)


(defun open-in-pycharm ()
  "Open visited file in pycharm."
  (interactive)
  (shell-command (concat "/home/moutsopoulosg/app/pycharm-community-2017.1.3/bin/pycharm.sh "
" ~/dev/master/ --line " (format "%s" (line-number-at-pos)) " " (buffer-file-name))))

(defun my-inferior-python-autoreload-hook ()
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2")
  )
(add-hook 'inferior-python-mode-hook 'my-inferior-python-autoreload-hook)


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

;; work with helm-etags-select
(defun gm/etags-python-helm-process-candidate (candidate)
  "Return list of MODULE and OBJECT of an helm-etags-select CANDIDATE."
  (let* ((split (helm-grep-split-line candidate))
         (fname (car split)) ; 1st
	 (linum (string-to-number (cadr split))) ; 2nd
	 (elm   (s-trim (cl-caddr split))) ; 3d
	 (tagpattern "\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)(")
	 (elmpattern (string-match tagpattern  elm))
	 (object (match-string 1 elm))  ; class/function name
	 (modulepattern "\\(?:\\(?:src\\|python\\|cython\\)/\\)?\\(?1:.*\\)\\.py")
	 (modulematch (string-match modulepattern fname))
	 (module (replace-regexp-in-string "/" "." (match-string-no-properties 1 fname)))
	 )
    (list module object)))
(defun gm/etags-python-helm-action-insert-import (candidate)
  "Action to insert an import statement one line above using helm-etags-select source."
  (let* ((p (gm/etags-python-helm-process-candidate candidate))
	   (module (car p))
	   (object (cadr p))
	   (string (concat "from " module " import " object "\n")))
    (save-excursion
      (beginning-of-line)
      (insert string)
      )))
(defun gm/etags-python-helm-action-insert-symbol (candidate)
    "Action to insert symbol using helm-etags-select source."
  (let* ((p (gm/etags-python-helm-process-candidate candidate))
	   (module (car p))
	   (object (cadr p)))
    (insert (concat " " object))))
(defun gm/etags-python-helm-advice (source)
  "Add actions for python insertion to helm-etags-select"
  (helm-add-action-to-source "Insert import" 'gm/etags-python-helm-action-insert-import source)
  (helm-add-action-to-source "Insert symbol" 'gm/etags-python-helm-action-insert-symbol source)
  (helm-add-action-to-source "Insert import and symbol" (lambda (c)
							  (gm/etags-python-helm-action-insert-import c)
							  (gm/etags-python-helm-action-insert-symbol c))
			     source)
  source)
;; the following does not seem to work?
(advice-add 'helm-etags-build-source :filter-return #'gm/etags-python-helm-advice)
; the following adds to helm-etags-select source (once built initially?)
(defun gm/etags=python-helm-add-action-post-build ()
(helm-add-action-to-source "Insert import" 'gm/etags-python-helm-action-insert-import helm-source-etags-select)
(helm-add-action-to-source "Insert symbol" 'gm/etags-python-helm-action-insert-symbol helm-source-etags-select))
; do it
(gm/etags=python-helm-add-action-post-build)

;; defintions in buffer
;; (defconst gm/top-pydef-regex
;;   "^\\(?:\\(?2:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)(\\|\\(?1:[[:alnum:]_]*\\) *\\(?2:=\\)\\)"
;;   "Class, def or variable definition regex.

;; 1: object, 2: def|class|=")
;; (defun gm/helm-occur-pydefs ()
;;   (interactive)
;;   (helm-occur "^[0-9]* \\(?:\\(?2:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)(\\|\\(?1:[[:alnum:]_]*\\) *\\(?2:=\\)\\)"))
;; (defun gm/occur-pydefs ()
;;   "Occur python definitions in buffer."
;;   (interactive)
;;   (occur gm/top-pydef-regex)
;;   )

(python-x-setup)

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

(defun gm/to_open_dataarray (beginning end)
  (interactive "*r")
  (let ((beginning (if (region-active-p) beginning (point-at-bol)))
	(end (if (region-active-p) end (point-at-eol)))
	(regex "\\([a-zA-Z0-9_]*\\)\.to_netcdf(\\([^)]*\\))")
	(replace "\\1 = xr.open_dataarray(\\2)"))
    (setq end (copy-marker end))
    (save-match-data
      (save-excursion
	(goto-char beginning)
	(while (re-search-forward regex end t)
          (replace-match replace))))
  (set-marker end nil)))

(defun gm/to_open_dataset (beginning end)
  (interactive "*r")
  (let ((beginning (if (region-active-p) beginning (point-at-bol)))
	(end (if (region-active-p) end (point-at-eol)))
	(regex "\\([a-zA-Z0-9_]*\\)\.to_netcdf(\\([^)]*\\))")
	(replace "\\1 = xr.open_dataset(\\2)"))
    (setq end (copy-marker end))
    (save-match-data
      (save-excursion
	(goto-char beginning)
	(while (re-search-forward regex end t)
          (replace-match replace))))
  (set-marker end nil)))


(defun gm/to_to_netcdf (beginning end)
  (interactive "*r")
  (let ((beginning (if (region-active-p) beginning (point-at-bol)))
	(end (if (region-active-p) end (point-at-eol)))
	(regex "\\([a-zA-Z0-9_]*\\) *= *xr\.open_data\\(?:array\\|set\\)(\\([^)]*\\))")
	(replace "\\1.to_netcdf(\\2)"))
    (setq end (copy-marker end))
    (save-match-data
      (save-excursion
	(goto-char beginning)
	(while (re-search-forward regex end t)
          (replace-match replace))))
  (set-marker end nil)))

