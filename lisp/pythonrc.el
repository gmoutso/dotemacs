;; pythonrc -- Summary
;; configuration for python
;;; Commentary:
;; None
;; Code
(use-package general)
(use-package jupyter
  :config
  (setq jupyter-eval-use-overlays nil))

(use-package yas-minor-mode
  :hook python-mode
  :config
  (yas-reload-all)
  (setq yas-triggers-in-field t))

(defvar my-python-intellisense `my-python-load-intellisense-select "Which function to use for intellisense.
E.g., anaconda-mode or lsp")
(defun my-python-load-intellisense-select ()
  "Select lsp or anaconda according to context (py or ipynb file)"
  (cond ((file-remote-p default-directory) nil)
	((and (boundp 'ein:notebook-mode) ein:notebook-mode) (anaconda-mode))
	((and (boundp 'python-mode) (derived-mode-p 'python-mode)) (lsp))))
(defun my-load-intellisense ()  "Load python intellisense" (funcall my-python-intellisense))
(add-hook 'python-mode-hook 'my-load-intellisense)
(add-hook 'ein:notebook-mode-hook 'my-load-intellisense)

;; note that exec-path cannot have nil (current directory) or env-conda-activate will not work
(use-package anaconda-mode
  :config
  (conda-env-autoactivate-mode)
  :custom
  (conda-anaconda-home "/home/moutsopoulosg/miniconda/"))

;; (use-package anaconda-eldoc-mode)

(use-package lsp-python-ms
  :config
  ;; :hook lsp-mode ; what to put here?
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-python-ms)
  ;;                         (lsp)))  ; or lsp-deferred
  :custom
  (lsp-python-ms-extra-paths '("/home/moutsopoulosg/miniconda/envs/blade/bin"
			       "/home/moutsopoulosg/dev/master/python"
			       "/home/moutsopoulosg/miniconda/envs/blade/lib/python2.7/site-packages"))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil))

(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil))  ;; if this causes problems, make none and enable flycheck

; also note this (flycheck-add-next-checker 'python-flake8 'python-pylint) for a hook
(use-package flycheck
  ;; :hook (python-mode . flycheck-mode)
  :config
  ;; (spaceline-toggle-flycheck-info-off)
  ;; (spaceline-toggle-flycheck-warning-off)
  :custom
  (flycheck-python-flake8-executable "python")
  (flycheck-flake8rc "~/.emacs.d/lisp/flakerc")
  ;; (flycheck-highlighting-mode nil) ;default is symbols
  :custom-face
  ;; (flycheck-warning ((t (:underline nil))))
  )

;; (general-def 'python-mode-map
;;   "M-p" (general-predicate-dispatch nil jupyter-repl-interaction-mode 'jupyter-send-fold-or-section-and-step)
;;   "<M-return>" (general-predicate-dispatch nil jupyter-repl-interaction-mode 'jupyter-eval-line-or-region)
;; ; "C-c l" 'python-shell-send-defun
;; ; "C-c r" 'python-shell-send-region
;; ; "C-c b" 'python-shell-send-buffer
;; ; "<M-return>" 'python-shell-send-region-or-line
;; ; "M-p" 'python-shell-send-fold-or-section-and-step
;;   "C-c C-p"  (general-predicate-dispatch 'my-run-python jupyter-repl-interaction-mode 'jupyter-repl-pop-to-buffer)
;;   ;"C-c C-c" 'python-shell-send-region-or-line
;; )
;; (with-eval-after-load "python"
;; (define-key python-mode-map (kbd "M-p") 'python-shell-send-fold-or-section-and-step))

(use-package python
  :init
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  :config
  (helm-gtags-mode)  ;; gtags --gtagslabel=pygments with ~/.globalrc coppied
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i")
  (python-shell-extra-pythonpaths
    '("/home/moutsopoulosg/dev/master/python" "/home/moutsopoulosg/Documents/python/modules"))
  :general
  ("M-p" (general-predicate-dispatch 'python-shell-send-fold-or-section-and-step jupyter-repl-interaction-mode 'jupyter-send-fold-or-section-and-step))
  ("<M-return>" (general-predicate-dispatch nil jupyter-repl-interaction-mode 'jupyter-eval-line-or-region))
  ("C-c C-p"  (general-predicate-dispatch 'my-run-python jupyter-repl-interaction-mode 'jupyter-repl-pop-to-buffer))
  )

(defun my-python-line-mode-hook ()
  (linum-mode 1)
  (line-number-mode t)
  (column-number-mode t))
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
  ;; (conda-env-autoactivate-mode)
  )

;; (defun local-python ()
;;   "use local ipython interpreter"
;;   (interactive)
;;   (setq python-shell-interpreter "ipython"
;; 	python-shell-interpreter-args "-i")
;;   (custom-set-variables
;;    '(conda-anaconda-home "/home/moutsopoulosg/miniconda/"))
;;   )
;; (local-python)


;; using elpy for python
;; (elpy-enable)
;; (setq elpy-rpc-python-command "python3")
;; (elpy-use-ipython "ipython3")
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'pyvenv-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)
;; (setq jedi:environment-root "jedi")  ; or any other name you like
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "python3")))
;(elpy-use-ipython)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Anaconda mode and auto-complete
;; has problems
;; (add-hook 'python-mode-hook 'ac-anaconda-setup) ; usually this would be enough

;; jedi complete
;; (add-hook 'python-mode-hook 'jedi:setup) ; jedi is more than ac
;(add-hook 'python-mode-hook 'jedi:ac-setup) ; only set up ac
;(setq jedi:complete-on-dot t)                 ; optional


;; (defun ted-banks ()
;;   "use ted ipython interpreter"
;;   (interactive)
;;   ;; the following does not work
;;   ;; solution: simply run-python from a tramp opened file
;;   ;; (setq
;;   ;; python-shell-interpreter "/ssh:ted:/home/moutsopoulosg/miniconda/envs/banks/bin/python"
;;    ;; python-shell-interpreter-args "--simple-prompt")
;;   (custom-set-variables
;;    '(conda-anaconda-home "/ssh:ted:/home/moutsopoulosg/miniconda/"))
;;   (add-to-list 'python-shell-extra-pythonpaths "/home/moutsopoulosg/master/python")
;;   )


;; ;; run python script
;; (defun python-send-buffer-with-args (args)
;;   (interactive "sPython arguments: ")
;;   (let ((source-buffer (current-buffer)))
;;     (with-temp-buffer
;;       (insert "import sys; sys.argv = '''" args "'''.split()\n")
;;       (insert-buffer-substring source-buffer)
;;       (python-send-buffer))))
;; ;(global-set-key "\C-c\C-a" 'python-send-buffer-with-args)

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

(defun my-run-python (&optional new)
  "Runs or switches to python shell"
  (interactive)
  (run-python)
  (python-shell-switch-to-shell)
  )

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

(defun python-move-down-and-newline ()
  "Move to next line, creating if needed."
  (python-nav-end-of-statement)
  (cua-set-mark)(cua-set-mark)
  (if (eobp) (newline))
  (python-nav-forward-statement)
  (python-nav-end-of-statement)
  )

(defun python-shell-send-current-statement ()
  "Send current statement to Python shell.
   Taken from elpy-shell-send-current-statement"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
;;    (elpy-shell-get-or-create-process)
    (python-shell-send-string (buffer-substring beg end)))
;;  (elpy-shell-display-buffer)
  (python-move-down-and-newline)
  )

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

(require 'importmagic)
(define-key importmagic-mode-map (kbd "C-c C-l") 'importmagic-fix-symbol-at-point)
(defun setup-importmagic ()
  (interactive)
  (conda-env-activate conda-env-current-name)
  (importmagic-mode 1)
  (importmagic--async-add-dir "/home/moutsopoulosg/dev/master/python"))
;; (add-hook 'python-mode-hook 'setup-importmagic)
(defadvice importmagic--query-imports-for-statement-and-fix (after send-import-statement (statement) activate) (python-shell-send-string statement))

;; (defun open-in-pycharm (arg)
;;   "Open visited file in pycharm."
;;   (interactive "P")
;;   (when buffer-file-name
;;     (shell-command (concat
;;                     (cond
;;                      ((and (not arg) (eq system-type 'darwin)) "open")
;;                      ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
;;                      (t (read-shell-command "Open current file with: ")))
;;                     " "
;;                     (shell-quote-argument buffer-file-name)))))

(defun open-in-pycharm ()
  "Open visited file in pycharm."
  (interactive)
  (shell-command (concat "/home/moutsopoulosg/app/pycharm-community-2017.1.3/bin/pycharm.sh "
" ~/dev/master/ --line " (format "%s" (line-number-at-pos)) " " (buffer-file-name))))

(setq pythonx-imenu-expression '(("Sections" "^ *# *---[ \n\t#]*\\(.*\\)" 1)))
(defun pythonx-imenu-index-function ()
  "Appends the imenu index created from default function with the imenu index created from expression."
  (let ((mode-imenu (python-imenu-create-index))
        (custom-imenu (imenu--generic-function pythonx-imenu-expression)))
    (append custom-imenu mode-imenu)))
(defun pythonx-imenu-merge-hook ()
  "Set up imenu for python-x."
  (setq imenu-create-index-function 'pythonx-imenu-index-function))
(add-hook 'python-mode-hook 'pythonx-imenu-merge-hook)

(defun my-inferior-python-autoreload-hook ()
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2")
  )
(add-hook 'inferior-python-mode-hook 'my-inferior-python-autoreload-hook)

(python-x-setup)

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

(major-mode-hydra-define python-mode (:exit t :quit-key ("q" "c" "<escape>"))
  ("Jupyter"
   (("l" jupyter-server-list-kernels "list kernels")
    ("a" jupyter-repl-associate-buffer "associate"))
   "Navigate"
   (("i" helm-imenu "imenu"))
   "Code"
   (("f" flycheck-list-errors "flycheck")
    ("g" helm-gtags-find-tag "gtags")
    ("e" conda-env-activate "activate env")))
  )

