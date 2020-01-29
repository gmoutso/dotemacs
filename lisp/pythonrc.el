(require 'use-package)
(use-package general)
(use-package jupyter-mode
  :defer t
  :config
  (setq jupyter-eval-use-overlays nil))

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
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; conda
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode)


;; Anaconda mode and auto-complete
;; has problems
;; (add-hook 'python-mode-hook 'ac-anaconda-setup) ; usually this would be enough

;; jedi complete
;; (add-hook 'python-mode-hook 'jedi:setup) ; jedi is more than ac
;(add-hook 'python-mode-hook 'jedi:ac-setup) ; only set up ac
;(setq jedi:complete-on-dot t)                 ; optional

;; Flycheck for python (jedi,pylint)
;(add-hook 'python-mode-hook 'flycheck-mode)
; also note this (flycheck-add-next-checker 'python-flake8 'python-pylint) for a hook

;; use ipython
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(defun local-python ()
  "use local ipython interpreter"
  (interactive)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i")
  (custom-set-variables
   '(conda-anaconda-home "/home/moutsopoulosg/miniconda/"))
  ;; (add-to-list 'python-shell-extra-pythonpaths "/home/moutsopoulosg/master/python")
  )
(local-python)

(defun ted-banks ()
  "use ted ipython interpreter"
  (interactive)
  ;; the following does not work
  ;; solution: simply run-python from a tramp opened file
  ;; (setq
  ;; python-shell-interpreter "/ssh:ted:/home/moutsopoulosg/miniconda/envs/banks/bin/python"
   ;; python-shell-interpreter-args "--simple-prompt")
  (custom-set-variables
   '(conda-anaconda-home "/ssh:ted:/home/moutsopoulosg/miniconda/"))
  (add-to-list 'python-shell-extra-pythonpaths "/home/moutsopoulosg/master/python")
  )


;; line mode for Python and other Python hacks
(defun my-python-line-mode-hook ()
  (linum-mode 1)
  (line-number-mode t)
  (column-number-mode t)
  )
(add-hook 'python-mode-hook 'my-python-line-mode-hook)

;; ;; run python script
;; (defun python-send-buffer-with-args (args)
;;   (interactive "sPython arguments: ")
;;   (let ((source-buffer (current-buffer)))
;;     (with-temp-buffer
;;       (insert "import sys; sys.argv = '''" args "'''.split()\n")
;;       (insert-buffer-substring source-buffer)
;;       (python-send-buffer))))
;; ;(global-set-key "\C-c\C-a" 'python-send-buffer-with-args)

(defun my-run-existing-jupyter ()
  "Run latest jupyter notebook kernel."
  (interactive)
    (let ((python-shell-interpreter "jupyter-console")
	  (python-shell-interpreter-args "--simple-prompt --existing")) ;;cannot find json! --ssh ted"))
      (run-python)
      (python-shell-switch-to-shell)
      )
    )

(defun my-run-existing-ipython-with-connect-info (connect-info)
  (interactive "sConnect-info:" connect-info)
  (let (
	 (python-shell-interpreter "ipython")
	(python-shell-interpreter-args
	 (concat "console --existing connect-info.json --ssh ted"))) ;;cannot find json! --ssh ted"))
    (with-temp-file "connect-info.json" (insert connect-info))
      (run-python)
      (python-shell-switch-to-shell)
      )
  )

(defun my-run-python (&optional new)
  "Runs or switches to python shell"
  (interactive)
  (run-python)
  (python-shell-switch-to-shell)
  )


;;inteligent send region or line to python shell
(defun python-shell-send-region-or-line nil
  "Sends from python-mode buffer to a python shell, intelligently.
If a region is selected, then send region (also deselect region).
Else, send the current line (also move a line down)."
  (interactive)
  (cond ((region-active-p)
	 (setq deactivate-mark t)
	 (python-shell-send-region (region-beginning) (region-end))
	 )
	(t
	 (python-shell-send-current-statement)
	 ;(next-line) (move-beginning-of-line nil)
	 )))

(defun python-shell-send-current-statement ()
  "Send current statement to Python shell.

Taken from elpy-shell-send-current-statement"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
;;    (elpy-shell-get-or-create-process)
    (python-shell-send-string (buffer-substring beg end)))
;;  (elpy-shell-display-buffer)
  (python-nav-end-of-statement)
  (cua-set-mark)(cua-set-mark)
  ;; (set-mark-command nil)
  (if (eobp) (newline))
  (python-nav-forward-statement)
;; (next-line)
  ;; (move-beginning-of-line nil)
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
  (conda-env-activate conda-project-env-name)
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
  (interactive)
  (let ((mode-imenu (python-imenu-create-index))
        (custom-imenu (imenu--generic-function pythonx-imenu-expression)))
    (append custom-imenu mode-imenu)))
(defun pythonx-imenu-merge-hook ()
  "Set up imenu for python-x."
  (interactive)
  (setq imenu-create-index-function 'pythonx-imenu-index-function))
(add-hook 'python-mode-hook 'pythonx-imenu-merge-hook)

(defun my-inferior-python-autoreload-hook ()
  (interactive)
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2")
  )
(add-hook 'inferior-python-mode-hook 'my-inferior-python-autoreload-hook)

(with-eval-after-load "python"
(define-key python-mode-map (kbd "M-p") 'python-shell-send-fold-or-section-and-step))

(defun only-flycheck-warnings-hook ()
  (interactive)
  ;; (set-face-attribute 'flycheck-warning nil :underline nil)
  ;; (set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
  )
(setq test '())
(setq test (append test '(("E501" . info) ;; line too long
			  ("E231" . info) ;; missing whitespace after ",", ":", ...
			  ("E291" . info) ;; trailing whitespace
			  ("E201" . info) ;; whitespace after (
			  ("E202" . info) ;; whitespace before )
			  ("E201" . info) ;; whitespace after (
			  )))
;; (("^E9.*$" . error)
;;  ("^F82.*$" . error)
;;  ("^F83.*$" . error)
;;  ("^D.*$" . info)
;;  ("^N.*$" . info))

(general-def 'python python-mode-map
  "C-c l" 'python-shell-send-defun
  "C-c r" 'python-shell-send-region
  "C-c b" 'python-shell-send-buffer
  "<M-return>" 'python-shell-send-region-or-line
  "C-c C-p"  'my-run-python
  "C-c C-c" 'python-shell-send-region-or-line)
