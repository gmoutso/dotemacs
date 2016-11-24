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

;; errors with ipython 5!
;; "error in process filter: Args out of range "
;; (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;; the setenv results to
;; Warning (python): Your ‘python-shell-interpreter’ doesn’t seem to support readline, yet ‘python-shell-completion-native’ was t and "ipython" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list.  Native completions have been disabled locally.
;; note: I used ubuntu's version <5, rather than pip3!
;; change: use it, because I sometimes use virtualenv with pip packages
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

;; Anaconda mode
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Anaconda mode and auto-complete
;; has problems
;; (add-hook 'python-mode-hook 'ac-anaconda-setup) ; usually this would be enough

;; jedi complete
;; (add-hook 'python-mode-hook 'jedi:setup) ; jedi is more than ac
(add-hook 'python-mode-hook 'jedi:ac-setup) ; only set up ac
(setq jedi:complete-on-dot t)                 ; optional

;; Flycheck for python (jedi,pylint)
;(add-hook 'python-mode-hook 'flycheck-mode)
; also note this (flycheck-add-next-checker 'python-flake8 'python-pylint) for a hook

;; use ipython
(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

;; line mode for Python and other Python hacks
(defun my-python-mode-hook ()
  (linum-mode 1)
  (line-number-mode t)
  (column-number-mode t)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; ;; run python script
;; (defun python-send-buffer-with-args (args)
;;   (interactive "sPython arguments: ")
;;   (let ((source-buffer (current-buffer)))
;;     (with-temp-buffer
;;       (insert "import sys; sys.argv = '''" args "'''.split()\n")
;;       (insert-buffer-substring source-buffer)
;;       (python-send-buffer))))
;; ;(global-set-key "\C-c\C-a" 'python-send-buffer-with-args)

(defun my-run-python (&optional new)
  "Runs or switches to python shell"
  (interactive)
  (run-python)
  (python-shell-switch-to-shell)
  )


(with-eval-after-load "python"
(define-key python-mode-map (kbd "C-c l") 'python-shell-send-defun)
(define-key python-mode-map (kbd "C-c r") 'python-shell-send-region)
(define-key python-mode-map (kbd "C-c b") 'python-shell-send-buffer)
;; needs define -region-or-line
(define-key python-mode-map (kbd "<M-return>") 'python-shell-send-region-or-line)
(define-key python-mode-map (kbd "C-c C-p") 'my-run-python)
)

;; inteligent send region or line to python shell
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
  (python-nav-forward-statement)
;; (next-line)
  ;; (move-beginning-of-line nil)
  )

;; change to tkagg in matplotlib, set ion
;; this is useful for virtualenv that lack qt or others
(defun python-shell-mpl-use-tk ()
  (interactive)
  (python-shell-send-string "
import matplotlib
matplotlib.use('tkagg')
import matplotlib.pyplot as plt
plt.ion()
print('plt ion with tkagg')" )
  (message "plt is interactive with tk backend"))
