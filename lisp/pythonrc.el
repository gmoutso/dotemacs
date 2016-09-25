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

;; Anaconda mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'ac-anaconda-setup)

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
(with-eval-after-load "python"
(define-key python-mode-map (kbd "C-c l") 'python-shell-send-defun)
(define-key python-mode-map (kbd "C-c r") 'python-shell-send-region)
(define-key python-mode-map (kbd "C-c b") 'python-shell-send-buffer)
  )

