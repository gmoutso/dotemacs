(use-package conda
  :hook eshell python
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/home/moutsopoulosg/anaconda3/bin:/home/moutsopoulosg/anaconda3/condabin"))
  (setq exec-path (append exec-path '("/home/moutsopoulosg/anaconda3/bin" "/home/moutsopoulosg/anaconda3/condabin")))
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
;; https://github.com/necaris/conda.el/issues/104
;; (defun me/conda--get-path-prefix (env-dir)
;;   (expand-file-name  "bin" env-dir))
;; (advice-add 'conda--get-path-prefix :override #'me/conda--get-path-prefix)
;; (advice-remove 'conda--get-path-prefix #'me/conda--get-path-prefix)

;; with-venv changes exec-path and $PATH and $VIRTUAL_ENV but not python-shell-virtualenv-root
;; we use conda.el conda-with-env
;; (defmacro gm/conda-with-env (name &rest forms)
;;   "With conda env NAME active, evaluate FORMS."
;;   `(progn
;;      (let ((prev-env-name conda-env-current-name)
;; 	   (prev-env-path conda-env-current-path))
;;        (conda-env-activate ,name) ;; switch it up
;;        (unwind-protect
;;            (progn ,@forms) ;; evaluate forms
;; 	 )
;;        (if ,prev-env-name ;; switch back ;; <--- correction
;;            (conda-env-activate-path ,prev-env-path)
;;          (conda-env-deactivate)))))

(defvar gm/emacs-venv "emacs")
(defun gm/with-emacs-venv-advice (orig-func &rest args)
  (let ((prev-env-name conda-env-current-name)
	(prev-env-path conda-env-current-path)
	conda-project-env-path)
    (let ((inhibit-message t))
      (conda-env-activate gm/emacs-venv))
    (apply orig-func args)
    (let ((inhibit-message t))
      (if prev-env-name
	  (conda-env-activate-path prev-env-path)
	(conda-env-deactivate))
	)))

(defun gm/with-emacs-venv-advice-add (func)
  (advice-add func
              :around
              'gm/with-emacs-venv-advice))
(defun gm/with-emacs-venv-advice-remove (func)
  "Remove advice of FUNC added by `with-venv-advice-add'."
  (advice-remove func
                 'gm/with-emacs-venv-advice))

; (use-package lsp-mode)
(gm/with-emacs-venv-advice-add 'lsp)
; (use-package blacken)
(gm/with-emacs-venv-advice-add 'blacken-buffer)
;; (use-package code-cells)
;; conda install jupytext
(gm/with-emacs-venv-advice-add 'code-cells-write-ipynb)
(gm/with-emacs-venv-advice-add 'code-cells-convert-ipynb)
;; pip install PyQtWebEngine
;; pip install PyQt6
;; conda install epc -c conda-forge
;; remove import line
;; ImportError: cannot import name 'QtWebEngineWidgets' from 'PyQt6'
;; (/home/moutsopoulosg/anaconda3/envs/emacs/lib/python3.10/site-packages/PyQt6/__init__.py
(gm/with-emacs-venv-advice-add 'eaf-restart-process)
(gm/with-emacs-venv-advice-add 'eaf-start-process)
