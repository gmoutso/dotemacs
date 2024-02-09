;; pythonrc -- Summary
;; configuration for python
;;; Commentary:
;; None
;; Code

(use-package general)

;;
;; identifying buffers
;;
(defun is-python-p () (derived-mode-p 'python-mode 'python-ts-mode))

;;
;; yas
;;
(use-package yas-minor-mode
  :hook python-mode python-ts-mode
  :config
  (yas-reload-all)
  (setq yas-triggers-in-field t))

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
(general-def python-mode-map python-ts-mode-map 
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

(defun gm/python-mode-hook ()
  (display-line-numbers-mode t)
  (display-line-numbers-mode t)
  (column-number-mode t)
  )
(add-hook 'python-mode-hook 'gm/python-mode-hook)
(add-hook 'python-ts-mode-hook 'gm/python-mode-hook)


;; (defun my-run-python (&optional new)
;;   "Runs or switches to python shell"
;;   (interactive)
;;   (let ((python-shell-interpreter "ipython")
;;         (python-shell-interpreter-args "-i --simple-prompt"))
;; 	(run-python))
;;   (python-shell-switch-to-shell)
;;   (setq-local tab-width 4)
;; )


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
	 (tagpattern "\\(?:def\\|class\\) *\\(?1:[[:alnum:]_]*\\)[(:]")
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
;; (advice-add 'helm-etags-build-source :filter-return #'gm/etags-python-helm-advice)
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

;; (python-x-setup)

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


; (setq gm/python-imenu-expression '(("Sections" "^ *# *---[ \n\t#]*\\(.*\\)" 1)))
(setq gm/python-imenu-expression '(("Sections" "^#+ *%+ *\\(.*\\)"
				    1)))
(defun gm/python-imenu-index-function ()
  "Appends the imenu index created from default function with the imenu index created from expression."
  (let ((mode-imenu (python-imenu-create-index))
        (custom-imenu (imenu--generic-function gm/python-imenu-expression)))
    (append custom-imenu mode-imenu)))
(defun gm/python-imenu-merge-hook ()
  "Set up imenu for python."
  (setq imenu-create-index-function 'gm/python-imenu-index-function))
(add-hook 'python-mode-hook 'gm/python-imenu-merge-hook)

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

(require 'nadvice)
(defun run-python-locally (&optional dedicated notshow)
  ;; https://emacs.stackexchange.com/questions/17753/make-run-python-etc-use-local-python
  ;; (interactive (advice-eval-interactive-spec
  ;;                      (cadr (interactive-form #'run-python))))
  (interactive "P\ni" python-mode python-ts-mode)
  (let ((default-directory user-emacs-directory))
    (conda-env-activate)
    (run-python nil dedicated (not notshow))))

;; (add-to-list 'tramp-remote-process-environment (format "DISPLAY=%s" (getenv "DISPLAY")))
;; (setq tramp-remote-process-environment '("ENV=''" "TMOUT=0" "LC_CTYPE=''" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct="))
;; (alist :key-type string :value-type (plist ???))

(defvar gm/run-python-configs
  '(
    ;; ("Python[local:egan]"
    ;;  :cmd "ipython --simple-prompt -i"
    ;;  :host "~/"
    ;;  :venv "anaconda3/envs/egan"
    ;;  :pythonpaths ("/home/moutsopoulosg/dev/py36/python")
    ;;  )
    ("Python[local:egan_reporter]"
     :cmd "ipython"
     :host "~/"
     :venv "anaconda3/envs/egan_reporter"
     :pythonpaths ("/home/moutsopoulosg/dev/py310/python")
     )
    ("Python[local:db3]"
     :cmd "ipython"
     :host "~/"
     :venv "anaconda3/envs/db3env"
     :pythonpaths ("/home/moutsopoulosg/dev/db3/python")
     )
    ("Python[local:banks]"
     :cmd "ipython -i"
      :host "~/"
      :venv "anaconda3/envs/banks"
      :pythonpaths ("/home/moutsopoulosg/dev/master/python")
      )
    ;; ("Python[local:dick]"
    ;;  :cmd "ipython"
    ;;   :host "~/"
    ;;   :venv "anaconda3/envs/dick"
    ;;   :pythonpaths ("/home/moutsopoulosg/dev/py36/python")
    ;;   )
    ("Python[phil:banks]"
      :cmd "ipython"
      :host "/ssh:phil:"
      :venv "miniconda/envs/banks"
      :pythonpaths ("/home/moutsopoulosg/dev/master/python")
      )
     ("Python[phil:egan]"
      :cmd "ipython -i --simple-prompt"
      :host "/ssh:phil:"
      :venv "miniconda/envs/egan"
      :pythonpaths ("/home/moutsopoulosg/dev/py36/python")
      )
     ("Python[phil:dick]"
      :cmd "ipython -i"
      :host "/ssh:phil:"
      :venv "miniconda/envs/dick"
      :pythonpaths ("/home/moutsopoulosg/dev/py36/python")
      )
    ("Python[beowulf@ted:banks]"
      :cmd "ipython -i"
      :host "/ssh:beowulf@ted:"
      :venv "miniconda/envs/banks"
      :pythonpaths ("/home/beowulf/dev/master/python")
      )
    ("Python[beowulf@ted:egan]"
      :cmd "ipython -i --simple-prompt"
      :host "/ssh:beowulf@ted:"
      :venv "miniconda3/envs/egan"
      :pythonpaths ("/home/beowulf/dev/py36/python")
      )
    ("Python[beowulf@ted:dick]"
      :cmd "ipython -i --simple-prompt"
      :host "/ssh:beowulf@ted:"
      :venv "miniconda3/envs/dick"
      :pythonpaths ("/home/beowulf/dev/py36/python")
      )
    ;; ("Python[migration-gm]"
    ;;  :cmd "ipython"
    ;;  :host "/ssh:test-migration-gm:"
    ;;  :venv "miniconda3/envs/bankscloud"
    ;;  :pythonpaths ("/home/ubuntu/dev/cloud_migration_py2/python")
    ;;  )
    ("Python[aws-tests:bankscloud]"
     :cmd "ipython"
     :host "/ssh:cloud-tests-gm:"
     :venv "miniconda3/envs/bankscloud"
     :pythonpaths ("/home/ubuntu/dev/cloud_migration_py2/python")
     )
    ;; ("Python[aws-tests:db3]"
    ;;  :cmd "ipython --simple-prompt  -i"
    ;;  :host "/ssh:gm_aws_migration:"
    ;;  :venv "miniconda3/envs/db3env"
    ;;  :pythonpaths ("/home/ubuntu/dev/cloud_migration_py3/python"
    ;; 		   "/home/ubuntu/dev/db3/python"
    ;; 		   )
    ;;  )
    ("Python[cloud-tests:py310]"
     :cmd "ipython --simple-prompt  -i"
     :host "/ssh:cloud-tests-gm:"
     :venv "miniconda3/envs/egan_simulator"
     :pythonpaths ("/home/ubuntu/dev/py310/python")
     )
    ;; ("Python[gm_aws:db3.v1.debug]"
    ;;  :cmd "ipython --simple-prompt  -i"
    ;;  :host "/ssh:gm_aws_migration:"
    ;;  :venv "miniconda3/envs/db3env"
    ;;  :pythonpaths ("/home/ubuntu/dev/cloud_migration_py3/python"
    ;; 		   "/home/ubuntu/dev/db3.v1.debug/python"
    ;; 		   )
    ;;  )
     )
  "Alist of configuratons of a name (usually host) => alist of setup (usually an environment on host) => plist of configs. Used in `gm/run-python' but nned not be remote."
  )

(defun gm/get-run-python-config ()
  (let* ((name (completing-read "config: " gm/run-python-configs nil t))
	 (config (cdr (assoc name gm/run-python-configs)))
	 (host (plist-get config :host))
	 (cmd (plist-get config :cmd))
	 (venv (plist-get config :venv))
	 (pythonpaths (plist-get config :pythonpaths)))
    (list :cmd cmd :host host :venv venv :pythonpaths pythonpaths
	  :name name)))

(defun gm/run-python ()
  "Run python using configs specified in `gm/run-python-configs'.

Under the hood, it uses `org-babel-python-initiate-session' instead of `run-python'
so that the session gets registered for an org-mode session if needed.
"
  (interactive)
  (let* ((config (gm/get-run-python-config))
	 (host (plist-get config :host))
	 (cmd (plist-get config :cmd))
	 (venv (plist-get config :venv))
	 (pythonpaths (plist-get config :pythonpaths))
	 (name (plist-get config :name)))
    (gm/run-python-with-args host cmd venv pythonpaths name)))

(defun gm/run-python-with-args
    (host &optional cmd venv pythonpaths name)
  (let ((default-directory host)
	(venv (pythonic-python-readable-file-name (concat host venv))))
    (let* ((name (generate-new-buffer-name (or name "Python")))
	   (org-babel-python-command (or cmd "ipython")) 
	   (python-shell-virtualenv-root (or venv "."))
	   (python-shell-extra-pythonpaths (or pythonpaths (list)))
	   (session (read-string (format "session (default %s): " name) nil nil name))
	   (buffer (org-babel-python-initiate-session session)))
    (cond
       ((and (is-python-p) (y-or-n-p "associate this python buffer?"))
	;; python-shell-get-process-name will return this
	(setq-local python-shell-buffer-name session))
       ((and (derived-mode-p 'org-mode))
	;; python-shell-get-process-name will return this
	(message (format "Use `:session %s' in header args manually." session))))
      (switch-to-buffer-other-window buffer)
      ;; buffer
      )))

(defun gm/read-buffer-with-mode (mode)
  "Choose a buffer with given major MODE."
  (interactive)
  (read-buffer (format "%s buffer: " mode) nil t
	       (lambda (b) (provided-mode-derived-p
			    (buffer-local-value 'major-mode (get-buffer b))
			    mode))))

(defun gm/associate-repl-with-python-buffer (&optional repl-buffer)
  (interactive nil 'python-mode 'python-ts-mode)
  (let* ((repl-buffer (or repl-buffer (gm/read-buffer-with-mode 'inferior-python-mode)))
	(session (org-babel-python-without-earmuffs repl-buffer)))
  (setq-local python-shell-buffer-name session)
  ))

(defun gm/register-python-buffer-wth-org (&optional buffer)
  "Use if a python repl was started outside org.

BUFFER is an inferior-python buffer name.

After doing this, one can simply put :session buffer in org source blocks.
This is not needed if a python repl was started with `gm/run-python'.
This is necessary if a python repl was started with built-in `run-python'.
"
  (interactive)
  (let ((buffer (or buffer (gm/read-buffer-with-mode 'inferior-python-mode))))
    (with-current-buffer buffer
      (setq-local org-babel-python--initialized t))))

(defun gm/get-relative-pyroot-filename ()
  "Get filename relative to root. If in dired, return current line, else return buffer file."
  (let* ((pyroot (expand-file-name
		  (or (file-name-concat (vc-root-dir) "..")
		      (flycheck-python-find-project-root 'checker_))))
	 (filename
	  (cond ((derived-mode-p 'dired-mode)
		 (dired-get-filename nil t))
		((buffer-file-name))
		((null (buffer-file-name))
		 (user-error "Current buffer is not associated with a file."))
		)))
    (file-relative-name filename pyroot)))

(defun gm/get-pydef (&optional with-variable no-module)
  (let* ((filename (gm/get-relative-pyroot-filename))
	 (dotted-filename (string-replace "/" "." filename))
	 (module (replace-regexp-in-string "\\.py$" "" dotted-filename))
	 (module (replace-regexp-in-string "^.*python\\.ev\\." "ev." module))
	 (funname (python-info-current-defun))
	 (varname (save-excursion (python-nav-beginning-of-statement)
				  (python-info-current-symbol)))
	 (pydef-no-module (if with-variable
			      (cond ((not funname) varname)
				    ((not varname) funname)
				    ((concat funname "." varname)))
			    (or funname varname)
			    ))
	 (pydef-with-module (concat module "::" pydef-no-module))
	 (pydef (if no-module pydef-no-module pydef-with-module)))
    pydef
	 ))

(defun gm/message-pydef ()
  (interactive)
  (message (gm/get-pydef)))

(defun gm/copy-file-location-pydef ()
  (interactive)
  (let ((module-pydef (gm/get-pydef)))
    (kill-new module-pydef)
    (message "copied: %s" module-pydef)))

(defun gm/copy-pydef-as-import ()
  (interactive)
  (let* ((module-pydef (split-string (gm/get-pydef) "::"))
	 (module (nth 0 module-pydef))
	 (pydef (nth 1 module-pydef))
	 (firstfun (nth 0 (split-string pydef "\\.")))
	 (import (format "from %s import %s" module firstfun))
	 )
    (kill-new import)
    (message "copied: %s" import)))

(defun gm/find-tag-copy-as-import ()
  (interactive)
  (with-current-buffer (find-tag-noselect (or (python-info-current-defun)
					      (python-info-current-symbol)))
    (gm/copy-pydef-as-import)))

(defun gm/copy-file-location-fileno (no-lineno)
  (interactive "P")
  (let* ((filename (gm/get-relative-pyroot-filename))
	 (fmt (if no-lineno "%s" "%s:%s"))
	 (lineno (line-number-at-pos nil t))
	 (filename-lineno (format fmt filename lineno)))
    (kill-new filename-lineno)
    (message "copied: %s" filename-lineno)))

(defun gm/copy-file-location-absolute ()
  (interactive)
  (let* ((filename
	  (cond ((derived-mode-p 'dired-mode)
		 (dired-get-filename nil t))
		((buffer-file-name))
		((null (buffer-file-name))
		 (user-error "Current buffer is not associated with a file."))
		))
	 )
    (kill-new filename)
    (message "copied: %s" filename)))

(defun gm/py-to-org ()
  "Convert python buffer to org-mode file.

Pipes through jupytext and pandoc"
  (interactive nil 'python-mode 'python-ts-mode)
  (let* ((header "jupyter-python")
	(jupytext-cmd (format "~/anaconda3/envs/bastille/bin/jupytext --from py:percent --to ipynb"))
	(pandoc-cmd "~/anaconda3/envs/bastille/bin/pandoc --from ipynb --to org")
	(buffer (get-buffer-create (concat (file-name-sans-extension (buffer-name)) ".org")))
	(command (format "%s | %s" jupytext-cmd pandoc-cmd)))
    (when (shell-command-on-region nil nil command buffer)
      (with-current-buffer buffer
	(replace-regexp-in-region "^\\(#\\+\\(?:begin_src\\|BEGIN_SRC\\) \\)python" (concat "\\1" header)
				  (point-min))
	(org-mode)
	)
      (pop-to-buffer buffer))
  ))

(defun gm/py-to-ipynb-v3 ()
  "Convert python buffer to good old version 3 (banks..) notebook version.

Pipes through jupytext and nbconvert"
  (interactive nil 'python-mode 'python-ts-mode)
  (let* ((jupytext-cmd "~/anaconda3/envs/bastille/bin/jupytext --from py:percent --to ipynb")
	 (nbconvert-cmd "~/anaconda3/envs/bastille/bin/jupyter nbconvert --to notebook --nbformat 3 --stdin --stdout --log-level=50")
	 (buffer (get-buffer-create (concat (file-name-sans-extension (buffer-name)) "-v3.ipynb")))
	 (command (format "%s | %s" jupytext-cmd nbconvert-cmd)))
    (shell-command-on-region nil nil command buffer)
    (pop-to-buffer buffer))
  )

;; ;; ox-ipynb does not work on Python cells. you should change them to ipython or jupyter-python cells.
;; ;; https://github.com/jkitchin/ox-ipynb/issues/42
;; (with-eval-after-load 'ox-ipynb 
;; (setq ox-ipynb-kernelspecs 
;;       (append  ox-ipynb-kernelspecs '((python . (language_info . ((codemirror_mode . ((name . python)
;; 						       (version . 3)))
;; 								  (file_extension . ".py")
;; 								  (mimetype . "text/x-python")
;; 								  (name . "python")
;; 								  (nbconvert_exporter . "python")
;; 								  (pygments_lexer . "python3")
;; 								  (version . "3.5.2"))))))))

(defun gm/org-to-ipynb-v3 ()
  "Convert org-mode buffer to good old version 3 (banks..) notebook version.

Pipes through jupytext and nbconvert"
  (interactive nil 'org-mode)
  (let* ((old-buffer (current-buffer))
  	 (nbconvert-cmd "~/anaconda3/envs/bastille/bin/jupyter nbconvert --to notebook --nbformat 3 --stdin --stdout --log-level=50")
	 (buffer-v3 (get-buffer-create (concat (file-name-sans-extension (buffer-name)) "-v3.ipynb")))
	 (buffer-v4))
    (with-temp-buffer
      (insert-buffer-substring old-buffer)
      (replace-regexp-in-region "^\\(#\\+\\(?:begin_src\\|BEGIN_SRC\\) \\)python" (concat "\\1 jupyter-python")
				  (point-min))
      (setq buffer-v4 (ox-ipynb-export-to-buffer)))
    (with-current-buffer buffer-v4
      (shell-command-on-region nil nil nbconvert-cmd buffer-v3))
    (pop-to-buffer buffer-v3))
  )

;; run org-babel remote sessions from local ones

(defun gm/org-babel-python-evaluate-session
    (session body &optional result-type result-params)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let* ((tmp-src-file (with-current-buffer session (org-babel-temp-file "python-")))
         (results
	  (progn
	    (with-temp-file tmp-src-file (insert body))
            (pcase result-type
	      (`output
	       (let ((body (format org-babel-python--exec-tmpfile
				   (org-babel-process-file-name
				    tmp-src-file 'noquote))))
		 (org-babel-python--send-string session body)))
              (`value
               (let* ((tmp-results-file (with-current-buffer session (org-babel-temp-file "python-")))
		      (body (org-babel-python-format-session-value
			     tmp-src-file tmp-results-file result-params)))
		 (org-babel-python--send-string session body)
		 (sleep-for 0 10)
		 (org-babel-eval-read-file tmp-results-file)))))))
    (org-babel-result-cond result-params
      results
      (org-babel-python-table-or-string results))))
(advice-add 'org-babel-python-evaluate-session :override #'gm/org-babel-python-evaluate-session)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
