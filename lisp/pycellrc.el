(use-package code-cells
  :custom
  (code-cells-convert-ipynb-style
   '(("/home/moutsopoulosg/anaconda3/envs/bastille/bin/jupytext" "--update" "--to" "ipynb")
    ("/home/moutsopoulosg/anaconda3/envs/bastille/bin/jupytext" "--to" "py:percent")
    nil code-cells-convert-ipynb-hook))
  :hook
  ((python-mode . code-cells-mode-maybe))
  )
;; (add-hook 'python-mode-hook 'code-cells-mode-maybe)

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle))
    (define-key map (kbd "C-c C-c") 'code-cells-eval)))

(general-def code-cells-mode-map
  "<M-return>" 'code-cells-eval
  "M-p" 'code-cells-backward-cell
  "M-n" 'code-cells-forward-cell)

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))


(defcustom gm/code-cells-convert-ipynb-maybe t "Whether to auto-convert ipynb files.")
(defun gm/code-cells-convert-ipynb-maybe ()
  "Converts to ipynb if variable is set to true."
  (if gm/code-cells-convert-ipynb-maybe (code-cells-convert-ipynb) (json-mode))
  )
(defun gm/code-cells-convert-ipynb-maybe-toggle ()
  "Toggle whether to auto-convert ipynb files with code-cells."
  (interactive)
  (setq gm/code-cells-convert-ipynb-maybe (not gm/code-cells-convert-ipynb-maybe)))
(setq auto-mode-alist (remove (rassoc 'code-cells-convert-ipynb auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . gm/code-cells-convert-ipynb-maybe))

(defun gm/code-cells-test-roundtrip (&optional filename)
  (interactive)
  (let ((filename (gm/get-filename filename)))
    (if (not (string-equal (file-name-extension filename) "ipynb"))
	(error "Only on ipynb files."))
    (async-shell-command (format "/home/moutsopoulosg/anaconda3/envs/bastille/bin/jupytext --update --to py:percent --test %s" filename)
			 "*code-cells-roundtrip*")
    )
  )
