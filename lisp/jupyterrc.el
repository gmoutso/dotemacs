(use-package jupyter
  :after (ob-jupyter ob-python)
  :config
  (setq jupyter-api-authentication-method 'password)
  (setq jupyter-eval-use-overlays nil)
  (setq org-babel-default-header-args:jupyter-python '((:session . "/jpy:localhost#8888:py")
                                                       (:kernel . "conda-env-edge-py")
                                                       (:async . "yes")
						       (:pandoc t)))
  (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names)
  (setq ob-async-no-async-languages-alist '("jupyter-python"))
  (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python")))

(use-package jupyter-tramp)

(conda-env-activate "bastille")
(use-package ob-jupyter
    :after (ob))

;;
;; jupyter repl
;;
(general-def jupyter-repl-interaction-mode-map
  "<M-return>" 'jupyter-send-fold-or-section-and-step
  "C-c C-p" 'jupyter-repl-pop-to-buffer)
;; (general-def ein:notebook-mode-map 'ein:shared-output-pop-to-buffer)

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

