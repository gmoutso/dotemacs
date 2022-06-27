(conda-env-activate "emacs")
;; use (jupyter-command "kernelspec" "list" "--json" "--log-level=40")
;; /home/moutsopoulosg/.emacs.d/elpa/jupyter-20220419.1852/jupyter-kernelspec.el:64
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
(require 'ob-jupyter)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) (python . t) (emacs-lisp . t) (dot . t) (plantuml . t)
   (jupyter . t)))

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


;; https://github.com/nnicandro/emacs-jupyter/issues/366
;; garbled errors
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)
