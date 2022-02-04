(use-package code-cells)
;; (add-hook 'python-mode-hook 'code-cells-mode-maybe)
(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle))))

(general-def code-cells-mode-map
  "<M-return>" 'code-cells-eval
  "M-p" 'code-cells-backward-cell
  "M-n" 'code-cells-forward-cell)

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))
