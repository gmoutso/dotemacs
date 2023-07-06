(use-package multi-term)
;; https://github.com/wangchen/redguardtoo-emacs.d/blob/master/lisp/init-term-mode.el
;; https://stackoverflow.com/questions/2886184/copy-paste-in-emacs-ansi-term-shell
(defun gm/term-dabbrev-expand ()
  (interactive)
  (let ((word (word-at-point))
	beg abbreviation)
    (with-temp-buffer
      (insert word)
      (setq beg (point))
      (dabbrev-expand nil)
      (kill-region beg (point))
      )
    (term-send-raw-string (substring-no-properties (current-kill 0)))
    )
  )
;; (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
(general-def term-raw-map
  "C-c C-j" 'term-line-mode
  "M-/" 'gm/term-dabbrev-expand)
(defun gm/term-tmux-set-dir ()
  (interactive)
  (term-send-raw-string
   "printf \"\\033Ptmux;\\033\\033AnSiTh %s\\n\\033\\033AnSiTu %s\\n\\033\\033AnSiTc %s\\n\\033\\\\\" $HOSTNAME $USER $PWD\n"))
(defun gm/term-set-dir ()
  (interactive)
  (term-send-raw-string
   "printf \"\\033AnSiTh %s\\n\\033AnSiTu %s\\n\\033AnSiTc %s\\n\" $HOSTNAME $USER $PWD\n"))
