(require 'ace-window)

;; add ace option to helm switch buffer
(defun helm-buffer-ace-window (buffer)
  "Use ‘ace-window’ to select a window to display BUFFER."
  (ace-select-window)
  (switch-to-buffer buffer))
(add-to-list 'helm-type-buffer-actions
             '("Switch to buffer in Ace window ‘C-c C-e'" . helm-buffer-ace-window)
             :append)
(defun helm-buffer-run-ace-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-buffer-ace-window)))
(define-key helm-buffer-map (kbd "C-c C-e") #'helm-buffer-run-ace-window)
;; allow ace in helm find file
(defun helm-find-ace-window (file)
  "Use ‘ace-window' to select a window to display FILE."
  (ace-select-window)
  (find-file file))
  (add-to-list 'helm-find-files-actions
             '("Find File in Ace window" . helm-find-ace-window)
             :append)
(defun helm-file-run-ace-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-file-ace-window)))
(define-key helm-find-files-map (kbd "C-c C-e") #'helm-file-run-ace-window)
(define-key helm-projectile-find-file-map (kbd "C-c C-e") #'helm-file-run-ace-window)

;; use ace in dired o shortcut
(defun find-file-ace-window ()
  "Use ace window to select a window for opening a file from dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (> (length (aw-window-list)) 1)
        (aw-select "" (lambda (window)
                        (aw-switch-to-window window)
                        (find-file file)))
      (find-file-other-window file))))
(define-key dired-mode-map "o" 'find-file-ace-window)
