;;
;; scrolling
;;
;; Turn on horizontal scrolling with mouse wheel
;; (global-set-key (kbd "<mouse-6>") '(lambda ()
;;                                      (interactive)
;;                                      (scroll-right 4)))
;; (global-set-key (kbd "<mouse-7>") '(lambda ()
;;                                      (interactive)
;;                                      (scroll-left 4)))

;;
;; winner undo
;;
(winner-mode 1)
(define-key winner-mode-map (kbd "C-c <left>") nil)
(define-key winner-mode-map  (kbd "C-x C-z") 'winner-undo)

;;
;; Windmove
;;
;; continuously move cursor in with arrows between windows
(defun my-cont-windmove ()
  "Continuously move cursor within windows using arrows"
  (interactive)
  (let ((map (make-sparse-keymap)))
    (dolist (x '(("<up>" . windmove-up)
                 ("<left>" . windmove-left)
                 ("<down>" . windmove-down)
                 ("<right>" . windmove-right)))
      (define-key map (read-kbd-macro (car x)) (cdr x)))
    (set-transient-map map t)))

(global-set-key (kbd "C-x <up>")  'windmove-up )
(global-set-key (kbd "C-x <down>") 'windmove-down )
(global-set-key (kbd "C-x <left>")  'windmove-left )
(global-set-key (kbd "C-x <right>") 'windmove-right )

;; Similarly, one should be able to move a window's position continuously
(global-set-key (kbd "C-x <S-up>")
		(lambda () (interactive)
		  (buf-move-up)
		  (buf-move))
		)
(global-set-key (kbd "C-x <S-down>")
		(lambda () (interactive)
		  (buf-move-down)
		  (buf-move))
		)
(global-set-key (kbd "C-x <S-left>")
		(lambda ()
		  (interactive)
		  (buf-move-left)
		  (buf-move))
		)
(global-set-key (kbd "C-x <S-right>")
		(lambda ()
		  (interactive)
		  (buf-move-right)
		  (buf-move))
		)

;; ;; Similarly, one should be able to move a window's position continuously
;; (global-set-key (kbd "C-x <S-up>")
;; 		(lambda () (interactive)
;; 		  (buf-move-up)
;; 		  (buf-move))
;; 		)
;; (global-set-key (kbd "C-x <S-down>")
;; 		(lambda () (interactive)
;; 		  (buf-move-down)
;; 		  (buf-move))
;; 		)
;; (global-set-key (kbd "C-x <S-left>")
;; 		(lambda ()
;; 		  (interactive)
;; 		  (buf-move-left)
;; 		  (buf-move))
;; 		)
;; (global-set-key (kbd "C-x <S-right>")
;; 		(lambda ()
;; 		  (interactive)
;; 		  (buf-move-right)
;; 		  (buf-move))
;; 		)

;;
;; popwin (close popup windows easily)
;;
;; Special buffers specified in popwin:special-display-config
(require 'popwin)
(popwin-mode 1)
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
;(push '("^\\*anything.*\\*$" :regexp t) popwin:special-display-config)
(push '(anaconda-mode-view-mode :dedicated t) popwin:special-display-config)
(push '(Man-mode :dedicated t) popwin:special-display-config)

;;
;; smarter move to first line
;;
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
;(global-set-key [remap move-beginning-of-line]
;                'smarter-move-beginning-of-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)


(require 'newcomment)
(defun move-end-of-code ()
  (interactive)
  (when (comment-search-forward (line-end-position) t)
    (goto-char (match-beginning 0))
    (skip-syntax-backward " " (line-beginning-position))))

(defun smarter-move-end-of-line (arg)
  "Move point to end of line or beginning of comment."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move arg lines forward first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-end-of-line nil)
    (when (= orig-point (point))
      (move-beginning-of-line nil)
      (move-end-of-code))))

(global-set-key (kbd "C-e") 'smarter-move-end-of-line)

;;
;; zoom in/out
;;
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; balance windows
(global-set-key (kbd "C-x C-0") 'balance-windows)

(defun kill-buffer-and-window-or-frame ()
  "Kill current and delete its window. If sole window, then kill frame."
  (interactive)
  (if (one-window-p)
      (progn (kill-buffer)
	     (delete-frame))
    (kill-buffer-and-window))
  )
(substitute-key-definition 'kill-buffer-and-window
                              'kill-buffer-and-window-or-frame
                              global-map)

(defun dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
