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

;; (variable-pitch-mode 0)
;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (org-mode . mixed-pitch-mode))
;; use variable pitch font
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)


(global-set-key (kbd "C-x <up>")  'windmove-up )
(global-set-key (kbd "C-x <down>") 'windmove-down )
(global-set-key (kbd "C-x <left>")  'windmove-left )
(global-set-key (kbd "C-x <right>") 'windmove-right )
;; move between frames
;; (use-package framemove
;;   :config
;;     (setq framemove-hook-into-windmove nil))
;;
;; popwin (close popup windows easily)
;;
;; Special buffers specified in popwin:special-display-config
(require 'popwin)
(popwin-mode 1)
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
;(push '("^\\*anything.*\\*$" :regexp t) popwin:special-display-config)
;; (push '(anaconda-mode-view-mode :dedicated t) popwin:special-display-config)
(push '(Man-mode :dedicated t) popwin:special-display-config)

;;
;; smarter move to first line
;;
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.

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
;(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

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

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(require 'general)
(general-define-key
 :keymaps
 'python-mode
 "C-a" 'smarter-move-beginning-of-line  ; so that visual-line remap works on other buffers
 "C-e" 'smarter-move-end-of-line)
