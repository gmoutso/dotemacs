;;
;; scrolling
;;
;; see also keys.el!!!
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(pixel-scroll-mode)
;; (good-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

(scroll-bar-mode 0)

;;
;; winner undo
;;
(winner-mode 1)
(define-key winner-mode-map (kbd "C-c <left>") nil)
(define-key winner-mode-map  (kbd "C-x C-z") 'winner-undo)
(global-set-key (kbd "C-x z") 'bury-buffer)
(global-set-key  (kbd "C-x <down>") 'bury-buffer)

;;
;; tab-line-mode
;;
(defun gm/tab-line-buffer-names () (mapcar (lambda (buff) (buffer-name buff)) (tab-line-tabs-window-buffers)))
(defun gm/tab-line-bury-marked-buffers-action (_ignore)
  (let* ((bufs (helm-marked-candidates))
         (killed-bufs (cl-count-if 'bury-buffer bufs)))
    (when (buffer-live-p helm-buffer)
      (with-helm-buffer
        (setq helm-marked-candidates nil
              helm-visible-mark-overlays nil)))
    (message "Bury %s buffer(s)" killed-bufs)))
(defun gm/tab-line-bury-marked-buffers-run-action ()
  "Run bury buffer action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'gm/tab-line-bury-marked-buffers-action)))
(put 'gm/tab-line-bury-marked-buffers-run-action 'helm-only t)
(defclass gm/helm-source-tab-line-buffers (helm-source-buffers) ())
(defun gm/helm-switch-to-tab-line-tab-buffer ()
    (interactive) 
    (let* ((candidates (gm/tab-line-buffer-names)) ;; note needs to call this outside helm
	   (source (helm-make-source "Window buffers" 'gm/helm-source-tab-line-buffers
		     :buffer-list (lambda () candidates)
		     :action (helm-make-actions
			      "Bury buffers" 'gm/tab-line-bury-marked-buffers-action))))
      (helm-add-action-to-source "Bury buffers" 'gm/tab-line-bury-marked-buffers-action source)
      (helm :sources source)))
(global-set-key  (kbd "C-x <up>") 'gm/helm-switch-to-tab-line-tab-buffer)

;;
;; tab-bar-mode
;;
(defun tab-bar-rename-after-create (&rest _) (call-interactively #'tab-bar-rename-tab))
(add-hook 'tab-bar-tab-post-open-functions 'tab-bar-rename-after-create)
;; (variable-pitch-mode 0)
;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (org-mode . mixed-pitch-mode))
;; use variable pitch font
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(setq tab-bar-new-tab-choice "*scratch*")


(windmove-default-keybindings 'shift)
;; https://orgmode.org/manual/Conflicts.html
(with-eval-after-load 'org
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (define-key org-read-date-minibuffer-local-map (kbd "<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))))
; (windmove-default-keybindings '(shift ctrl))
;; (windmove-default-keybindings 'ctrl)
;; (global-set-key (kbd "C-x <up>")  'windmove-up )
;; (global-set-key (kbd "C-x <down>") 'windmove-down )
;; (global-set-key (kbd "C-x <left>")  'windmove-left )
;; (global-set-key (kbd "C-x <right>") 'next-buffer )
;; move between frames
;; (use-package framemove
;;   :config
;;     (setq framemove-hook-into-windmove nil))
;;
;; popwin (close popup windows easily)
;;
;; Special buffers specified in popwin:special-display-config
(use-package popwin
  :config
  (popwin-mode 1)
  )

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
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(general-define-key
 :keymaps
 'python-mode
 "C-a" 'smarter-move-beginning-of-line  ; so that visual-line remap works on other buffers
 "C-e" 'smarter-move-end-of-line)


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

;; tabspaces
(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it. 
  ;; :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  ;; (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  )
