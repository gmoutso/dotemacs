;;; variousrc.el --- ;; enable cua

;;; Commentary:
;; Custom configuration file.

;;; Code:

(use-package savehist
  :init
  (savehist-mode))

;; ;; enable cua
(cua-selection-mode t)
(transient-mark-mode t)
(setq mark-even-if-inactive nil)
;; (cua-mode t)
;; (setq cua-prefix-override-inhibit-delay 0.7)
;; (setq cua-keep-region-after-copy nil)
;; (setq cua-enable-cua-keys nil)

;; remove cua-scrolling
(defun gm/set-cua-scroll ()
     (interactive)
(define-key cua-global-keymap [remap scroll-up]	#'cua-scroll-up)
(define-key cua-global-keymap [remap scroll-down]	#'cua-scroll-down)
(define-key cua-global-keymap [remap scroll-up-command]   #'cua-scroll-up)
(define-key cua-global-keymap [remap scroll-down-command] #'cua-scroll-down)
)
(defun gm/unset-cua-scroll ()
(define-key cua-global-keymap [remap scroll-up]	nil)
(define-key cua-global-keymap [remap scroll-down] nil)
(define-key cua-global-keymap [remap scroll-up-command] nil)
(define-key cua-global-keymap [remap scroll-down-command] nil)
)
(gm/unset-cua-scroll)


(use-package general)
(use-package desktop
  :init
  (setq desktop-dirname user-emacs-directory)
  )
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode 'image-mode)
;; (setq desktop-load-locked-desktop t) ; to always load after a crash
;; (setq desktop-restore-frames nil)
;; (setq desktop-restore-forces-onscreen nil)
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-copy-env "PATH"))
(use-package term/tmux
  :defer t
  :custom
  (xterm-tmux-extra-capabilities '(modifyOtherKeys setSelection)))

(require 'iedit)
(use-package "eshellrc"
  :after eshell
  )
(use-package pdf-tools
  :defer t
  :mode "\\.pdf\\'"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query nil :no-error)
  (load-library "pdf-scroll-other-window")
  )

(require 'expand-region)
;; also consider expreg https://github.com/casouri/expreg that uses treesitter
(global-set-key (kbd "C-M-SPC") 'er/expand-region)


(defun gm/swap-line/up () (interactive)
       (let ((beg) (end))
	 (beginning-of-line)
	 (setq beg (point))
	 (next-line)
	 (setq end (point))
	 (kill-region beg end)
	 (previous-line)
	 (yank)
	 ))

(defun gm/swap-line/down () (interactive)
       (let ((beg) (end))
	 (beginning-of-line)
	 (setq beg (point))
	 (next-line)
	 (setq end (point))
	 (kill-region beg end)
	 (next-line)
	 (yank)
	 ))

(defun gm/edit-path ()
  "Allows a minibuffer edit of path to open new file.

Keeps old as is."
  (interactive)
  (let* ((is-file (buffer-file-name))
	 (old-path (or (buffer-file-name) default-directory))
	 (new-path (read-string "new path: " old-path)))
    (cond ((eq major-mode 'eshell-mode) (eshell/cd new-path))
	  ((eq major-mode 'shell-mode) (insert (format "cd %s" new-path)))
	  ((find-file new-path))))
 )

(add-to-list 'auto-mode-alist '("[Dd]ockerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))

(defun gm/list-buffers-by-mode (mode)
  (seq-filter (lambda (buf) (with-current-buffer buf (derived-mode-p mode))) (buffer-list)))

(defun gm/switch-to-buffer-or-create (mode create-func &optional switch-unique-func)
  "Jump to a buffer of major-mode MODE (without selecting if unique) or create such a buffer using CREATE-FUNC."
  (let* ((bufs (gm/list-buffers-by-mode mode))
	 (n (length bufs))
	 (candidates (mapcar (lambda (i) (cons (buffer-name i) i)) bufs)))
    (cond ((equal n 0) (funcall create-func))
	  ((equal n 1) (if switch-unique-func
			   (funcall switch-unique-func)
			   (switch-to-buffer (car bufs))))
	  (t  (switch-to-buffer (completing-read (format "switch to %s buffer: " mode)
						 candidates nil t)))
	  )))

(defun gm/jump-to-vterm (&optional arg)
  "Switch to vterm (or create) with multiple vterm choice if they exist."
  (interactive "P")
  (if arg (multi-vterm)
  (if (multi-vterm-dedicated-exist-p)
      (multi-vterm-dedicated-open)
      (gm/switch-to-buffer-or-create 'vterm-mode 'multi-vterm 'vterm-toggle))))

(defun gm/org-remove-and-delete-file-link ()
  (interactive)
  (let* ((link (org-element-context))
         (path (org-element-property :path link))
	 )
    (move-file-to-trash path)
    (goto-char (org-element-property :begin link))
    (delete-region (org-element-property :begin link)
                   (org-element-property :end link))
    (set-mark (point))
    (insert (org-element-property :description link))
    (activate-mark)
    )
  )

(defun gm/run-etags ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
   (shell-command "find . -type f -iname \"*.py\" | emacs.etags -")
  ))

(defun gm/shell-command-on-filename (&optional command filename output-buffer)
  "Run a shell COMMAND, replacing %f with FILENAME (default: current buffer's file).
With C-u, prompt for FILENAME."
  (interactive
   (list (read-shell-command "Shell command (%f for filename insertion): ")
          (if current-prefix-arg
                    (read-file-name "File: " nil nil t)
                  (buffer-file-name))
	  (when current-prefix-arg (read-buffer "Output buffer: " "*Shell Command Output*"))))
  (unless filename
    (user-error "No filename provided and buffer is not visiting a file"))
  (let* ((safe-filename (shell-quote-argument filename))
         (final-cmd (replace-regexp-in-string "%f" safe-filename command t t)))
    (shell-command final-cmd output-buffer)))

(defun gm/shell-command-on-buffer (command &optional output-buffer)
  "Run shell COMMAND on the entire buffer as input (stdin).
If OUTPUT-BUFFER is non-nil, insert output there; otherwise, use *Shell Command Output*."
  (interactive
   (list (read-shell-command "Shell command: ")
         (when current-prefix-arg
           (read-buffer "Output buffer: " "*Shell Command Output*"))))
  (shell-command-on-region
   (point-min) (point-max)
   command
   nil output-buffer))

(defun gm/snapshot-backup-file ()
  "Create a backup of the current file in its own directory."
  (interactive)
  (if (buffer-file-name)
      (let ((backup-name (concat (buffer-file-name) ".~" (format-time-string "%Y%m%d-%H%M%S") "~")))
        (copy-file (buffer-file-name) backup-name t)
        (message "Manual backup created: %s" (file-name-nondirectory backup-name)))
    (message "Buffer is not visiting a file!")))

(defun gm/tramp-to-kio (name)
  (let ((fullname (expand-file-name name)))
    (if (file-remote-p fullname)
	   (let* ((struc (tramp-dissect-file-name fullname))
		 (localname (tramp-file-name-localname struc))
		 (host (tramp-file-name-host-port struc))
		 (user (tramp-file-name-user struc))
		 (method (tramp-file-name-method struc))
		 (kioclient (cdr (assoc method '(("ssh" . "fish"))))))
	     (concat kioclient "://" user (if user "@") host localname))
      fullname)))

(defun gm/konsole ()
  (interactive)
  (if (not (file-remote-p default-directory))
      (call-process "konsole" nil 0 nil "--new-tab")
    (let* ((struc (tramp-dissect-file-name default-directory))
	   (localname (tramp-file-name-localname struc))
	   (host (tramp-file-name-host-port struc))
	   (user (tramp-file-name-user struc))
	   (method (tramp-file-name-method struc)))
      (call-process "konsole" nil 0 nil "--new-tab" "-e" method (concat user (if user "@") host localname))
    )))

(defun gm/kde-open (&optional filename)
  "Works remotely and local files.

Does not work with snap firefox because it cannot access hidden files in .cache"
  (interactive)
  (let ((filename (or filename (dired-get-filename nil t) default-directory)))
    ;; (cmd (shell-quote-argument (concat "kde-open5 " (gm/tramp-to-kio filename)))))
    (if filename
	(make-process
         :name "kio-open" :connection-type nil :noquery t
         :buffer nil
	 :command (list  "setsid" "-w" "kde-open5" (gm/tramp-to-kio filename))
	 )
      (message "Cannot guess url to open."))))
(with-eval-after-load 'dired
  (define-key dired-mode-map [remap browse-url-of-dired-file] 'gm/kde-open))

;; ;; esc-esc-esc annoying
(setq-default buffer-quit-function
	      #'(lambda () (message "Are you trying to quit?")))

;; unfill-paragraph from Stefan Monnier <foo at acm.org>.
;; It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; ;; I want to kill-ring-save a whole line if no region is selected
(defun my-kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
	  (goto-char end)
	(goto-char beg))
      (sit-for blink-matching-delay))))

(global-set-key [remap kill-ring-save] 'my-kill-ring-save)
;; I want to kill-region a whole line if no region is selected
(defun my-kill-region (beg end flash)
  "kills the selected region, or kills the whole line (including EOL) at point if a region is not selected."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2) 'flash)))
  (kill-region beg end)
  )
(global-set-key [remap kill-region] 'my-kill-region)

;; ;; sentences end with a single space (for use with sentence navigation)
(setq sentence-end-double-space nil)

;; ;; global visual line mode
(global-visual-line-mode 1)

;; ;; Automatically reload files was modified by external program
(global-auto-revert-mode 1)

;; ;; show matching parentheses (is a global mode)
(show-paren-mode 1)

;; ;; parentheses pairing
;; (electric-pair-mode 1)
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  )


;; ;; turn off menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; ;; Change "yes or no" to "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files t ; stop creating backup~ files
      backup-by-copying t
      ;; backup in one flat place
      backup-directory-alist '(("." . (concat user-emacs-directory "backup-files")))
      delete-old-versions t
      kept-new-versions 2
      version-control t
      ;; kept-old-versions 2
      )
(setq initial-scratch-message "Welcome!\n"
      inhibit-startup-screen t)

;; ;; undo-tree mode
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
)

(use-package recentf
  :init
  (recentf-mode t)
  :config
  ;; (run-at-time nil (* 10 60) 'recentf-save-list)
  )

;; cursor
(setq
 blink-cursor-mode nil
 cursor-type (quote box)
 )



;; tramp
;; solve issue of unix_listener: path "/home/moutsopoulosg/snap/alacritty/common/.cache/emacs/tramp.1f55902153a4e212109d8c0b9d51574abd36be19.m1bxsVtM9G8FDNKC" too long for Unix domain socket
;; https://lists.libreplanet.org/archive/html/tramp-devel/2024-11/msg00007.html
;; (unless small-temporary-file-directory
  ;;          (customize-set-variable
    ;;         'small-temporary-file-directory
      ;;       (format "/run/user/%d/emacs/" (user-uid)))
        ;;    (make-directory small-temporary-file-directory t))

;; (setq tramp-connection-properties nil)
;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "/ssh:")
;;                    "remote-shell" "/bin/bash"))

;; ;; view-mode
(eval-after-load "view"
  '(progn
     (define-key view-mode-map "k" 'View-scroll-line-backward)
       (define-key view-mode-map "j" 'View-scroll-line-forward)))
(define-key ctl-x-map "\C-q" 'view-mode)

;; ;; My signature
;; (setq
;;  browse-url-browser-function 'browse-url-generic
;;  browse-url-generic-program "kde-open5"
;;  browse-url-default-browser "kde-open5";; used by shr
;;  )

;; (setq ffap-machine-p-known 'reject)
;; (put 'narrow-to-region 'disabled nil)
;; (put 'scroll-left 'disabled nil)

(require 'cl-lib)

(defun move-current-file (&optional filename)
  "Move current file and update buffer, with overwrite confirmation."
  (interactive (list (if current-prefix-arg
                        (read-file-name "Move file: ")
                      buffer-file-name)))
  (cl-assert filename nil '("filename is nil"))
  (let* ((target (read-file-name "Choose file or directory: " nil nil nil nil
                                 (lambda (f) (or (file-directory-p f) (file-regular-p f)))))
         (final-target (if (file-directory-p target)
                           (expand-file-name (file-name-nondirectory filename) target)
                         target)))
    (when (and (file-exists-p final-target)
               (not (yes-or-no-p (format "File %s exists. Overwrite? " final-target))))
      (user-error "Move cancelled"))
    (rename-file filename final-target t)
    (set-visited-file-name final-target t t)
    (message "Moved %s to %s" filename final-target)))

(defun copy-current-file (&optional filename)
  "Move current file and update buffer, with overwrite confirmation."
  (interactive (list (if current-prefix-arg
                         (read-file-name "Move file: ")
                      buffer-file-name)))
  (cl-assert filename nil '("filename is nil"))
  (let* ((target (read-file-name "Choose file or directory: " nil nil nil nil
                                 (lambda (f) (or (file-directory-p f) (file-regular-p f)))))
         (final-target (if (file-directory-p target)
                           (expand-file-name (file-name-nondirectory filename) target)
                         target)))
    (when (and (file-exists-p final-target)
               (not (yes-or-no-p (format "File %s exists. Overwrite? " final-target))))
      (user-error "Copy cancelled"))
    (copy-file filename final-target t)
    (message "Copy %s to %s" filename final-target)))

;; rename file and buffer (and maybe git rename)
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting. May use git rename."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " default-directory nil nil (file-name-nondirectory filename))))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


(defun gm/get-filename-dwim ()
  (cond ((derived-mode-p 'dired-mode)
	 (dired-get-filename nil t))
	((buffer-file-name))
	((null (buffer-file-name))
	 (user-error "Current buffer is not associated with a file."))
	)
  )

(defun gm/copy-filename ()
  (interactive)
  "Copy current file name."
  (let ((name (gm/get-filename-dwim)))
    (kill-new name)
    (message "copied %s" name)))

(provide 'variousrc)
;;; variousrc.el ends here
