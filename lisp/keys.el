;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-l") 'helm-locate)
(global-set-key (kbd "C-x /") 'helm-find)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)

;; ace
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; treemacs
(global-set-key (kbd "C-x C-t") 'treemacs)

;; PERSISTENT viewing keys in minor mode
(defvar my-keys-mode-map (make-keymap) "my-keys-mode keymap.")
(define-minor-mode my-keys-mode
  "A minor mode with my keys."
  t " myKeys" 'my-keys-mode-map)
(my-keys-mode 1)
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-mode))
      (let ((mykeys (assq 'my-keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys)))
  (if (not (eq (car (car minor-mode-map-alist)) 'scroll-all-pdf-mode))
      (let ((mykeys (assq 'scroll-all-pdf-mode minor-mode-map-alist)))
        (assq-delete-all 'scroll-all-pdf-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;; Scrolling.
;; (defun gm/scroll-up-line () (interactive) (iscroll-up))
;; (defun gm/scroll-down-line () (interactive) (iscroll-down))
(defun gm/scroll-up-line () (interactive) (scroll-up 1))
(defun gm/scroll-down-line () (interactive) (scroll-down 1))
;; (defun gm/scroll-down-line-two ()  (interactive)  ;; (scroll-down-line 2)
       ;; (iscroll-down 2))
(define-key my-keys-mode-map (kbd "M-j") 'gm/scroll-up-line)
(define-key my-keys-mode-map (kbd "M-k") 'gm/scroll-down-line)
;; (define-key my-keys-mode-map (kbd "M-j") 'good-scroll-up)
;; (define-key my-keys-mode-map (kbd "M-k") 'good-scroll-down)

(defun my-minibuffer-setup-hook ()
  (my-keys-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; gdb navigation keys in minor mode
(defvar my-gdb-navigation-mode-map (make-keymap) "my-gdb-navigation-mode keymap.")
(define-minor-mode my-gdb-navigation-mode
  "A minor mode so that I navigate gdb steps."
  nil " gdbNav" 'my-gdb-navigation-mode-map)
(define-key my-gdb-navigation-mode-map (kbd "s") 'gud-step)
(define-key my-gdb-navigation-mode-map (kbd "n") 'gud-next)
(define-key my-gdb-navigation-mode-map (kbd "c") 'gud-cont)

;; kill this buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Which key intergration
;; https://github.com/justbur/emacs-which-key/issues/306
(use-package which-key)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode))
(which-key-mode)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
