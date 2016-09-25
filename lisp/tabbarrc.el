(setq tabbar-ruler-global-tabbar nil) 
    
;; customize to show all normal files in one group

(defun my-tabbar-buffer-groups () 
   "Returns the name of the tab group names the current buffer belongs to."
(list (cond 
((string-equal "*" (substring (buffer-name) 0 1)) "internal")
((eq major-mode 'dired-mode) "dired")
((eq major-mode 'org-mode) "org")
((memq major-mode '(latex-mode bibtex-mode)) "tex")
((memq major-mode '(c-mode c++-mode makefile-mode cmake-mode python-mode)) "dev")
((eq major-mode 'Emacs-Lisp) "lisp")
(t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; Windows-like C-tab
(defmacro defun-prefix-alt (name on-no-prefix on-prefix)
`(defun ,name (arg)
 (interactive "P")
 (if (equal nil arg) ,on-no-prefix ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group))
(global-set-key (kbd "<C-tab>")  'shk-tabbar-next)
(global-set-key (kbd "<C-S-iso-lefttab>")  'shk-tabbar-prev)

;; ;; tabbar-ruler does not work in server mode
(defun reload-tabrule ()
   "load tabbar-ruler and reload tabbarrc"
   (interactive)
   (tabbar-ruler-up)
   (load-library "tabbarrc")
   )
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (select-frame frame)
;;             (reload-tabrule)))

;; do not show icons in helm (mode-icons-mode)
(setq mode-icons-change-mode-name nil)
(mode-icons-reset)
