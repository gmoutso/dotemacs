;;; themerc.el --- --- theming config ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom configuration file.

;;; Code:

(use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq
   modus-themes-mixed-fonts t
   ;; modus-themes-italic-constructs t
   ;; modus-themes-bold-constructs nil
   ;; modus-themes-region '(bg-only no-extend)
   ;; ;; modus-themes-mixed-fonts t; org-variable-pitch does it instead
   ;; ;; modus-themes-mode-line '(accented)
   ;; modus-themes-tabs-accented nil
   ;; modus-themes-markup '(background)
   modus-themes-org-blocks 'gray-background
   ;; modus-themes-headings '((1 . (background overline variable-pitch 1.5))
   ;;                         (2 . (overline rainbow 1.3))
   ;;                         (3 . (overline 1.1))
   ;;                         (t . (monochrome)))
   )
  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi :no-confirm))

(defconst gm/my-custom-theme 'modus-vivendi)
(defvar gm/my-theme-window-loaded nil)
(defvar gm/my-theme-terminal-loaded nil)
(defun gm/load-daemon-frame-theme-hook (frame)
  (with-selected-frame frame
    (if (display-graphic-p)
      (unless gm/my-theme-window-loaded
          (if gm/my-theme-terminal-loaded
              (load-theme gm/my-custom-theme)
            (load-theme gm/my-custom-theme t))
	  (if (featurep 'org-variable-pitch) (org-variable-pitch-setup))
          (setq gm/my-theme-window-loaded t))
      (unless gm/my-theme-terminal-loaded
        (if gm/my-theme-window-loaded
            (load-theme gm/my-custom-theme)
          (load-theme gm/my-custom-theme t))
	(if (featurep 'org-variable-pitch) (org-variable-pitch-setup))
        (setq gm/my-theme-terminal-loaded t)))))
(defun gm/load-theme-now ()
  (load-theme gm/my-custom-theme t)
  (if (display-graphic-p)
      (setq gm/my-theme-window-loaded t)
    (setq gm/my-theme-terminal-loaded t))
  (if (featurep 'org-variable-pitch) (org-variable-pitch-setup)))

(if (daemonp) 
    (add-hook 'after-make-frame-functions 
	      #'gm/load-daemon-frame-theme-hook)
  (gm/load-theme-now))


(provide 'themerc)
;;; themerc.el ends here
