;;; rsync-project.el --- Rsync current file to a remote project root -*- lexical-binding: t; -*-

;; Author: George Moutsopoulos
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (rsync-mode "0.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/moutsopoulosg/rsync-project

;;; Commentary:

;; Provides a command to rsync the current buffer's file to a remote
;; project root, using `rsync-mode' for the underlying transfer.

;;; Code:

(require 'tramp)
(require 'project)
(use-package rsync-mode
  :custom
   (rsync-default-excluded-dirs . (".git"
				   "data"
				   ".ipynb_checkpoints"
				   ".pytest_cache"
				   "venv"
				   "*.egg-info"))
   )

(defun rsync-project--tramp-to-shell (file-or-path)
  "Convert a TRAMP FILE-OR-PATH to an rsync-compatible shell path."
  (with-parsed-tramp-file-name file-or-path tfop
    (format "%s%s:%s"
            (if tfop-user (format "%s@" tfop-user) "")
            tfop-host
            tfop-localname)))

(defmacro rsync-project--value-if-bound (var)
  "Return the value of VAR if it is bound, otherwise nil."
  `(if (boundp (quote ,var)) ,var))

;;;###autoload
(defun rsync-project-file ()
  "Rsync the current file to a remote project root.
Uses `rsync-mode' for the underlying transfer.  The remote
destination is chosen from `rsync-remote-paths' if set, or
prompted interactively."
  (interactive)
  (let* ((file-absolute
          (file-truename (gm/get-filename (buffer-file-name) t)))
         (project-root (or (rsync-project--value-if-bound rsync-local-path)
                           (project-root (project-current))))
         (file-relative (file-relative-name file-absolute project-root))
         (remote-root (if (bound-and-true-p rsync-remote-paths)
                          (completing-read "Rsync project to: " rsync-remote-paths nil t)
                        (rsync-project--tramp-to-shell
                         (read-directory-name "Remote root: ")))))
    (rsync--run remote-root nil project-root nil file-relative)))

(provide 'rsync-project)
;;; rsync-project.el ends here
