;;; emamuxrc.el --- Emamux tmux integration

;;; Commentary:
;; Custom configuration file.

;;; Code:

(use-package ob-emamux)
(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages '((emamux . t))))

(provide 'emamuxrc)
;;; emamuxrc.el ends here