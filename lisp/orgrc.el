(setq org-latex-preview-ltxpng-directory "~/.emacs.d/latexfragments/")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; mobile org
(setq org-directory "~/Dropbox/org")

;; make M-Ret not break heading content if cursor is not at the end of item
(setq org-insert-heading-respect-content nil)

;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name) (file-exists-p (buffer-file-name))
;;        (progn
;; 	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
;; 	 (global-auto-revert-mode t)
;; 	 (reftex-parse-all)
;; 	 ;add a custom reftex cite format to insert links
;; 	 (reftex-set-cite-format
;; 	  '((?b . "[[bib:%l][%l-bib]]")
;; 	    (?n . "[[notes:%l][%l-notes]]")
;; 	    (?p . "[[papers:%l][%l-paper]]")
;; 	    (?t . "%t")
;; 	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
;;   (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)
;; (defun org-mode-reftex-search ()
;;   ;;jump to the notes for the paper pointed to at from reftex search
;;   (interactive)
;;   (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

;; allow to send emails from org-mode
(require 'org-mime)
(setq org-mime-default-header "#+OPTIONS: latex:t toc:nil\n")

;; org-mode beautiul as a word-processor
;; from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;;
;; hide italics and bold markers
(setq org-hide-emphasis-markers t)
;; make dashes and bullets into unicode bullets
(font-lock-add-keywords 'org-mode
			'(("^ +\\([-*]\\) "
                        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; better header bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; use variable pitch font
(add-hook 'org-mode-hook 'variable-pitch-mode)
;(require 'org-faces)
(eval-after-load 'org-faces '(progn 
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
; removed face (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch)
(set-face-attribute 'org-level-1 nil :height 1.50)
(set-face-attribute 'org-level-2 nil :height 1.25)
(set-face-attribute 'org-level-3 nil :height 1.1)
(set-face-attribute 'org-level-4 nil :height 1.1)
(set-face-attribute 'org-document-title nil :height 1.50)
))
;; I like one python babel session
;; note the variable is buffer-local
(eval-after-load 'ob-python '(add-to-list 'org-babel-default-header-args:python '(:session . "*org-python*")))
(eval-after-load 'ob-ipython '(add-to-list 'org-babel-default-header-args:ipython '(:session . "ipython")))
;; use <p[tab] for python block
(eval-after-load 'org '(add-to-list 'org-structure-template-alist '("p" "#+BEGIN_SRC ipython :session\n?\n#+END_SRC")))
(require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t) (C . t) (python . t) (emacs-lisp . t)
   ))

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)

;;; key bindings
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c m") 'org-toggle-latex-fragment)
  (define-key org-mode-map (kbd "C-c l t") 'org-toggle-link-display)
					;use narrow instead C-x n s/b/e/w
					;(define-key org-mode-map (kbd "C-c b") 'org-tree-to-indirect-buffer)
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp)
     ;; (define-key org-mode-map (kbd "C-c a") 'org-agenda)
     )
(define-key global-map "\C-ca" 'org-agenda)

;; when cycling TODO->DONE insert a CLOSED timestamp
(setq org-log-done t)
;; the CLOSED timestamp should not have time
(setq org-log-done-with-time nil)
;; the CLOSED timestamp should be kept if DONE is deleted
(setq org-closed-keep-when-no-todo t)

;; org-mode shortcuts
(add-hook 'org-mode-hook 
	  (lambda()
	    (setq line-spacing '0.25)
	    )
	  )

;;
;; org-publish projects
;;
;; (setq org-publish-project-alist
;;   '(("html"
;;      :base-directory "~/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/org/exports"
;;      :publishing-function org-publish-org-to-html)
;;     ("pdf"
;;      :base-directory "~/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/org/exports"
;;      :publishing-function org-publish-org-to-pdf)
;;     ("all" :components ("html" "pdf"))))



;;; org capture-refile-archive
;; for use in refile
;; use org-agenda-file-to-front, or by setting the org-agenda-files
;; (setq org-agenda-files "~/Dropbox/org/")
(setq org-agenda-files
    '("~/Dropbox/org/notes.org" "~/Dropbox/org/finance/jobs.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
;; for use in capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Dropbox/org/notes.org")
;; org-capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ;; ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	;;  "* %?\nEntered on %U\n  %i\n  %a")
	("c" "Clipboard note" entry (file "~/Dropbox/org/notes.org")
	 "* %?\n %x %i")
	("e" "Empty note" entry (file nil) "" :empty-lines 1 :kill-buffer t)
	("s" "Source Code Snippet" entry
         (file nil)
         ;; Prompt for tag and language
         "* %^{header description}\n#+BEGIN_SRC %^{language|ipython|c++|shell} \n %i %? \n#+END_SRC")))
;; also include the file to refile as header level 1
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido nil)


;; org journal
;; Quick summary:
;; To create a new journal entry for the current time and day: C-c C-j
;; To open today's journal without creating a new entry: C-u C-c C-j
;; In calendar view: j to view an entry in a new buffer
;;                   C-j to view an entry but not switch to it
;;                   i j to add a new entry
;;                   f w to search all entries of the current week
;;                   f m to search all entries of the current month
;;                   f y to search all entries of the current year
;;                   f f to search all entries of all time
;;                   [ to go to previous entry
;;                   ] to go to next entry
;; When viewing a journal entry: C-c C-b to view previous entry
;;                               C-c C-f to view next entry
(require 'org-journal)
(setq org-journal-dir (file-name-as-directory (expand-file-name "journal" org-directory)))

;; call an org-capture frame
;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
;;
;; http://emacs.stackexchange.com/questions/22242/using-org-protocol-with-a-floating-capture-window
;;
;; emacsclient -a "emacs-snapshot --daemon" -c -F "((name . \"org-capture\") (height . 10) (width . 80))" -e "(org-capture nil \"t\")"
;; or better
;; emacsclient -a "emacs-snapshot --daemon" -e "(make-capture-frame)"
;;
(defun org-capture-finalize-my-advice (&optional STAY-WITH-CAPTURE)
  "delete the frame named org-capture after capturing"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame) ))
(advice-add 'org-capture-finalize :after #'org-capture-finalize-my-advice)
;; (advice-remove  'org-capture-finalize 'org-capture-my-finalize-advice)
(defun make-capture-frame ()
  "Create a new frame and run org-capture. Useful as an OS shrotcut."
  (interactive)
  ;; (make-frame '((name . "org-capture")
  ;;               (width . 120)
  ;;               (height . 15)))
  (select-frame-by-name "org-capture")
  (org-capture nil "c")
  (setq mode-line-format nil)
  (delete-other-windows))
