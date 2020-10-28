;; needs to be set before org is loaded
(setq org-list-allow-alphabetical t)
;; respect content always
(setq org-insert-heading-respect-content t)
;;(setq org-drill-hide-item-headings-p )
;; search through many org files
(require 'helm-org-rifle)
(require 'ox)
;;(require 'org-tempo)
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/latexfragments/")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(use-package ox-beamer)
(use-package helm-org)
(general-def org-mode-map
  "C-c C-j" 'helm-org-in-buffer-headings)

;; mobile org
(setq org-directory "~/Documents/org")

;; (use-package org-download
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; Add handlers for drag-and-drop when Org is loaded.
;;   (with-eval-after-load 'org
;;     (org-download-enable)))  ;; why not hook this into org-mode-hook?
(use-package org-download
  :hook (org-mode . org-download-enable)
  )

;; make M-Ret not break heading content if cursor is not at the end of item
(setq org-insert-heading-respect-content nil)

;; increase math
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))

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

;; org-mode beautiul as a word-processor
;; from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;;
;; hide italics and bold markers
(setq org-hide-emphasis-markers t)

;; ;; make dashes and bullets into unicode bullets
;; (font-lock-add-keywords 'org-mode
;; 			'(("^ +\\([-*]\\) "
;;                         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; ;; better header bullets
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; ;(require 'org-faces)
;; (eval-after-load 'org-faces '(progn
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
;; ; removed face (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-level-1 nil :height 1.50)
;; (set-face-attribute 'org-level-2 nil :height 1.25)
;; (set-face-attribute 'org-level-3 nil :height 1.1)
;; (set-face-attribute 'org-level-4 nil :height 1.1)
;; (set-face-attribute 'org-document-title nil :height 1.50)
;; ))

;; I like one python babel session
;; note the variable is buffer-local
(with-eval-after-load "ob"
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
)
(eval-after-load 'ob-python '(add-to-list 'org-babel-default-header-args:python '(:session . "*org-python*")))
;; (eval-after-load 'ob-ipython '(add-to-list 'org-babel-default-header-args:ipython '(:session . "ipython")))
;; (remove-hook 'org-mode-hook 'ob-ipython-auto-configure-kernels)
;; use <p[tab] for python block
(eval-after-load 'org '(add-to-list 'org-structure-template-alist '("p" . "src python")))
(eval-after-load 'org '(add-to-list 'org-structure-template-alist '("d" . "src dot")))
;; (require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '( ;;(ipython . t)
   (C . t) (python . t) (emacs-lisp . t) (dot . t) (plantuml . t)
   ;; (jupyter . t)
   ))

;; plantuml with babel executable
(setq org-plantuml-jar-path
      (expand-file-name "~/app/plantuml.1.2020.12.jar"))
(setq plantuml-jar-path org-plantuml-jar-path)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel t)


;;; key bindings
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c m") 'org-latex-preview)
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


(setq
 ;; I want to see today and tomorrow by default (was 'week)
 org-agenda-span 2
 ;; start the week from today
 org-agenda-start-on-weekday nil
 ;; in global todo do not ignore some scheduled
 org-agenda-todo-ignore-scheduled nil;'all
 ;;
 ;; org-agenda-todo-ignore-with-date t
 ;;
 ;; skip scheduling line if same entry shows because of a (near) deadline
 org-agenda-skip-scheduled-if-deadline-is-shown nil
 ;;
 ;; The symbol ‘pre-scheduled’ eliminates the deadline prewarning only prior to the scheduled date
 ;; if a near-future deadline appears and you reschedule it for the future, the deadline entry will then be hidden
 org-agenda-skip-deadline-prewarning-if-scheduled nil ;'pre-scheduled
 ;;
 ;; hide entries with deadline/scheduled today or in the future that are done
 ;; note that entries deadline/scheduled in the passed that are done are hidden always anyway
 ;; the purpose of letting this nil is for a happy feeling
 org-agenda-skip-deadline-if-done nil
 org-agenda-skip-scheduled-if-deadline-is-shown nil
 ;;
 ;; skip scheduled delay when entry also has a deadline
 ;; org-agenda-skip-scheduled-delay-if-deadline t
 ;;
 ;; agenda custom views
 org-agenda-custom-commands
 '(("n" "Agenda and TODOs"
  ((agenda "")
   (alltodo "" (
		;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		;; (org-agenda-overriding-header "Items without a deadline or schedule: ")
		)))))
 ;;
 ;; window arrangement
 org-agenda-restore-windows-after-quit t
 org-agenda-window-setup (quote only-window)
 )
; ((opt1 val1) (opt2 val2) ...)

;; in agenda, also display effort %e if it exists
;; (with-eval-after-load 'org-agenda
;;   (add-to-list 'org-agenda-prefix-format '(todo .  " %i %-5:c %-5e "))
;;   (add-to-list 'org-agenda-prefix-format '(agenda .  " %i %-12:c%?-12t %-5e %s ")))


;; timespan property
;; (load-file "org-span.el")

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



;; capture
;; for use in refile
;; use org-agenda-file-to-front, or by setting the org-agenda-files
;; (setq org-agenda-files "~/Dropbox/org/")
(setq-default
 org-todo-file "~/Documents/org/todo.org"
 org-default-notes-file "~/Documents/org/notes.org")
(setq
 ;; this is usually overriden from custom variables in init.el
 org-agenda-files (list org-todo-file org-default-notes-file)
 )
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(global-set-key (kbd "C-c c") 'org-capture)
;; org-capture templates
(setq org-capture-templates
      '(("i" "Insert file to index" entry (file "./index.org") "* %f\n%?")
	("a" "Annotate" plain (function org-annotate-code-capture-finding-location) "%?")
	("w" "Annotate word" plain (function org-annotate-word-capture-finding-location) "%?")
	("p" "Annotate python" plain (function org-annotate-python-capture-finding-location) "%?")
	("t" "Todo" entry (file+headline org-todo-file "Tasks") "* TODO %?\n" :kill-buffer t)
        ;; ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	;;  "* %?\nEntered on %U\n  %i\n  %a")
	("n" "Note" entry (file org-default-notes-file) "* %?\n %a" :kill-buffer t)
	("j" "Journal" entry (function org-journal-find-location)
	 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
	("r" "Roam" plain (function org-roam--capture-get-point)
	 "%?"
	 :file-name "%<%Y%m%d%H%M%S>-${slug}"
	 :head "#+TITLE: ${title}\n"
	 :unnarrowed t)
	))
(setq org-roam-directory "~/Documents/org/roam")
(setq org-capture-templates-contexts '(("p" ((in-mode . "python-mode")))))
(use-package org-annotate-word)
(use-package org-annotate-python)
;; also include the file to refile as header level 1
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido nil)

;; copy current item to Tasks and schedule it for today
(defun org-copy-schedule-today (ARG)
    "refile current item in current buffer"
  (interactive "P")
  (let (
	(date (if ARG
		  (org-read-date nil nil nil "Schedule to:")
		"+0d"))
	(pos (with-current-buffer (find-file-noselect org-todo-file)
               (org-find-exact-headline-in-buffer "Tasks")))
	)
    ;; refile immediately
    (org-refile 3 nil (list "Tasks" org-todo-file nil pos))
    (save-window-excursion ;; come back after this
      ;; go to insertion
      (org-refile-goto-last-stored)
      (org-refile-goto-last-stored)
      ;; change state and schedule
      (org-todo "TODO")
      (org-schedule nil "+0d")
      )
    ))
(defun org-agenda-copy-schedule-today (arg)
  "Schedule the item at point.
ARG is passed through to `org-copy-schedule-today'."
  (interactive "P")
  ;; shall we allow it on all entries?
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (type (marker-insertion-type marker))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 ts)
    (set-marker-insertion-type marker t)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-copy-schedule-today arg)))
    (org-agenda-redo))
    (message "Copied and scheduled %s" ts)))

;;
;; org agenda key bindings
;;
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "SPC") 'org-agenda-tree-to-indirect-buffer)
  (define-key org-agenda-mode-map "C" 'org-agenda-copy-schedule-today)
  )
;;
;; org agenda hooks
;;
;; (defun my-org-agenda-mode-hooks ()
;;   "For use in `org-agenda'."
;;   (local-set-key (kbd "C") 'org-agenda-copy-schedule-today)
;;   )
;; ;; add to hook
;; (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hooks)
(add-hook 'org-agenda-finalize-hook
      (lambda () (remove-text-properties
         (point-min) (point-max) '(mouse-face t)))) 

;; org-journal.el
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
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))
;; (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               ;; "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))



;; howardism.org journal solution
;; (defun get-journal-file-today ()
;;   "Return filename for today's journal entry"
;;    (let ((daily-name (format-time-string "%Y%m%d")))
;;     (expand-file-name daily-name org-journal-dir)))
;; (add-to-list 'org-capture-templates '(
;; 	"j" "Journal Note"
;;          entry (file (get-journal-file-today))
;;          "* Event: %?"
;;          ))
;; (add-to-list 'auto-mode-alist '(".*/journal/[0-9]*$" . org-mode))
;; (defun journal-file-insert ()
;;   "Insert's the journal heading based on the file's name."
;;   (interactive)
;;   (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
;;                       (buffer-name))
;;     (let ((year  (string-to-number (match-string 1 (buffer-name))))
;;           (month (string-to-number (match-string 2 (buffer-name))))
;;           (day   (string-to-number (match-string 3 (buffer-name))))
;;           (datim nil))
;;       (setq datim (encode-time 0 0 0 day month year))
;;       (insert (format-time-string
;; 	       "* %A, %d %B %Y \n" datim)))))
;; (require 'autoinsert)
;; (add-hook 'find-file-hook 'auto-insert)
;; (add-to-list 'auto-insert-alist '(".*/[0-9]*$" . journal-file-insert))


;; call an org-capture frame
;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
;;
;; http://emacs.stackexchange.com/questions/22242/using-org-protocol-with-a-floating-capture-window
;;
;; emacsclient -a "emacs-snapshot --daemon" -c -F "((name . \"org-capture\") (height . 10) (width . 80))" -e "(org-capture nil \"t\")"
;; or better
;; emacsclient -a "emacs-snapshot --daemon" -e "(make-capture-frame)"
;;
;; (defun org-capture-finalize-my-advice (&optional STAY-WITH-CAPTURE)
;;   "delete the frame named org-capture after capturing"
;;   (if (equal "org-capture" (frame-parameter nil 'name))
;;       (delete-frame) ))
;; (advice-add 'org-capture-finalize :after #'org-capture-finalize-my-advice)
;; ;; (advice-remove  'org-capture-finalize 'org-capture-my-finalize-advice)
;; (defun make-capture-frame ()
;;   "Create a new frame and run org-capture. Useful as an OS shrotcut."
;;   (interactive)
;;   ;; (make-frame '((name . "org-capture")
;;   ;;               (width . 120)
;;   ;;               (height . 15)))
;;   (select-frame-by-name "org-capture")
;;   (org-capture nil "c")
;;   (setq mode-line-format nil)
;;   (delete-other-windows))

;; need spaceline
(setq spaceline-org-clock-p t)
;; do not overclock
(setq org-clock-idle-time 90)


(defun org-paste-df ()
  (interactive)
  (with-temp-buffer
    (yank)
    (goto-char 0)
    (while (re-search-forward "^\\(.*\\)$" nil t)
       (replace-match "| \\1 |"))
    (goto-char 0)
    (while (search-forward "\t" nil t)
       (replace-match " | "))
    (kill-region (point-min) (point-max))
    )
  (yank))

(defun org-copy-to-tabs (beg end)
  "copy org table. does not use line"
  (interactive "r")
  (copy-region-as-kill beg end)
  (with-temp-buffer
    (yank)
    (goto-char 0)
    (while (re-search-forward "^ *| *\\(.*\\) *| *$" nil t)
       (replace-match "\\1"))
    (goto-char 0)
    (while (search-forward "|" nil t)
       (replace-match "\t"))
    (kill-region (point-min) (point-max))
    )
  )

(defun org-open-pdf  ()
  "Open pdf file with same name"
  (interactive)
(org-open-file (expand-file-name
		(concat
		 (file-name-sans-extension
		  (or (file-name-nondirectory buffer-file-name))) "." "pdf")
		(file-name-directory buffer-file-name))))

(defun org-paste-link-xclip ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "xclip" nil `(:file ,filename) nil "-selection" "clipboard" "-t" "image/png" "-o")
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(require 'ox-clip)

;; (require 'org-link-edit)
;; (defun jk/unlinkify ()
;;   "Replace an org-link with the description, or if this is absent, the path."
;;   (interactive)
;;   (let ((eop (org-element-context)))
;;     (when (eq 'link (car eop))
;;       (message "%s" eop)
;;       (let* ((start (org-element-property :begin eop))
;;              (end (org-element-property :end eop))
;;              (contents-begin (org-element-property :contents-begin eop))
;;              (contents-end (org-element-property :contents-end eop))
;;              (path (org-element-property :path eop))
;;              (desc (and contents-begin
;;                         contents-end
;;                         (buffer-substring contents-begin contents-end))))
;;         (setf (buffer-substring start end)
;;               (concat (or desc path)
;;                       (make-string (org-element-property :post-blank eop) ?\s)))))))

(defun org-to-html-from-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank).
   https://emacs.stackexchange.com/a/12124/13752"
  (interactive)
  (kill-new (shell-command-to-string "xclip -o -t TARGETS | grep -q text/html && (xclip -o -t text/html | pandoc -f html -t json | pandoc -f json -t org) || xclip -o"))
  (yank))

;;
;; work with pdf tools
;;
(use-package org-noter)
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;;
;; org clock and agenda
;;
;; shortcuts to change clock in agenda

(use-package org-clock-convenience)
  ;; :ensure t
  ;; :bind (:map org-agenda-mode-map
  ;;  	   ("<S-up>" . org-clock-convenience-timestamp-up)
  ;;  	   ("<S-down>" . org-clock-convenience-timestamp-down)
  ;;  	   ("<C-f>" . org-clock-convenience-fill-gap)
  ;;  	   ("<M-f>" . org-clock-convenience-fill-gap-both)))
(general-def org-agenda-mode-map
  "<S-up>" 'org-clock-convenience-timestamp-up
  "<S-down>" 'org-clock-convenience-timestamp-down
  "<C-f>" 'org-clock-convenience-fill-gap
  "<M-f>" 'org-clock-convenience-fill-gap-both)

;; show log to use this
(setq org-agenda-start-with-log-mode t
      org-agenda-use-time-grid nil)
;; shortcut to clock in from helm-org-agenda-files-headings
(defun dfeich/helm-org-clock-in (marker)
  "Clock into the item at MARKER"
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (org-clock-in)))
(eval-after-load 'helm-org
  '(nconc helm-org-headings-actions
          (list
           (cons "Clock into task" #'dfeich/helm-org-clock-in))))
;; graphic in agenda
(use-package org-timeline :ensure t)
(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
;; clock in with mru
(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 100))
(setq org-mru-clock-files #'org-agenda-files)

