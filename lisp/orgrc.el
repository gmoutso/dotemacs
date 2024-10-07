(use-package org-variable-pitch
  :hook (org-variable-pitch-minor . org-mode))
(add-hook 'after-init-hook #'org-variable-pitch-setup)
;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))

;; toggle emphases, links, etc when cursor is on them
;; will also render tex with unicode
(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks nil) ;; https://github.com/awth13/org-appear/issues/34
  ;; org-appear-inside-latex is about pretty UTF elements, not image fragments like fragtog
  (org-appear-inside-latex nil) ;; relevant if org-pretty-entities
  (org-pretty-entities nil) ;; non-nil means use UTF8 for \something
  (org-appear-autoentities nil) ;; relavent if org-appear-inside-latex
  (org-appear-autosubmarkers t) ;; relavent if org-appear-inside-latex
  (org-appear-delay 2)
  ) 
;; toggle latex framgents when cursor is on them
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.4)
  (org-fragtog-ignore-predicates '(org-at-table-p))
  )
;; modern look
(use-package org-modern)
(global-org-modern-mode)

;; needs to be set before org is loaded
(setq org-list-allow-alphabetical t)
;; respect content always
(setq org-insert-heading-respect-content t)
;;(setq org-drill-hide-item-headings-p )
;; search through many org files
(require 'helm-org-rifle)
(use-package ox
  :custom
  (org-export-with-smart-quotes nil)
  (org-export-with-emphasize t)
  (org-export-with-special-strings t)
  (org-export-with-fixed-width t)
  (org-export-with-timestamps t)
  (org-export-preserve-breaks nil)
  (org-export-with-archived-trees nil)
  (org-export-headline-levels 3)
  (org-export-time-stamp-file t)
  (org-export-with-toc t)
  (org-export-with-todo-keywords t)
  (org-export-with-sub-superscripts nil)
  )
;;(require 'org-tempo)
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/latexfragments/")
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(use-package ox-beamer)
(use-package helm-org)

(setq org-adapt-indentation nil)
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
;; ob-ipython incompatible with emacs-jupyter
;; (require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '( ;;(ipython . t)
   (shell . t) (C . t) (python . t) (emacs-lisp . t) (dot . t) (plantuml . t)
   ;; (jupyter . t)
   ))

;; plantuml with babel executable
(setq org-plantuml-jar-path
      (expand-file-name "~/app/plantuml-1.2023.1.jar"))
(setq plantuml-jar-path org-plantuml-jar-path)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel t)


;;; key bindings
(use-package org
  :bind (("C-c l s"  . org-store-link)
	 (:map org-mode-map
               (("C-c m" . org-latex-preview)
		("C-c l t" . org-toggle-link-display)
		("C-c l i" . org-insert-link)
		("C-c C-." . org-time-stamp)
		("C-c C-j" . helm-org-in-buffer-headings)
		)))
  :custom
  (org-use-sub-superscripts nil))
(use-package org-agenda
  :bind (("C-c a" . org-agenda)))

;; when cycling TODO->DONE insert a CLOSED timestamp
(setq org-log-done t)
;; the CLOSED timestamp should not have time
(setq org-log-done-with-time nil)
;; the CLOSED timestamp should be kept if DONE is deleted
(setq org-closed-keep-when-no-todo t)


(setq
 ;; I want to see week # was today and tomorrow by default (was 'week)
 org-agenda-span 'fortnight
 ;; do not start the week from today (opposite: start on Monday)
 org-agenda-start-on-weekday 1
 ;; in global todo do not ignore some scheduled
 org-agenda-todo-ignore-scheduled nil; 10; nil;'all
 ;;
 ;; org-agenda-todo-ignore-with-date t
 ;;
 ;; skip scheduling line if same entry shows because of a (near) deadline
 org-agenda-skip-scheduled-if-deadline-is-shown t
 ;;
 ;; The symbol ‘pre-scheduled’ eliminates the deadline prewarning only prior to the scheduled date
 org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled;'pre-scheduled
 ;;
 ;; hide entries with deadline/scheduled today or in the future that are done
 ;; note entries deadline/scheduled in the past that are done are hidden always
 ;; the purpose of letting this nil is for a happy feeling
 org-agenda-skip-deadline-if-done nil
 ;;
 ;; skip scheduled delay when entry also has a deadline
 ;; org-agenda-skip-scheduled-delay-if-deadline t
 )
(setq
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
(defun gm/org-hooks ()
  (setq line-spacing '0.25)
  ; (iscroll-mode)
  )
(add-hook 'org-mode-hook 'gm/org-hooks)

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
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(global-set-key (kbd "C-c c") 'org-capture)
;; org-capture templates
(setq org-capture-templates
      '(;("i" "Insert file to index" entry (file+headline "./README.org" "index") "* %f\n%?")
	("i" "Index this file" plain (function org-annotate-index-capture-finding-location) "%?"
	 :unnarrowed t :kill-buffer t)
	;; ("a" "Annotate" plain (function org-annotate-code-capture-finding-location) "%?")
	;; ("w" "Annotate word" plain (function org-annotate-word-capture-finding-location) "%?")
	("p" "annotate Python" plain (function org-annotate-python-capture-finding-location) "%?" :unnarrowed t)
	("f" "describe project Folder" plain (function org-annotate-projects-capture-finding-location) "%?" :unnarrowed t)
	("t" "Todo" entry (file+headline org-todo-file "Tasks") "* TODO %?\n"
	 :kill-buffer t)
        ;; ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	;;  "* %?\nEntered on %U\n  %i\n  %a")
	("n" "Note" entry (file org-default-notes-file) "* %?\n %a" :kill-buffer t)
	("l" "work Log" entry (file "~/Documents/org/worklog.org")
	 "* %? \n:LOGBOOK: \n- Created on %T \n:END:\n")
	("j" "Journal" entry (function org-journal-find-location)
	 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
	))

;; make logbook notes have active timestamp
(add-to-list 'org-log-note-headings '(note . "Note taken on %T"))
;; always add an inactive timestamp CREATED property to captured notes
(defun org-set-created-property (&optional active)
  (interactive)
  (let* ((created "CREATED")
         (fmt (if active "<%s>" "[%s]"))
         (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now))))
(add-hook 'org-capture-before-finalize-hook #'org-set-created-property)



;; org-roam
(use-package org-roam
    :after org
    :init 
    :custom
    (org-roam-directory "~/Documents/org/roam")
    :config
    (org-roam-setup)
    :bind (("C-c n c" . org-roam-capture)
	   ("C-c n f" . org-roam-node-find)
           ("C-c n j" . org-roam-dailies-capture-today)
	   ("C-c n d" . org-roam-dailies-goto-date)
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))
(use-package org-roam-dailies
  :custom
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %(format-time-string org-journal-time-format) %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")
	 :unnarrowed t))))
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))



;; (setq org-capture-templates-contexts '(("p" ((in-mode . "python-mode")))))
(setq org-capture-templates-contexts '(("p" ((in-mode . "python-ts-mode")))))
(use-package org-annotate-word)
(use-package org-annotate-python)
(use-package org-annotate-projects)
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

(defun copy-table-as-tsv ()
  (interactive)
  (unless (org-at-table-p)
    (user-error "No table at point"))
  (let ((table (org-table-to-lisp
		(buffer-substring-no-properties (org-table-begin)  (org-table-end)))))
    (kill-new (orgtbl-to-tsv table '()))))

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
  "Save an image in clipboard, eg a screenshot, into a time stamped unique-named file 
in the same directory as the org-buffer and insert a link to this file."
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
;; (use-package org-timeline :ensure t)
;; (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

;; clock in with mru
(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 100))
(setq org-mru-clock-files #'org-agenda-files)

;;
;; source blocks
;;
(defun gm/org-id-new ()
  "Re-purposing `org-id' hit a snag when colons were forbidden in Source-Block
  names. Adding support for a user-defined Org-Id separator would have fixed
  this but with no benefit to Org-Id. So this function removes the colon
  instead. Taken from https://github.com/grettke/help
 "
  (interactive)
  (let* ((gend (org-id-new "SRC"))
         (newid (replace-regexp-in-string ":" "_" gend)))
    newid))
;; (org-babel-where-is-src-block-head)
;; (org-babel-get-src-block-info)
;; (eq (org-element-type (org-element-context)) 'src-block)
(defun gm/jupyter-source-block-at-point-p ()
  (org-babel-when-in-src-block
     (member (org-element-property :language (org-element-at-point)) '("jupyter-python"))))
(defun gm/source-block-name ()
  (let* ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'src-block)
	(org-element-property :name element)
      (error "Not a jupyter block"))))
(defun gm/org-add-src-name-maybe ()
  "If it doesn't have a NAME property then add one and
   assign it a UUID."
  (interactive)
  (when (gm/jupyter-source-block-at-point-p)
    (if (gm/source-block-name)
	(message "Has name")
      (let ((name (gm/org-id-new)))
	(save-excursion
	  (goto-char (org-babel-where-is-src-block-head))
	  ;; (let ((case-fold-search nil)) (search-backward-regexp "#\\+begin_src"))
	  (beginning-of-line)
	  (insert "#+name: " name "\n")
	  (org-babel-where-is-src-block-result 'insert)
	  )))))

(defun gm/org-result-decorate ()
  (interactive)
  (when (gm/jupyter-source-block-at-point-p)
    (if (not (gm/source-block-name)) (gm/org-add-src-name-maybe))
    (save-excursion
      (goto-char (org-babel-where-is-src-block-result))
      (insert "#+ATTR_LATEX: \n#+NAME: \n#+CAPTION: \n")
    )
  ))
;; automatically add a #+NAME: above a source block
(defun gm/org-add-src-name-maybe-advice (&rest dumby) (gm/org-add-src-name-maybe))
;; (advice-add 'jupyter-org-insert-src-block :after #'gm/org-add-src-name-maybe-advice)
;; (advice-add 'org-insert-structure-template :after #'gm/org-add-src-name-maybe-advice)
;; (advice-remove 'org-insert-structure-template 'gm/org-add-src-name-maybe-advice)
;; (advice-remove 'jupyter-org-insert-src-block  'gm/org-add-src-name-maybe-advice)


(defun gm/find-pydef-at-point (&optional ask)
  "Find defition of symbol at point within the current org document.

If ASK then ask for the symbol to find."
  (interactive "P")
  (let ((word (if ask (read-from-minibuffer "Symbol: ") (symbol-at-point))))
  (gm/find-pydef-in-buffer word)))
(defalias 'gm/org-find-definition 'gm/find-pydef-in-buffer)
(defalias 'gm/org-find-definition-at-point 'gm/find-pydef-at-point)

(defun gm/find-pydef-in-buffer (&optional word)
  "Find defition of WORD within the current org document."
  (interactive)
  (let* ((word (or word (read-from-minibuffer "Symbol: ") (symbol-at-point)))
	 (regex (format "^ *\\(?:def *%s(\\|class *%s(\\|%s *=\\)" word word word))
	 (ncount (count-matches regex (point-min) (point-max))))
    (push-mark)
    (cond ((eq ncount 0) (message "No definition for %s" word))
	  ((eq ncount 1) (progn (goto-char (point-min))
				(search-forward-regexp regex nil t)
				(message "Found unique definition for %s" word)))
	  ((> ncount 1) (occur regex)))))

(defun gm/org-cut-and-dump-to-section ()
  "If return is pressed then cut active region and paste to org-goto location."
  (interactive)
  (save-excursion
    (let ((beg (region-beginning))
	  (end (region-end)))
      (deactivate-mark)
      (org-goto)
    (when (and (use-region-p) (eq org-goto-exit-command 'return))
	(kill-region beg end)
	(end-of-line)
	(let ((case-fold-search nil)) (search-forward-regexp "^\\** "))
	(beginning-of-line)
	(insert "\n")
	(yank)
	(insert "\n")))))

(defun gm/org-show-image-files ()
  (interactive)
  (swiper "\\[\\[file:.*\\(?:png\\)"))

;;
;; attach in the exported pdf a file
;; (\usepackage{attachfile2} is needed)
;;

(org-link-set-parameters
   "attachfile"
   :follow
   (lambda (link-string) (org-open-file link-string))
   :complete
   (lambda ()
     (concat "attachfile:" (abbreviate-file-name (expand-file-name (read-file-name "File: ")))))
   :export
   ;; formatting
   (lambda (keyword desc format)
     (cond
      ((eq format 'html) (source-data-uri desc keyword)); no output for html
      ((eq format 'latex)
       ;; write out the latex command
       (format "\\textattachfile[ucfilespec={%s}, description={attached file %s}, author={%s}, color=1 0 0]{%s}{%s}" (file-name-nondirectory keyword) keyword  "George Moutsopoulos" keyword desc)))))

(defun source-data-uri (desc source)
    "Encode the string in SOURCE to a data uri."
    (format
     "<a class=\"org-source\" href=\"data:text/plain;charset=US-ASCII;base64,%s\">%s</a>"
     (base64-encode-string source) (or desc "source")))

(require 'ox-ipynb)

;;
;; Make preview image color background different to default face
;; (so one can see axes etc in orgmode)
;;

(defcustom gm/org-inline-image-background nil
  "The color used as the default background for inline images.
  When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(defun gm/create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background gm/org-inline-image-background) ;(face-background 'default))
            props)))

(advice-add 'create-image :filter-args
            #'gm/create-image-with-background-color)

(defun gm/org-tab-config ()
  "org python code blocks get tabs converted to spaces and moreover a tab may even be two python indents (8 spaces). 
The scr code special edit is fine at first, but going back to org-mode makes the conversion.
This happens when src block is not aligned the edge (eg begin in column 2). 
Turning off the use of tab in org-mode stops this conversion."
   (setq indent-tabs-mode nil)
   (setq tab-width 4)
   )
(add-hook 'org-mode-hook 'gm/org-tab-config)

(use-package helm-org-ql)
(use-package helm-rg)
(use-package org-z
  :config
  (org-z-mode 1))

;; Org ID Properties
(defun gm/org-id-remove-entry ()
"Remove/delete the ID entry and update the databases.
Update the `org-id-locations' global hash-table, and update the
`org-id-locations-file'.  `org-id-track-globally' must be `t`."
(interactive)
  (save-excursion
    (org-back-to-heading t)
    (when (org-entry-delete (point) "ID")
      (org-id-update-id-locations nil 'silent))))


(defun gm/org-id-remove-all-buffer-entries ()
"Remove/delete all ID entry and update the databases.
Update the `org-id-locations' global hash-table, and update the
`org-id-locations-file'.  `org-id-track-globally' must be `t`."
(interactive)
    (if (or (org-map-entries '(org-entry-delete (point) "ID") nil 'file))
	(org-id-update-id-locations nil 'silent)))

;; (defun gm/org-ispell ()
;;   "Configure `ispell-skip-region-alist' for `org-mode'."
;;   (make-local-variable 'ispell-skip-region-alist)
;;   (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
;;   (add-to-list 'ispell-skip-region-alist '("~" "~"))
;;   (add-to-list 'ispell-skip-region-alist '("=" "="))
;;   (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
;;   ) 
;; (add-hook 'org-mode-hook #'gm/org-ispell)
;; (require 'company)
;; (add-to-list 'company-backends 'company-ispell)

(defvar gm/org-resize-images '(("large" . 1200)
			      ("medium" . 800)
			      ("small" . 500))
  "Sizes used")
(defun gm/org-resize-images ()
  "Resize images in this buffer and redisplay."
  (interactive)
  (let ((org-image-actual-width
	 (alist-get (completing-read "Size: " gm/org-resize-images nil t)
		    gm/org-resize-images nil nil 'equal)))
    (org-redisplay-inline-images)))

(defun gm/org-set-size-images ()
  "Resize images in this session and redisplay.

To make this permanent, use customize `org-image-actual-width'."
  (interactive)
  (setq-local org-image-actual-width (alist-get (completing-read "Size: " gm/org-resize-images nil t)
		    gm/org-resize-images nil nil 'equal))
  (org-redisplay-inline-images))

;; Helm will show you the car of each cell, but return the cdr of the selected entry. 


;; correct python session eval on server
;; (defun org-babel-python-evaluate-session
;;     (session body &optional result-type result-params)
;;   "Pass BODY to the Python process in SESSION.
;; If RESULT-TYPE equals `output' then return standard output as a
;; string.  If RESULT-TYPE equals `value' then return the value of the
;; last statement in BODY, as elisp."
;;   (let* ((tmp-src-file (with-current-buffer session (org-babel-temp-file "python-")))
;;          (results
;; 	  (progn
;; 	    (with-temp-file tmp-src-file (insert body))
;;             (pcase result-type
;; 	      (`output
;; 	       (let ((body (format org-babel-python--exec-tmpfile
;; 				   (org-babel-process-file-name
;; 				    tmp-src-file 'noquote))))
;; 		 (org-babel-python--send-string session body)))
;;               (`value
;;                (let* ((tmp-results-file (with-current-buffer session (org-babel-temp-file "python-")))
;; 		      (body (org-babel-python-format-session-value
;; 			     tmp-src-file tmp-results-file result-params)))
;; 		 (org-babel-python--send-string session body)
;; 		 (sleep-for 0 10)
;; 		 (org-babel-eval-read-file tmp-results-file)))))))
;;     (org-babel-result-cond result-params
;;       results
;;       (org-babel-python-table-or-string results))))

;; (defun org-babel-python-async-evaluate-session
;;     (session body &optional result-type result-params)
;;   "Asynchronously evaluate BODY in SESSION.
;; Returns a placeholder string for insertion, to later be replaced
;; by `org-babel-comint-async-filter'."
;;   (org-babel-comint-async-register
;;    session (current-buffer)
;;    "ob_comint_async_python_\\(.+\\)_\\(.+\\)"
;;    'org-babel-chomp 'org-babel-python-async-value-callback)
;;   (let ((python-shell-buffer-name (org-babel-python-without-earmuffs session)))
;;     (pcase result-type
;;       (`output
;;        (let ((uuid (md5 (number-to-string (random 100000000)))))
;;          (with-temp-buffer
;;            (insert (format org-babel-python-async-indicator "start" uuid))
;;            (insert "\n")
;;            (insert body)
;;            (insert "\n")
;;            (insert (format org-babel-python-async-indicator "end" uuid))
;;            (python-shell-send-buffer))
;;          uuid))
;;       (`value
;;        (let ((tmp-results-file (with-current-buffer session (org-babel-temp-file "python-")))
;;              (tmp-src-file (with-current-buffer session (org-babel-temp-file "python-"))))
;;          (with-temp-file tmp-src-file (insert body))
;;          (with-temp-buffer
;;            (insert (org-babel-python-format-session-value tmp-src-file tmp-results-file result-params))
;;            (insert "\n")
;;            (insert (format org-babel-python-async-indicator "file" tmp-results-file))
;;            (python-shell-send-buffer))
;;          tmp-results-file)))))

(use-package ox-pandoc)
(add-to-list 'org-pandoc-extensions '(ipynb . ipynb))
(add-to-list 'org-pandoc-menu-entry '(?i "to ipynb" org-pandoc-export-to-ipynb))
(defun org-pandoc-export-to-ipynb (&optional a s v b e)
  "Export to ipynb."
  (interactive) (org-pandoc-export 'ipynb a s v b e))
(defcustom org-pandoc-options-for-ipynb nil
  "Pandoc options for ipynb"
  :group 'org-pandoc
  :type org-pandoc-option-type)
(setq org-pandoc-command
      "/home/moutsopoulosg/anaconda3/envs/bastille/bin/pandoc"
      )
 
