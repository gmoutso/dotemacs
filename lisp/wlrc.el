;;
;; directories
;;
(setq 
 wl-temporary-file-directory "~/.emacs.d/wanderlust/tmp"
 wl-score-files-directory "~/.emacs.d/wanderlust/elmo"
 elmo-cache-directory "~/.emacs.d/wanderlust/elmo/cache"
 elmo-msgdb-directory "~/.emacs.d/wanderlust/elmo"
 elmo-localdir-folder-path "~/.emacs.d/wanderlust/localMail"
 )



;; Confirm
(setq wl-interactive-exit nil
      wl-interactive-send nil
      wl-interactive-save-folders t
      ;; Confirm prefetching if message size is larger than ‘wl-prefetch-threshold’
      wl-prefetch-confirm nil
      ;; wl-draft-send-confirm-type
      ;; confirm  fetching if message size is larger than ‘elmo-message-fetch-threshold
      elmo-message-fetch-confirm nil
      ;; Confirm if update number exceeds ‘elmo-folder-update-threshold’
      ;; elmo-folder-update-confirm nil
      )

;; check folders at init
;;
(setq wl-auto-check-folder-list '(".*Inbox.*")
      wl-auto-uncheck-folder-list '(".*")
	    )


;;
(defun my-wl-folder-hook ()
  "hook to run when entering the wl-folders first time"
  (wl-folder-open-all)
  (beginning-of-buffer)
  (if (search-forward "Inbox" nil t) (backward-char 5))
  )
(add-hook 'wl-auto-check-folder-hook ;;wl-folder-init-hook
	  'my-wl-folder-hook)
(defun my-wl-summary-hook ()
       "hook to run when entering wl-summary first time"
       (wl-summary-rescan "!date")
       (beginning-of-buffer)
       )
(add-hook 'wl-summary-prepared-hook 'my-wl-summary-hook)

;; need to make the change that ONLY SPC is accepted
;; at end of message buffer
(defun wl-ask-folder (func mes-string)
  (let* (key keve)
    (message "%s" mes-string)
    (setq key (car (setq keve (wl-read-event-char))))
    (if (equal key (string-to-char " "))
	(progn
	  (message "")
	  (funcall func))
      (wl-push (cdr keve) unread-command-events))))


;;
;; summary-line smart date config
;;
(defun wl-summary-line-smart-date ()
  "return the date written in a smart format according to the date"
  (let* ((year   (aref wl-datevec 0))
	 (month  (aref wl-datevec 1))
	 (day    (aref wl-datevec 2))
	 (hour   (aref wl-datevec 3))
	 (minute (aref wl-datevec 4))
	 (date (format "%04d-%02d-%02d %02d:%02d" year month day hour minute))
	 (datenow (current-time-string))
	 (yearnow (string-to-number (substring datenow 20 24)))
	 (monthnow-s (substring datenow 4 7))
	 (months-alist '(
					     ("Jan" . 1)
					     ("Feb" . 2)
					     ("Mar" . 3)
					     ("Apr" . 4)
					     ("May" . 5)
					     ("Jun" . 6)
					     ("Jul" . 7)
					     ("Aug" . 8)
					     ("Sep" . 9)
					     ("Oct" . 10)
					     ("Nov" . 11)
					     ("Dec" . 12)
					     ))
	 (monthnow-i (cdr (assoc monthnow-s months-alist)))
	 (days (days-between datenow date)))
    (cond
     ((< days 1)
      (format "%02d:%02d     " hour day) ;今天
      )
     ((< days 2)
      "yesterday " ;昨天
      )
     ((< days 8)
      (format "%-10.10s" (cdr (assoc (wl-summary-line-day-of-week) '(("Mon" . "Monday") ("Tue" . "Tuesday") ("Wed" . "Wednesday") ("Thu" . "Thursday") ("Fri" . "Friday") ("Sat" . "Saturday") ("Sun" . "Sunday")))) )
      )
     ;; ((and (< days 31) (= monthnow-i month))
     ;;  (format "%4.4s %3.3s  " (cond
     ;; 			       ((or (= day 1) (= day 31)) (format "%02dst" day))
     ;; 			       ((= day 2) "02nd")
     ;; 			       ((= day 3) "03rd")
     ;; 			       (t (format "%02dth" day)))
     ;; 	      monthnow-s)
     ;;  )
     ;; ((= year yearnow)
     ;;  (format "%4.4s %3.3s  " (cond
     ;; 			       ((or (= day 1) (= day 31)) (format "%02dst" day))
     ;; 			       ((= day 2) "02nd")
     ;; 			       ((= day 3) "03rd")
     ;; 			       (t (format "%02dth" day)))
     ;; 	      (car (rassoc month months-alist)))
     ;;  )
          ((= year yearnow)
      (format "%3.3s %4.4s  " 
	      (car (rassoc month months-alist))
	      (cond
				       ((or (= day 1) (= day 31)) (format "%02dst" day))
				       ((= day 2) "02nd")
				       ((= day 3) "03rd")
				       (t (format "%02dth" day)))
		      )
	      )
     (t
      (format "%4d-%02d-%02d" year month day)
      )
     )))

(setq
 wl-summary-line-format-spec-alist
 (put-alist '?a
	    '((wl-summary-line-smart-date))
	    wl-summary-line-format-spec-alist)
;; line format
 wl-summary-line-format
 " %T%P %a %t%[%17(%c %f%) %] %s"
 ;; do not highlight under mouse
 wl-use-highlight-mouse-line nil
 )

;;
;; scroll in long summary buffer
;;
;; (defun wl-summary-hscroll-hook
;;     "allow horizontal scrolling in an unvisual line"
;;   (visual-line-mode 0)
;;   (local-set-key (kbd "<mouse-6>") '(lambda ()
;;                                      (scroll-right 4)))
;;   (local-set-key (kbd "<mouse-7>") '(lambda ()
;;                                      (scroll-left 4)))
;;   )
;; (add-hook 'wl-summary-mode-hook 'wl-summary-hscroll-hook)

(setq wl-summary-toggle-mime "mime")
;; (require 'mime-w3m)
(setq mime-edit-split-message nil)
(setq wl-draft-reply-buffer-style 'full)
(require 'mime-shr)
(setq mime-view-text/html-previewer 'shr
      shr-color-visible-luminance-min 75
      )
(setq
 mime-play-find-every-situations nil
 mime-play-delete-file-immediately nil
 process-connection-type nil
 mime-view-mailcap-files '("~/.mailcap") ;; this needs to be done before SEMI is loade
 )

;;
;; view html mail in browser
;; https://psg.com/lists/wanderlust/msg02750.html
;;
(defun wl-summary-view-html ()
  (interactive)
  (save-window-excursion
    (wl-message-select-buffer wl-message-buffer)
    (let ((done nil)
          (mime-preview-over-to-next-method-alist
           `((wl-original-message-mode .
                                       (lambda ()
                                         (setq done t)))))
          type subtype)
      (while (not done)
        (setq type (cdr (assq 'type (get-text-property (point) 'mime-view-situation))))
        (setq subtype (cdr (assq 'subtype (get-text-property (point) 'mime-view-situation))))
        (if (and (eq type 'text)
                 (eq subtype 'html))
            (progn
              (setq done t)
              (mime-preview-play-current-entity))
          (mime-preview-move-to-next))))))


;;
;; summary buffer
;;
(setq
  ;; sort order
 wl-summary-sort-specs '(!date number subject from list-info size)
 wl-summary-default-sort-spec `!date
 ;; forward prefix
 wl-forward-subject-prefix "Fwd: "
 ;; wl-folder-desktop-name ""
 ;; wl-folder-info-save nil
 ;; uncached read, ie no marks (was "!")
 wl-summary-uncached-mark "O"
 ;; has important (or other) tag (was "$")
 wl-summary-flag-mark "!" 
 wl-summary-width nil
 ;; elmo-imap4-use-cache nil
 )

;;
;; summary buffer shortcuts
;;
(with-eval-after-load 'wl
  ;; $ does nothing
  (define-key wl-summary-mode-map "$" nil)
  ;; c composes
  (define-key wl-summary-mode-map "c" 'wl-summary-write)
  ;; ! marks important
  (define-key wl-summary-mode-map "!" 'wl-summary-mark-as-important)
  ;; (define-key wl-summary-mode-map "m !" 'wl-summary-target-mark-mark-as-important)
  ;; a wide replies
  (define-key wl-summary-mode-map "a" 'wl-summary-reply-with-citation)
  ;; A wide replies without citation
  (define-key wl-summary-mode-map "A" 'wl-summary-reply)
  ;; r mark read
  (define-key wl-summary-mode-map "r" 'wl-summary-mark-as-read)
  ;; (define-key wl-summary-mode-map "m r" 'wl-summary-target-mark-mark-as-read)
  ;; (define-key wl-summary-mode-map "r m" 'wl-summary-target-mark-region)
  ;; hey! use the simpler next-page for scrolling a message
  ;; (was wl-summary-read)
  (define-key wl-summary-mode-map (kbd "SPC") 'wl-summary-next-page)
  (define-key wl-summary-mode-map (kbd "S-SPC") 'wl-summary-prev-page)
  ;;
  ;; (define-key wl-summary-mode-map (kbd "l"))
  ;; shortcut for preview in browser
  (define-key mime-view-mode-default-map (kbd "C-c C-v") nil)
  (define-key mime-view-mode-default-map (kbd "C-c C-v C-f") nil)
  (define-key mime-view-mode-default-map "\C-c\C-vh" nil)
  (define-key mime-view-mode-default-map "\C-c\C-v\C-c" nil)
  (define-key mime-view-mode-default-map (kbd "C-c C-v") 'mime-preview-play-current-entity)
  (define-key wl-summary-mode-map (kbd "C-c C-v") 'wl-summary-view-html)
  ;; use v to toggle view in message too
  (define-key mime-view-mode-default-map "v" 'wl-message-toggle-disp-summary)
  (define-key wl-summary-mode-map "d" nil)
  (define-key wl-summary-mode-map (kbd "<delete>") 'wl-summary-dispose)
)

;;
;; cache management
;;
(setq
 wl-message-buffer-cache-size 10 ;; keep N read articles in cache
       ;; remember you can flush the cache, otherwise they are stored
      ;; wl-message-buffer-prefetch-folder-type-list nil ;; types like imap4
      wl-message-buffer-prefetch-folder-list '(".*inbox.*") ;; prefetch only these folders
      ;; wl-message-buffer-prefetch-depth 1 ;; default is 1
      wl-message-buffer-prefetch-idle-time 3 ;; seconds
      ;; wl-auto-prefetch-first nil ;; default
      )

;; queued message is flushed automatically.
(setq wl-auto-flush-queue t)

;;
;; imap default search
;;
(setq
 ;; use gmail search on All Mail
 wl-quicksearch-folder "%[Google Mail]/All Mail:\"gmoutso@gmail.com\"/clear@imap.gmail.com:993!")


;; default IMAP
;; (setq elmo-imap4-default-server "imap.gmail.com"
;;       elmo-imap4-default-user "gmoutso@gmail.com"
;;       elmo-imap4-default-authenticate-type 'clear
;;       elmo-imap4-default-port '993
;;       elmo-imap4-default-stream-type 'ssl
;;       ;; For non ascii-characters in folder-names
;;       elmo-imap4-use-modified-utf7 t)

;;
;; Multiple SMTP
;;
;; wl-draft-parent-folder => %INBOX:myname/clear@imap.gmail.com:993
(setq
 ;; my identities
 wl-from "George Moutsopoulos <gmoutso@gmail.com>"
 wl-user-mail-address-list
 '("George Moutsopoulos <gmoutso@gmail.com>"
   "George Moutsopoulos <gmoutso@yahoo.com>")
 ;; regexp matching
 wl-draft-config-alist
 '(((string-match "yahoo" wl-draft-parent-folder)
    (template . "yahoo"))
   ((string-match "gmail" wl-draft-parent-folder)
    (template . "gmail"))
   (reply "\\(To\\|Cc\\|Delivered-To\\): .*gmoutso@yahoo.*"
	  (template . "yahoo"))
   (reply "\\(To\\|Cc\\|Delivered-To\\): .*gmoutso@gmail.*"
	  (template . "gmail")))
 ;; templates: cycle with C-c C-j
 wl-template-alist
 '(("gmail"
    (wl-from . "George Moutsopoulos <gmoutso@gmail.com>")
    ("From" . wl-from)
    (wl-smtp-posting-user . "gmoutso@gmail.com")
    (wl-smtp-posting-server . "smtp.gmail.com")
    (wl-smtp-authenticate-type . "plain")
    (wl-smtp-connection-type . 'ssl)
    (wl-smtp-posting-port . 465)
    (wl-local-domain . "gmail.com")
    (wl-message-id-domain . "smtp.gmail.com"))
   ("yahoo"
    (wl-from . "George Moutsopoulos <gmoutso@yahoo.com>")
    ("From" . wl-from)
    (wl-fcc . "%Sent:\"gmoutso@yahoo.com\"/clear@imap.mail.yahoo.com:993!")
    ("Fcc" . wl-fcc)
    (wl-smtp-posting-user . "gmoutso@yahoo.co.uk")
    (wl-smtp-posting-server . "smtp.mail.yahoo.com")
    (wl-smtp-authenticate-type . "plain")
    (wl-smtp-connection-type . 'starttls)
    (wl-smtp-posting-port . 25)
    (wl-local-domain . "yahoo.com")
    (wl-message-id-domain . "smtp.mail.yahoo.com"))
   ))
;; How messages with disposal mark ("d") are to be handled.
;; remove = instant removal (same as "D"), thrash = move to wl-trash-folder
;; string = move to string.
(setq wl-dispose-folder-alist
      '(("^%.*yahoo.*" . "%Trash:\"gmoutso@yahoo.com\"/clear@imap.mail.yahoo.com:993!")
        ("^%.*gmail.*" . "%[Google Mail]/Bin:gmoutso/clear@imap.gmail.com:993!")
        ))

;; SMTP
;; (setq 
;;  wl-smtp-connection-type 'starttls
;;       wl-smtp-posting-port 587
;;       wl-smtp-authenticate-type "plain"
;;       wl-smtp-posting-user "gmoutso"
;;       wl-smtp-posting-server "smtp.gmail.com"
;;       wl-local-domain "gmail.com"
;;       wl-message-id-domain "smtp.gmail.com")


(setq wl-folder-check-async t)

(setq
 ;; All system folders (draft, trash, spam, etc) are placed in the
 ;; [Google Mail]-folder, except inbox. "%" means it's an IMAP-folder
 ;; wl-default-folder "%inbox"
 ;; wl-draft-folder   "%[Google Mail]/Drafts"
 ;; wl-trash-folder   "%[Google Mail]/Trash"
 ;; ;; The below is not necessary when you send mail through Gmail's SMTP server,
 ;; wl-fcc            "%[Google Mail]/Sent"
 ;; Mark sent messages as read
 ;; (placed in the folder wl-fcc)
 wl-fcc-force-as-read    t
 ;; For auto-completing foldernames
 wl-default-spec "%"
 )

(setq wl-draft-always-delete-myself t)

;; ;;; @ bbdb
;; (setq mime-bbdb/use-mail-extr nil)
;; (require 'bbdb-wl)
;; (bbdb-wl-setup)
;; (require 'mime-bbdb)

;; (autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)

;; (add-hook 'wl-mail-setup-hook 'bbdb-insinuate-sendmail)

;; (setq bbdb-use-alternate-names t)

;; (setq bbdb-file "~/.bbdb") 
;; (setq bbdb/mail-auto-create-p   'bbdb-ignore-some-messages-hook)

;; ;; add record to .bbdb manually
;; (setq bbdb-new-nets-always-primary t)
;; (setq bbdb/mail-auto-create-p nil) 

;; ;;; height of BBDB's window
;; (setq bbdb-pop-up-target-lines 7)

;; ;;; popup display of record in the .bbdb
;; (setq bbdb-use-pop-up t)
;; (setq signature-use-bbdb t)
;; (setq bbdb-north-american-phone-numbers-p nil)

;; ;;; automatic adding to ML field
;; (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

;; ;;; hide bbdb field while wl-folder-suspend
;; (defadvice wl-folder-suspend (after wl-bbdb-suspend activate compile)
;;   (interactive)
;; (bbdb-wl-exit-2))
;; (defadvice wl-exit (after wl-bbdb-suspend activate compile)
;;   (interactive)
;; (bbdb-wl-exit-2))

;; (put 'ML 'field-separator "\n")
;; (put 'User-Agent 'field-separator "\n")

;; (setq bbdb-auto-notes-alist
;;        '(
;;        ("X-ML-Name" (".*$" ML 0))
;;        ("X-Mailinglist" (".*$" ML 0))
;;        ("X-Ml-Name" (".*$" ML 0))
;;        ("X-Mailer" (".*$" User-Agent 0))
;;        ("X-Newsreader" (".*$" User-Agent 0))
;;        ("User-Agent" (".*$" User-Agent 0))
;;        ("X-Face" ("[ \t\n]*\\([^ \t\n]*\\)\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
;;                                  face "\\1\\3\\5\\7"))
;;        ))


;; look in zip files as if they were folders
;; (setq elmo-archive-treat-file t)

;;show sent mail by who it was to
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

;;
;; refiling
;;
;; (setq wl-refile-rule-alist
;;       '((("To" "Cc")
;;          ("^wl-en@lists.airs.net" . "+mlists"))))
;;
;; manual refile for gmail and yahoo
;;
(defun djcb-wl-summary-refile (mark-list)
  "refile the current message to FOLDER; if FOLDER is nil, use the default"
  (interactive)
  (let*
      ((this-folder (or
		     (wl-summary-buffer-folder-name) ;; if in summary
		     wl-message-buffer-cur-folder ;; if in message
		     ))
       (gmail-p (string-match ".*gmail.*" this-folder))
       (yahoo-p (string-match ".*yahoo.*" this-folder)))
    (cond (gmail-p
	   ;; gmail needs just deleting from inbox
	   (wl-summary-move-mark-list-messages mark-list
				      'null
				      "Archiving to All Mail"))
	  (yahoo-p
	   (wl-summary-move-mark-list-messages mark-list
				      'null
				      "Archiving to All Mail"))
	  )
    ))
;; (define-key wl-summary-mode-map (kbd "b x") ;; => Project X
;;   '(lambda()(interactive)(djcb-wl-summary-refile ".project-x"))) 
;; (define-key wl-summary-mode-map (kbd "b y") ;; => Project Y
;;   '(lambda()(interactive)(djcb-wl-summary-refile ".project-y")))

;;
;; check if attachments
;; from emacs-fu.blogspot.co.uk
;;
(defun wl-draft-attachment-check ()
  "if attachment is mention but none included, warn the the user"
  (save-excursion
    (goto-char 0)
    (unless ;; don't we have an attachment?
	(re-search-forward "^Content-Disposition: attachment" nil t) 
      (when ;; no attachment; did we mention an attachment?
	  (re-search-forward "attach" nil t)
        (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
          (error "Abort."))))))
(add-hook 'wl-mail-send-pre-hook 'wl-draft-attachment-check)

;; proportion of summary and message
(setq wl-message-window-size '(3 . 7))
;; Fields in the e-mail header that I do not want to see (regexps)
(setq wl-message-ignored-field-list '("^"))
;; Fields in the e-mail header that I want to see even if they match the 
;; regex in wl-message-ignored-field-list
(setq wl-message-visible-field-list '("^To" "^Subject" "^From" "^Date" "^Cc"))



;; Enables auto-fill-mode in the draft buffer
(add-hook 'wl-mail-setup-hook 'auto-fill-mode)
;;auto-fill 
;; (setq mime-edit-mode-hook
;;       '(lambda ()
;;       	 (auto-fill-mode 1)))


  
;; Use orgstruct++-mode in the draft buffer
;; (add-hook 'mail-mode-hook 'turn-on-orgstruct)
(add-hook 'mail-mode-hook 'turn-on-orgstruct++)

(setq mime-acting-situation-example-list
      '((((type . text)
          (subtype . html)
          (mode . "play")
          (method . "chromium-browser %s")
          (major-mode . wl-original-message-mode)
          (body . visible)
          (body-presentation-method . mime-display-text/html)
          (description . t)
          (encoding . "quoted-printable")
          (nametemplate . "%s.html")
          (test . "test -n \"$DISPLAY\"")
          ("charset" . "UTF-8"))
         . 1)))
