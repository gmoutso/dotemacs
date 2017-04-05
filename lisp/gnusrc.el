(require 'nnir)
(require 'smtpmail-multi)
(require 'smtpmail)

;;
;; smtpmail defaults
;; (starttls requires gnutls-bin)
;;
(setq
send-mail-function 'smtpmail-send-it
user-full-name "George Moutsopoulos"
message-send-mail-function 'smtpmail-send-it
user-mail-address "gmoutso@gmail.com"
smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
smtpmail-default-smtp-server "smtp.gmail.com"
smtpmail-smtp-server "smtp.gmail.com"
smtpmail-smtp-service 587
smtpmail-stream-type  'starttls
starttls-extra-arguments nil
starttls-gnutls-program "/usr/bin/gnutls-cli"
starttls-use-gnutls t
)

;;
;; allow also gmail-like server-side search (has:attachment newer_than:2d etc)
;;
(require 'imap-search)
(setq gmail-search-default-groups '(("nnimap:gmail" ("nnimap+gmail:[Google Mail]/All Mail"))))
(setq imap-search-default-groups '( ("nnimap:yahoo" ("nnimap+yahoo:Inbox")) ("nnimap:gmail" ("nnimap+gmail:[Google Mail]/All Mail")) ))



;;
;; smtpmail-multi
;;
(setq smtpmail-multi-accounts
      '((gmail . ("gmoutso@gmail.com" ;username
                 "smtp.gmail.com" ;server
                 587 ;port
                 "gmoutso@gmail.com" ;mail-from id
                 starttls nil nil nil))
        (yahoo . ("gmoutso@yahoo.com"
                   "smtp.mail.yahoo.com"
                   587
                   "gmoutso@yahoo.com"
                   starttls
                   nil nil nil)))
      smtpmail-multi-associations
       '(("gmoutso@gmail.com" gmail)
	 ("gmoutso@yahoo.com" yahoo)
	 )
       smtpmail-multi-default-account 'gmail
       message-send-mail-function 'smtpmail-multi-send-it
       )

(setq gnus-select-method '(nnnil ""))

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; replace from header with to header in sent folders
(setq gnus-ignored-from-addresses '("gmoutso@yahoo.com" "gmoutso@gmail.com")
      gnus-summary-to-prefix ""
      gnus-summary-newsgroup-prefix "")

(setq gnus-secondary-select-methods '(
  (nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Google Mail]/Bin")
                      (nnmail-expiry-wait immediate)
		      (nnimap-fetch-partial-articles "text/")
		      )
  (nnimap "yahoo"
                      (nnimap-address "imap.mail.yahoo.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+yahoo:Trash")
                      (nnmail-expiry-wait immediate)
		      (nnimap-fetch-partial-articles "text/")
		      )
  ))

;; do not save or read .newsrc 
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)


;;
;; line formats
;;
(setq-default
 gnus-summary-line-format "%U%R%O %&user-date;  %-20,20f %B%s\n"
 ;; gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-user-date-format-alist ;; fix padding
 '(((gnus-seconds-today)
  . "today %H:%M")
 ((+ 86400
     (gnus-seconds-today))
  . "yest. %H:%M")
 (604800 . " %a  %H:%M")
 ((gnus-seconds-month)
  . "%dth  (%a)")
 ((gnus-seconds-year)
  . "%dth %b  ")
 (t . "%Y-%m-%d"))
 gnus-group-line-format ;; "%M%S%p%P%5y:%B%(%g%)\n")
 "%S%p%P %(%-40g%): %4y unread, %2T ticked\n"
 gnus-summary-display-arrow nil)
;;
;; threading formatting
;;
(setq-default
 ;; do not show threads by default
 gnus-show-threads nil
 ;; but when you do, use references
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 ;; reverse date (or number) order
 gnus-thread-sort-functions '((not gnus-thread-sort-by-date))
 gnus-article-sort-functions '((not gnus-article-sort-by-date))
 ;; formating
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│"
 ;; gnus-downloaded-mark "📩" ;+
 ;; gnus-downloadable-mark "⮯" ;%
 ;; gnus-undownloaded-mark "⭽" ;-
 ;; gnus-cached-mark "🖫" ;*
 )
;; experiment
;(setq gnus-treat-buttonize)

;;
;; on startup, only read inboxes!
;;
;; makes it faster
;; inbox 1, sent 2, unsubscribed 6, rest 3
(setq gnus-activate-level 2
      gnus-group-list-inactive-groups nil)

;; set gnus-parameter
;; 1) show all (even unread) or a number N of messages
(setq gnus-parameters
  '(("nnimap.*"
     (display . 20)
     )
     ))
;;
;; html message rendering
;;
;; set renderer for html mail to w3m in emacs
(setq mm-text-html-renderer 'shr ;;default seems best renderer!
      shr-color-visible-luminance-min 75
      gnus-inhibit-images nil)

;;
;; faster and easier text emails
;;
(setq nnimap-fetch-partial-articles "text/"
      gnus-check-new-newsgroups nil)

; Archive outgoing email in Sent folder on imap.gmail.com:
;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;;       gnus-message-archive-group "[Gmail]/Sent Mail")
;; gmail automatically saves in sent
(setq gnus-message-archive-group nil)
;; https://searchcode.com/file/115586788/gnus.el
;;[[http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus]]
(setq gnus-large-newsgroup nil)


;;
;; offline agent and cache
;;
(defvar article-old-days 30 "How old is old an article. Used in article-old-p and gnus-agent.")
(defun article-old-p ()
  "Say whether an article is old. Customize article-old-days. To be used in customization of gnus-agent categories."
  (> (- (time-to-days (current-time)) (time-to-days (date-to-time (mail-header-date gnus-headers))))
     article-old-days))
(setq
      ;; gnus-auto-goto-ignores 'undownoaded ;; re maneuvering
      ;; expiring
      gnus-agent-enable-expiration 'ENABLE
      gnus-agent-expire-days 0 ;; expire if 0 day old and expirable
      gnus-agent-expire-all nil ;; only read to be expired
      ;;
      gnus-agent-cache nil ;; prefer online imap to local storage
      gnus-fetch-old-headers nil ;; t:for building complete threads / nil much faster
      gnus-agent-synchronize-flags 'ask
      gnus-agent-mark-unread-after-downloaded nil ;; we might want also unread downloaded
      gnus-agent-consider-all-articles nil ;; only download unread (the default) when nil / run a predicate on them whatever the value
      gnus-use-cache t ;; save locally ticked (important) and dormant (semi-important) articles / remove when read
      )


;; shortcuts
;;
(with-eval-after-load 'gnus
  (define-key gnus-summary-mode-map (kbd "C-c C-v") 'gnus-article-browse-html-article)
  (define-key gnus-article-mode-map (kbd "C-c C-v") 'gnus-article-browse-html-article)
  )
