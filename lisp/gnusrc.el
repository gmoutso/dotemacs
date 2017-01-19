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
(add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))
;; (setq nnir-imap-default-search-key "gmail")




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
;; (setq gnus-ignored-newsgroups "")

(setq gnus-secondary-select-methods '(
  (nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      ;; (nnmail-expiry-wait 90)
		      )
  (nnimap "yahoo"
                      (nnimap-address "imap.mail.yahoo.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ;; press 'E' to expire email
                      ;;(nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      ;;(nnmail-expiry-wait 90)
		      )
  ))

;; do not save or read .newsrc 
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)


;;
;; summary line format
;;
(setq-default
 gnus-summary-line-format "%U%R%O %(%&user-date;  %-15,15f %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-group-line-format ;; "%M%S%p%P%5y:%B%(%g%)\n")
 "%S%p%P %(%-40g%): %4y unread, %2T ticked\n")
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
 ;; formating
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

;;
;; on startup, only read inboxes!
;;
;; makes it faster
;; inbox 1, sent 2, unsubscribed 6, rest 3
;; (setq gnus-activate-level 6)


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
(setq mm-text-html-renderer 'gnus-w3m)
(setq gnus-inhibit-images nil)

;;
;; faster and easier text emails
;;
(setq nnimap-fetch-partial-articles "text/")

; Archive outgoing email in Sent folder on imap.gmail.com:
;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;;       gnus-message-archive-group "[Gmail]/Sent Mail")
;; gmail automatically saves in sent
(setq gnus-message-archive-group nil)
;; https://searchcode.com/file/115586788/gnus.el
;;[[http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus]]
(setq gnus-large-newsgroup nil)

;;
;; caching
;;
;; To turn caching on, set gnus-use-cache to t. By default, all articles ticked ! or marked as dormant ? will then be copied over to your local cache (gnus-cache-directory). If you  set gnus-use-cache to 'passive', you can still use manually the cache with * and M-*
;; (setq gnus-use-cache 'passive)
;; do we really need caching?
;; caching is different "in use" to offline emailing (see "the gnus agent")

;;
;; offline agent
;;
(setq
      ;; gnus-auto-goto-ignores 'undownoaded ;; re maneuvering
      ;; expiring
      gnus-agent-enable-expiration 'ENABLE
      gnus-agent-expire-days 0 ;; expire if 0 day old and expirable
      gnus-agent-expire-all nil ;; only read to be expired
      ;;
      gnus-agent-cache nil ;; prefer online imap to local storage
      )

