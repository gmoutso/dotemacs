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

;; do not use ~/.newsrc (for use with other newsclients)
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)

;; reverse date (number) order
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))

;; set gnus-parameter (does this even work?)
(setq gnus-parameters
  '(("nnimap.*"
     (display . all))))

; Archive outgoing email in Sent folder on imap.gmail.com:
;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;;       gnus-message-archive-group "[Gmail]/Sent Mail")
;; gmail automatically saves in sent
(setq gnus-message-archive-group nil)
;; https://searchcode.com/file/115586788/gnus.el
;;[[http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus]]
(setq gnus-large-newsgroup 'nil)

(setq-default
 gnus-summary-line-format "%U%R%z%I %(%&user-date;  %-15,15f  %s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
)
