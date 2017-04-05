;;; org-span.el: A task property decribing time span of task
;;; Commentary:
;;; the values of span are like mins, hours, day or days
;;; this is more coarse-grained than the builtin effort property
;;; Code:

;;
;; requirements
;;
(require 'org)
(require 'org-agenda)

;;
;; variables
;;
(defvar org-span-property "Span" "Name of span property.")

;;
;; functions
;;
(defun org-set-span (&optional value increment)
   "Set the span property of the current entry.
 With numerical prefix arg, use the nth allowed value.

 When INCREMENT is non-nil, set the property to the next allowed value. Crudely coppied from set-effort."
   (interactive "P")
   (let* ((completion-ignore-case t)
          (prop org-span-property)
          (cur (org-entry-get nil prop))
          (allowed (org-property-get-allowed-values nil prop 'table))
          (existing (mapcar 'list (org-property-values prop)))
          (heading (nth 4 (org-heading-components)))
	  rpl
          (val (cond
                ((stringp value) value)
                ((and allowed (integerp value))
                 (or (car (nth (1- value) allowed))
                     (car (org-last allowed))))
                ((and allowed increment)
                 (or (caadr (member (list cur) allowed))
                     (user-error "Allowed effort values are not set")))
                (allowed
                 (message "Select number or 0, [RET%s]: %s"
                          (if cur (concat "=" cur) "")
                          (mapconcat 'car allowed " "))
                 (setq rpl (read-char-exclusive))
                 (if (equal rpl ?\r)
                     cur
                   (setq rpl (- rpl ?0))
                   (if (equal rpl 0) (setq rpl 10))
                   (if (and (> rpl 0) (<= rpl (length allowed)))
                       (car (nth (1- rpl) allowed))
                     (org-completing-read "Effort: " allowed nil))))
                (t
                 (let (org-completion-use-ido org-completion-use-iswitchb)
                   (org-completing-read
                    (concat "Effort" (and cur (string-match "\\S-" cur)
                                          (concat " [" cur "]"))
                            ": ")
                    existing nil nil "" nil cur))))))
     (unless (equal (org-entry-get nil prop) val)
       (org-entry-put nil prop val))
     (org-refresh-property
      '((effort . identity)
        (effort-minutes . org-duration-string-to-minutes))
      val)
     (when (equal heading (org-bound-and-true-p org-clock-current-task))
       (setq org-clock-effort (get-text-property (point-at-bol) 'effort))
       (org-clock-update-mode-line))
     (message "%s is now %s" prop val)))

(defun org-agenda-set-span ()
  "Set the span property for the current headline in the agenda."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                      (org-agenda-error)))
        (buffer (marker-buffer hdmarker))
        (pos (marker-position hdmarker))
        (inhibit-read-only t)
        newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
       (widen)
       (goto-char pos)
       (org-show-context 'agenda)
       (call-interactively 'org-set-span)
       (end-of-line 1)
       (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

;;
;; glue into org
;;
;; with-eval-after-load "org"
(add-to-list 'org-global-properties '("Span_ALL" . "mins hrs day days"))
(add-to-list 'org-speed-commands-user '("e" . org-set-span))
;; with-eval-after-load 'org-agenda
(org-defkey org-agenda-mode-map  (kbd "e") 'org-agenda-set-span)


;;
;; package closing
;;
(provide 'org-span)
