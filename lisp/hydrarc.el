(use-package hydra)
(use-package general)
(use-package pretty-hydra)
(use-package major-mode-hydra)
(use-package hera)
(general-def 'override
  "C-c C-m" 'major-mode-hydra)

(pretty-hydra-define hydra-files (:exit t :quit-key "q")
  ("Visit"
   (("f" helm-find-files "files")
    ("/" helm-find "find name")
    ("s" helm-do-grep-ag "grep ag")
    ;; ("b" helm-mini "buffers")
    ;; ("l" helm-locate "locate")
    )
   "Dired"
   (("d" dired "dired")
    ("g" find-grep-dired "grep")
    ("F" find-dired "find")
    )
   "Places"
   (("p" helm-projectile "projects") 
    ("B" helm-filtered-bookmarks "bookmarks")
    ("t" treemacs "treemacs"))
   "Org"
   (
   ("od" helm-org-rifle-directories "rifle dir")
   ("oo" helm-org-rifle-org-directory "rifle org/")
   ("om" (hera-push 'hydra-files-org-more/body) "more.." :exit t)
   ;; ("ob" helm-org-rifle "buffers")
   ;; ("of" helm-org-rifle-files "files")
   ;; ("oa" helm-org-rifle-agenda-files "agenda")
   )))
(defhydra hydra-files-org-more (:exit t)
  "
more org rifle..\n"
   ("c" helm-org-rifle-current-buffer "current")
   ("b" helm-org-rifle "buffers")
   ;; ("d" helm-org-rifle-directories "directories")
   ;; ("o" helm-org-rifle-org-directory "org-directory")
   ("f" helm-org-rifle-files "files")
   ("a" helm-org-rifle-agenda-files "agenda")
   ("q" (hera-pop) "cancel" :exit t))
(general-def 'override
  "C-c C-f" 'hydra-files/body)

;; (pretty-hydra-define hydra-agenda
;;   (:exit nil :quit-key ("q" "<escape>"))
;;    ("Scheduling"
;;    (("s" org-schedule     "org-schedule")
;;     ("d" org-deadline     "deadline"))
;;    "Todo"
;;    (("T" org-todo       "menu")
;;     ("H" org-shiftleft  "left")
;;     ("J" org-shiftdown  "down")
;;     ("K" org-shiftup    "up")
;;     ("L" org-shiftright "right")))
;; )

(major-mode-hydra-define org-mode
  (:exit t :quit-key ("q" "<escape>"))
  (
   "Paste"
   (("yh" org-to-html-from-clip "html")
    ("yd" org-paste-df "dataframe")
    ("yl" org-paste-link-xclip "image")
    )
   "Links"
   (("ls" org-store-link  "store")
    ("li" org-insert-link  "insert")
    ("lt" org-toggle-link-display "toggle")
    ("ii" org-toggle-inline-images "inline image")
    ;; ("f" org-footnote-new "footnote")
    ;; ("o" org-open-at-point  "open link")
    )
   "Jump"
   (("h" helm-org-in-buffer-headings "headings")
    ("r" helm-org-rifle-current-buffer "rifle")
    ("s" helm-occur "search")
     ;("ne" hydra-navigate "navigate emacs")
    )
   "Ox"
   ( ;("nb" org-narrow-to-block "narrow block")
    ; ("nw" widen "widen")
    ("v" org-open-pdf "view pdf")
    ("e" org-latex-export-to-pdf "export pdf" :exit nil))
   ))

;; (defhydra hydra-rifle (:exit t)
;;    ("c" helm-org-rifle-current-buffer "current")
;;    ("b" helm-org-rifle "buffers")
;;    ("d" helm-org-rifle-directories "directories")
;;    ("o" helm-org-rifle-org-directory "org-directory")
;;    ("f" helm-org-rifle-files "files")
;;    ("a" helm-org-rifle-agenda-files "agenda")
;;    ("q" nil "cancel"))

;; (pretty-hydra-define hydra-projectile (:exit t)
;;   ("Open"
;;    (("f" helm-projectile-find-file "file")
;;     ("r" helm-projectile-recent "recent")
;;     ("p" helm-projectile-switch-project "project")
;;     ("d" helm-projectile-find-dir "directory"))
;;    "Search"
;;    (("o" projectile-multi-occur "occur")
;;     ("a" projectile-ag))
;;    "Buffers"
;;    (("b" helm-projectile-switch-to-buffer "switch")
;;     ("k" helm-projectile-kill-buffers "kill"))
;;    "Cache"
;;    (("C" projectile-invalidate-cache "clear")
;;     ("x" projectile-remove-known-project "remove project")
;;     ("X" projectile-cleanup-known-projects "cleanup"))))

;; (pretty-hydra-define hydra-registers (:exit t)
;;   ("Point"
;;    (("r" point-to-register "save point")
;;     ("j" jump-to-register "jump")
;;     ("v" view-register "view all"))
;;    "Text"
;;    (("c" copy-to-register "copy region")
;;     ("C" copy-rectangle-to-register "copy rect")
;;     ("i" insert-register "insert")
;;     ("p" prepend-to-register "prepend")
;;     ("a" append-to-register "append"))
;;    "Macros"
;;    (("m" kmacro-to-register "store")
;;     ("e" jump-to-register "execute"))))

;;
;; windows management
;;
(use-package ace-window)
(use-package windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(pretty-hydra-define hydra-window (:exit-key "q")
  ("Jump"
   (("h" windmove-left "←")
    ("l" windmove-right "→")
    ("k" windmove-up "↑")
    ("j" windmove-down "↓")
    ("a" ace-select-window "ace"))
   "Size"
   (("q" hydra-move-splitter-left "←")
    ("r" hydra-move-splitter-right "→")
    ("e" hydra-move-splitter-up "↑")
    ("w" hydra-move-splitter-down "↓")
    ("_" balance-windows "balance"))
   "Create"
   (("0" delete-window "del")
    ("1" delete-other-windows "max")
    ("2" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) "↦")
    ("3" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) "↧"))
   "Do"
   (("d" delete-window "del")
    ("o" delete-other-windows "max")
    ("u" winner-undo "undo")
    ("R" winner-redo "redo"))
  "Arrange"
  (("s" ace-swap-window "swap")
   ("f" helm-find-files "file")
   ("b" helm-mini "buffer")
   ("." toggle-window-dedicated "dedicate"))
  "Zoom"
   (("+" text-scale-increase "zoom in")
    ("-" text-scale-decrease "zoom out")
    ("=" (lambda () (interactive) (text-scale-set 0)) "reset")
    ("c" nil "cancel")
   )))

(general-def "C-x 9" 'hydra-window/body)

(pretty-hydra-define hydra-olive (:exit-key "q")
  ("Olivetti" (("t" olivetti-mode "toggle")
  ("-" olivetti-shrink "shrink")
  ("=" olivetti-expand "expand")
  ("w" olivett-set-width "width" :quit t))))

(defhydra hydra-navigate (:color red
                          :hint nil)
  "
^→^/^←^               ^↑^/^↓^              ^↑^/^↓^  
_f_/_b_: character    _p_/_n_: line        _j_/_k_: scroll   
_w_/_W_: word         _{_/_}_: paragraph   
_s_/_S_: sentence     _]_/_[_: sexp        
_e_/_a_: end/beg      _<_/_>_: buffer
_m_ (_M_): set mark (jump)                 _q_: quit
"
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("W" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("}" forward-paragraph)
  ("{" backward-paragraph)
  ("m" org-mark-ring-push)
  ("M" org-mark-ring-goto)
  ("j" scroll-up-line)
  ("k" scroll-down-line)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("e" smarter-move-end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("a" smarter-move-beginning-of-line)
  ("q" nil))
