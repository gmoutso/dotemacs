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
    ("b" helm-filtered-bookmarks "bookmarks")
    ("t" treemacs "treemacs"))
   "Org"
   (
   ("od" helm-org-rifle-directories "rifle dir")
   ("oo" helm-org-rifle-org-directory "rifle org/")
   ("om" (hera-push 'hydra-files-org-more/body) "more..")
   ;; ("ob" helm-org-rifle "buffers")
   ;; ("of" helm-org-rifle-files "files")
   ;; ("oa" helm-org-rifle-agenda-files "agenda")
   )
   "Shells"
   (("e" eshell "eshell")
    ("v" gm/jump-to-vterm "vterm"))))

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
(defun gm/org-display-inline-images () (interactive)
       (org-display-inline-images nil t))

(major-mode-hydra-define jupyter-repl-mode
  (:exit t :quit-key ("q" "<escape>"))
  ("Kernel"
  (("g" jupyter-repl-restart-kernel "restart")
   ("r" org-recipes "recipes"))
  ))

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
    ("lf" org-footnote-new "footnote")
    )
   "Visibility"
   (("tl" org-toggle-link-display "toggle links")
    ("ti" org-toggle-inline-images "toggle images")
    ("ir" gm/org-display-inline-images "images refresh")
    ("ts" org-fold-hide-block-all "hide blocks")
    ("is" gm/org-resize-images "images temp resize")
    ("iS" gm/org-set-size-images "set images size")
    )
   "Jump"
   (("jh" helm-org-in-buffer-headings "headings")
    ("jr" helm-org-rifle-current-buffer "rifle")
    ("jo" helm-occur "occur")
    ("ji" gm/org-show-image-files "images")
     ;("ne" hydra-navigate "navigate emacs")
    )
   "Ox"
   ( ;("nb" org-narrow-to-block "narrow block")
    ; ("nw" widen "widen")
    ("v" org-open-pdf "view pdf")
    ("e" org-latex-export-to-pdf "export pdf")
    ("c" gm/org-result-decorate "caption"))
   "Jupyter"
   (("k" gm/jupyter-kernels "kernels")
    ("." gm/org-find-definition-at-point "org def")
    ("w" gm/jupyter-whos "whos")
    ("r" org-recipes "recipes")
    )
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


;; Hydra for org agenda (graciously taken from Spacemacs)
(major-mode-hydra-define org-agenda-mode
  (:exit nil :quit-key ("q" "<escape>"))
  ("Headline"
  (("hA" org-agenda-archive-default "archive")
  ("hk" org-agenda-kill "kill")
  ("hp" org-agenda-priority "priority")
  ("hr" org-agenda-refile "refile")
  ("h:" org-agenda-set-tags "tags")
  ("ht" org-agenda-todo "todo"))
  "Visit"
  (("o"   link-hint-open-link "open" :exit t)
  ("TAB" org-agenda-goto "goto" :exit t)
  ("SPC" org-agenda-show-and-scroll-up "show")
  ("RET" org-agenda-switch-to "switch" :exit t))
  "Date"
  (("dt" org-agenda-date-prompt "timestamp")
  ("dd" org-agenda-deadline "deadline")
  ("+" org-agenda-do-date-later "later")
  ("-" org-agenda-do-date-earlier "earlier")
  ("ds" org-agenda-schedule "schedule"))
  "View"
  (("vd" org-agenda-day-view "day")
  ("vw" org-agenda-week-view "week")
  ("vt" org-agenda-fortnight-view "fortnight")
  ("vm" org-agenda-month-view "month")
  ("vy" org-agenda-year-view "year")
  ("vn" org-agenda-later "later")
  ("vp" org-agenda-earlier "earlier")
  ("vr" org-agenda-reset-view "reset"))
  "Toggle"
  (("ta" org-agenda-archives-mode "archive")
  ("tA" (org-agenda-archives-mode 'files) "archive files")
  ("tr" org-agenda-clockreport-mode "report")
  ("tf" org-agenda-follow-mode "follow")
  ("tl" org-agenda-log-mode "log")
  ("td" org-agenda-toggle-diary "diary"))
  "Filter"
  (("fc" org-agenda-filter-by-category "category")
  ("fx" org-agenda-filter-by-regexp "reg")
  ("ft" org-agenda-filter-by-tag "tag")
  ("fr" org-agenda-filter-by-tag-refine "refine")
  ("fh" org-agenda-filter-by-top-headline "headline")
  ("fd" org-agenda-filter-remove-all "reset"))
  "Other"
  (("gd" org-agenda-goto-date "goto date")
  ("." org-agenda-goto-today "goto today")
  ("gr" org-agenda-redo "redo"))))

(major-mode-hydra-define python-mode (:exit t :quit-key ("q" "c" "<escape>"))
  ("Jupyter"
   (("k" gm/jupyter-kernels "kernels")
    ("w" gm/jupyter-whos "whos")
    ("P" gm/run-python "python"))
   "Buffer"
   (("n" (hera-push 'hydra-navigate-python/body) "navigate" :exit t)
    ("i" helm-imenu "imenu")
    ("I" gm/message-pydef "info defun")
    )
   "Code"
   (("f" flycheck-list-errors "flycheck")
    ("g" gtags-find-tag "gtags")
    ("e" conda-env-activate "activate env")
    ("r" org-recipes "recipes")
    )
   "LSP"
   (("ll" lsp "LSP")
    ("ld" lsp-ui-doc-glance "document")
    ("ls" lsp-signature-activate "signature")
    ("s" lsp-treemacs-symbols "symbols"))
   ))

(major-mode-hydra-define python-ts-mode (:exit t :quit-key ("q" "c" "<escape>"))
  ("Jupyter"
   (("k" gm/jupyter-kernels "kernels")
    ("w" gm/jupyter-whos "whos")
    ("P" gm/run-python "python"))
   "Buffer"
   (("n" (hera-push 'hydra-navigate-python/body) "navigate" :exit t)
    ("i" helm-imenu "imenu")
    ("I" gm/message-pydef "info defun")
    )
   "Code"
   (("f" flycheck-list-errors "flycheck")
    ("g" gtags-find-tag "gtags")
    ("e" conda-env-activate "activate env")
    ("r" org-recipes "recipes")
    )
   "LSP"
   (("ll" lsp "LSP")
    ("ld" lsp-ui-doc-glance "document")
    ("ls" lsp-signature-activate "signature")
    ("s" lsp-treemacs-symbols "symbols"))
   ))

;; python-nav-forward-block "forward block")
;; ("nba" python-nav-backward-block "backward block")
;; ("nfe" python-nav-backward-defun "backward defun")
;; ("nu"  python-nav-backward-up-list "up list")

(defhydra hydra-navigate-python-full (:color red :hint nil :exit nil)
  "
^⬎^/^⬑^              ^←^/^→^               ^↑^/^↓^              ^↑^/^↓^  
_→_/_←_: character    _S_/_s_: statement    _(_/_)_: statement   _j_/_k_: scroll   
_↑_/_↓_: line         _F_/_f_: ❌ defun      _{_/_}_: defun       _N_/_n_: narrow
_e_/_a_: line         _B_/_b_: block        _[_/_]_: block       _<_/_>_: buffer
_q_/_Q_: quit (back)                                             _u_/_U_: list
_m_/_M_: mark (jump)
"
  ("<right>" forward-char)
  ("<left>" backward-char)
  ;; ("w" forward-word)
  ;; ("W" backward-word)
  ("<down>" next-line)
  ("<up>" previous-line)
  ("a" smarter-move-beginning-of-line)
  ("e" smarter-move-end-of-line)
  ; statement
  ("S" python-nav-beginning-of-statement)
  ("s" python-nav-end-of-statement)
  (")" python-nav-forward-statement)
  ("(" python-nav-backward-statement)
  ; defun
  ("{" python-nav-backward-defun)
  ("}" python-nav-forward-defun)
  ("F" python-nav-beginning-of-defun)
  ("f" python-nav-end-of-defun)
  ; block
  ("b" python-nav-end-of-block)
  ("B" python-nav-beginning-of-block)
  ("[" python-nav-backward-block)
  ("]" python-nav-forward-block)
  ; list
  ("u" python-nav-backward-up-list)
  ("U" python-nav-up-list)
  ; other
  ("n" narrow-to-defun)
  ("N" widen)
  ("m" org-mark-ring-push)
  ("M" org-mark-ring-goto)
  ("j" scroll-up-line)
  ("k" scroll-down-line)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("q" nil :exit t)
  ("Q" (hera-pop) :exit t))

(defun hydra-backward-symbol ()
  (interactive)
  (forward-symbol -1))
(defun hydra-interactive-python-nav-beginning-of-defun ()
  (interactive)
  (python-nav-beginning-of-defun))
(defhydra hydra-navigate-python (:exit nil :color red :hint nil)
  "
^⬎^/^⬑^               ^↑^/^↓^               ^↑^/^↓^  
_)_/_(_: statement     _↑_/_↓_: statement    _j_/_k_: scroll   
_f_/_F_: defun         _{_/_}_: defun        _w_/_n_: narrow
_u_/_e_: up (end)      _[_/_]_: block        _<_/_>_: buffer
_→_/_←_: symbol        _m_/_M_: mark (jump)  _q_/_Q_: quit (back)
"
  ("<right>" forward-symbol)
  ("<left>" hydra-backward-symbol)
  ; statement
  ("(" python-nav-beginning-of-statement)
  (")" python-nav-end-of-statement)
  ("<down>" python-nav-forward-statement)
  ("<up>"   python-nav-backward-statement)
  ; defun
  ("{" python-nav-backward-defun)
  ("}" python-nav-forward-defun)
  ("F" hydra-interactive-python-nav-beginning-of-defun)
  ("f" python-nav-end-of-defun)
  ; block
  ("e" python-nav-end-of-block)
  ("[" python-nav-backward-block)
  ("]" python-nav-forward-block)
  ; list
  ("u" python-nav-backward-up-list)
  ; other
  ("n" narrow-to-defun)
  ("w" widen)
  ("m" org-mark-ring-push)
  ("M" org-mark-ring-goto)
  ("j" scroll-up-line)
  ("k" scroll-down-line)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("q" nil :exit t)
  ("Q" (hera-pop) :exit t)
  ("C-<down>" forward-paragraph)
  ("C-<up>" backward-paragraph)
  ("<SPC>" View-scroll-half-page-forward)
  ("S-<SPC>" View-scroll-half-page-backward))
