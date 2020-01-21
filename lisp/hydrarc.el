(require 'hydra)
(require 'general)
(require 'pretty-hydra)
(use-package major-mode-hydra)
(general-def 'override
  "C-c C-m" 'major-mode-hydra)

(pretty-hydra-define hydra-files (:title "Do things with Files" :exit t :quit-key "q")
  ("Visit Files"
   (("f" helm-find-files "files")
    ("/" helm-find "find name")
    ("s" helm-do-grep-ag "ag")
    ("b" helm-mini "buffers")
    ("l" helm-locate "locate")
    )
   "Dired"
   (("d" dired "dired")
    ("g" find-grep-dired "grep")
    ("F" find-dired "find")
    )
   "Places"
   (("p" helm-projectile "projects") 
    ("B" helm-filtered-bookmarks "bookmarks")
    ("t" treemacs "treemacs" :exit nil))
   "Org Rifle"
   (;("rc" helm-org-rifle-current-buffer "current")
   ("ob" helm-org-rifle "buffers")
   ("od" helm-org-rifle-directories "directories")
   ("oo" helm-org-rifle-org-directory "org-directory")
   ("of" helm-org-rifle-files "files")
   ("oa" helm-org-rifle-agenda-files "agenda")
   )
   )
  )

(general-def 'override
  "C-c C-f" 'hydra-files/body)

(pretty-hydra-define hydra-agenda
  (:exit nil :quit-key ("q" "<escape>"))
   ("Scheduling"
   (("s" org-schedule     "org-schedule")
    ("d" org-deadline     "deadline"))
   "Todo"
   (("T" org-todo       "menu")
    ("H" org-shiftleft  "left")
    ("J" org-shiftdown  "down")
    ("K" org-shiftup    "up")
    ("L" org-shiftright "right")))
)

(major-mode-hydra-define org-mode
  (:exit t :quit-key ("q" "<escape>"))
  (
  ;; "Special"
  ;;  (("," org-ctrl-c-ctrl-c "ctrl-c-ctrl-c")
  ;;   ("*" org-ctrl-c-star   "ctrl-c-star")
  ;;   ("-" org-ctrl-c-minus  "ctrl-c-minus")
  ;;   ("." org-priority      "set priority")
  ;;   ("^" org-sort          "sort"))
   "Copy/paste"
   (("ch" nil "copy to html")
    ("yh" org-to-html-from-clip "past from html")
    ("yd" org-paste-df "paste dataframe")
    ("yl" org-paste-link-xclip "past as link")
    )
   "Hyper"
   (("l" org-insert-link  "insert link")
    ("f" org-footnote-new "footnote")
    ("o" org-open-at-point  "open link")
    )
   "Jump"
   (("r" helm-org-rifle-current-buffer "rifle heading")
    ("h" helm-org-in-buffer-headings "headings")
    ("s" helm-occur "occur search")
    ("ne" hydra-navigate "navigate emacs"))
   "Ox"
   (("nb" org-narrow-to-block "narrow block")
    ("nw" widen "widen")
    ("P" org-open-pdf "view pdf")
    ("e" org-latex-export-to-pdf "export pdf"))
   ;; (("N" org-next-block     "next block" :exit nil)
   ;;  ("P" org-previous-block "previous block" :exit nil)
   ;;  ("n" org-next-link      "next link" :exit nil)
   ;;  ("p" org-previous-link  "previous link" :exit nil)
   ;;  ("o" org-open-at-point  "open link" :exit nil))
   ))

(defhydra hydra-rifle (:exit t)
   ("c" helm-org-rifle-current-buffer "current")
   ("b" helm-org-rifle "buffers")
   ("d" helm-org-rifle-directories "directories")
   ("o" helm-org-rifle-org-directory "org-directory")
   ("f" helm-org-rifle-files "files")
   ("a" helm-org-rifle-agenda-files "agenda")
   ("q" nil "cancel")
   )

(pretty-hydra-define hydra-projectile (:exit t)
  ("Open"
   (("f" helm-projectile-find-file "file")
    ("r" helm-projectile-recent "recent")
    ("p" helm-projectile-switch-project "project")
    ("d" helm-projectile-find-dir "directory"))
   "Search"
   (("o" projectile-multi-occur "occur")
    ("a" projectile-ag))
   "Buffers"
   (("b" helm-projectile-switch-to-buffer "switch")
    ("k" helm-projectile-kill-buffers "kill"))
   "Cache"
   (("C" projectile-invalidate-cache "clear")
    ("x" projectile-remove-known-project "remove project")
    ("X" projectile-cleanup-known-projects "cleanup"))))

(pretty-hydra-define hydra-registers (:exit t)
  ("Point"
   (("r" point-to-register "save point")
    ("j" jump-to-register "jump")
    ("v" view-register "view all"))
   "Text"
   (("c" copy-to-register "copy region")
    ("C" copy-rectangle-to-register "copy rect")
    ("i" insert-register "insert")
    ("p" prepend-to-register "prepend")
    ("a" append-to-register "append"))
   "Macros"
   (("m" kmacro-to-register "store")
    ("e" jump-to-register "execute"))))

(use-package ace-window)
(pretty-hydra-define hydra-window (:exit-key "q")
  ("Jump"
   (("h" windmove-left "left")
    ("l" windmove-right "right")
    ("k" windmove-up "up")
    ("j" windmove-down "down")
    ("a" ace-select-window "ace"))
   "Split"
   (("q" split-window-right "left")
    ("r" (progn (split-window-right) (call-interactively 'other-window)) "right")
    ("e" split-window-below "up")
    ("w" (progn (split-window-below) (call-interactively 'other-window)) "down"))
   "Do"
   (("d" delete-window "delete")
    ("o" delete-other-windows "delete others")
    ("u" winner-undo "undo")
    ("R" winner-redo "redo"))))

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
