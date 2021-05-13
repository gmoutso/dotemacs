(require 'ox)
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))

(add-to-list 'org-latex-classes
             '("evalue"
"\\documentclass[10pt]{article}
\\usepackage{amsthm}
\\usepackage{brex-base}
\\usepackage{brex-xtras}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{color}
\\usepackage{listings}
\\definecolor{mygreen}{rgb}{0,0.6,0}
\\definecolor{mygray}{rgb}{0.5,0.5,0.5}
\\definecolor{mymauve}{rgb}{0.58,0,0.82}
\\lstdefinestyle{customc}{
  backgroundcolor=\\color{white},
  basicstyle=\\footnotesize,        % the size of the fonts that are used for the code
  commentstyle=\\color{mygreen},    % comment style
  keywordstyle=\\color{blue},       % keyword style
  numberstyle=\tiny\\color{mygray}, % the style that is used for the line-numbers
  stringstyle=\\color{mymauve},     % string literal style
}
\\lstset{style=customc}
\\usepackage{attachfile2}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
  ;; ("\\part{%s}" . "\\part*{%s}")
  ;; ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(setq org-latex-title-command "\\maketitle\\thispagestyle{report}

")
(add-to-list 'org-latex-classes
             '("screen"
"\\documentclass[12pt]{article}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage[screen,centering]{geometry}
\\usepackage{hyperref}
[NO-DEFAULT-PACKAGES]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-default-class "evalue")
(setq org-latex-compiler "xelatex")

;; export to html - also applies to org-mime-org-mode-htmlize
(setq org-html-with-latex t)
;; exports images -- messes with mime?
(defun my-replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
(defun my-org-html--format-image (source attributes info)
  (progn
    (setq source (my-replace-in-string "%20" " " source))
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
            (or (file-name-extension source) "")
            (base64-encode-string
             (with-temp-buffer
               (insert-file-contents-literally source)
              (buffer-string)))
            (file-name-nondirectory source))))
(defun my-org-html-export-with-inline ()
  (interactive)
  (cl-flet ((org-html--format-image (source attributes info) (my-org-html--format-image source attributes info)))
    (org-html-export-to-html)))

