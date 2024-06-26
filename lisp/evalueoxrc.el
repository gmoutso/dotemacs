(require 'ox)

(add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
(use-package ox-latex
  :custom
  ;; minted needs -shell-escape
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
			   "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
			   "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-default-class "evalue")
  (org-latex-compiler "xelatex")
  )
;; use minted for org source blocks
(setq org-latex-listings 'minted)
(setq org-latex-listings 'listings)

(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
;; minted needs -shell-escape. Below is for tex-mode
(setq TeX-command-extra-options "-shell-escape")


(add-to-list 'org-latex-classes
             '("evalue"
"\\documentclass[10pt]{article}
\\usepackage{amsmath}
\\usepackage{spring21-light}
\\usepackage{amsthm}
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
  numberstyle=\\tiny\\color{mygray}, % the style that is used for the line-numbers
  stringstyle=\\color{mymauve},     % string literal style
}
\\lstset{style=customc}
\\usepackage{marginnote}
\\usepackage[color={0 0 0}]{attachfile2}
% xelatex+caption+attachfile2 issue https://github.com/ho-tex/attachfile2/issues/8
\\usepackage[singlelinecheck=false]{caption}
\\usepackage{placeins}
\\newcommand\\alwaysFloatBarrier{%
   \\pretocmd{\\section}{\\FloatBarrier}{}{}%
   \\pretocmd{\\subsection}{\\FloatBarrier}{}{}%
   \\pretocmd{\\subsubsection}{\\FloatBarrier}{}{}%
}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
  ;; ("\\part{%s}" . "\\part*{%s}")
  ;; ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("plain"
"\\documentclass[10pt]{article}
\\usepackage{amsmath}
\\usepackage{amsthm}
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
  numberstyle=\\tiny\\color{mygray}, % the style that is used for the line-numbers
  stringstyle=\\color{mymauve},     % string literal style
}
\\lstset{style=customc}
\\usepackage{marginnote}
\\usepackage[color={0 0 0}]{attachfile2}
% xelatex+caption+attachfile2 issue https://github.com/ho-tex/attachfile2/issues/8
\\usepackage[singlelinecheck=false]{caption}
\\usepackage{placeins}
\\newcommand\\alwaysFloatBarrier{%
   \\pretocmd{\\section}{\\FloatBarrier}{}{}%
   \\pretocmd{\\subsection}{\\FloatBarrier}{}{}%
   \\pretocmd{\\subsubsection}{\\FloatBarrier}{}{}%
}
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
\\usepackage[screen]{geometry}
\\usepackage{hyperref}
\\usepackage{placeins}
[NO-DEFAULT-PACKAGES]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


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

