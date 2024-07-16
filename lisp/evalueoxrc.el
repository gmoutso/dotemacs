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

(dolist (item '(
	 ("" "minted")
	 ("" "listings")
	 ("dvipsnames" "xcolor")
	 ("" "amsmath")
	 ("" "amsthm")
	 ("" "amssymb")
	 ("" "graphicx")
	 ("" "longtable")
	 ("" "booktabs")
	 ("" "hyperref")
	 ("color={0 0 0}" "attachfile2")
	 ;; xelatex+caption+attachfile2 issue https://github.com/ho-tex/attachfile2/issues/8
	 ("singlelinecheck=false" "caption" nil ("xelatex"))
	 ("" "pdflscape")
	 ))
    (add-to-list 'org-latex-packages-alist item))

(add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
(setq org-latex-listings-options '(
				   ("backgroundcolor" "\\color{white}")
				   ("basicstyle" "\\footnotesize")
				   ("commentstyle" "\\color{green}")
				   ("keywordstyle" "\\color{blue}")
				   ("numberstyle" "\\tiny\\color{gray}")
				   ("stringstyle" "\\color{purple}")
				   ))
;; minted needs -shell-escape. Below is for tex-mode
(setq TeX-command-extra-options "-shell-escape")

(add-to-list 'org-latex-classes
             '("evalue"
	       "\\documentclass[10pt]{article}
\\usepackage{amsmath}
\\usepackage{spring21-light}
\\AddToHook{cmd/maketitle/after}{\\thispagestyle{report}}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{marginnote}
"
("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

(add-to-list 'org-latex-classes
             '("halfscreen"
	       "\\documentclass[10pt]{article}
\\usepackage[margin=2cm,papersize={180mm,200mm}]{geometry}
\\usepackage{fancyhdr}
\\pagestyle{fancy}\\fancyhead{\\nouppercase{\\rightmark\\hfill\\leftmark}}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{marginnote}
\\usepackage{placeins} % provides FloatBarrier
\\usepackage{etoolbox} % provides etoolbox
\\usepackage{graphicx}
\\setkeys{Gin}{width=0.7\\textwidth}
"
("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))


(add-to-list 'org-latex-classes
             '("plain"
"\\documentclass[10pt]{article}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{marginnote}
\\usepackage{placeins} % provides FloatBarrier
\\usepackage{etoolbox} % provides etoolbox
"
("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

(add-to-list 'org-latex-classes
             '("screen"
"\\documentclass[12pt]{article}
\\usepackage[screen]{geometry}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{placeins}
[NO-DEFAULT-PACKAGES]"
("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))

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


