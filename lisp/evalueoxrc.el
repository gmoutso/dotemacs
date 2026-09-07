;;; evalueoxrc.el --- Evalueox configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom configuration file.

;;; Code:

(require 'ox)
(use-package ox-latex
  :custom
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
			   "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
			   "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-default-class "evalue")
  (org-latex-compiler "xelatex")
  )
(setq org-export-async-init-file
      (expand-file-name "~/.emacs.d/lisp/evalueoxrc.el"))
;; use minted for org source blocks
(add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
;; (setq org-latex-src-block-backend 'minted)
(setq org-latex-src-block-backend 'listings)

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
	 ("singlelinecheck=false" "caption" nil ("xelatex"))
	 ("" "pdflscape")
	 ))
    (add-to-list 'org-latex-packages-alist item))

(add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
(setq org-latex-listings-options '(
				   ("breaklines" "true")
				   ("prebreak" "\\textbackslash")
				   ("breakatwhitespace" "true")
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

;;
;; pandoc exporter
;;
(use-package ox-pandoc
  :custom
  (org-pandoc-options '((standalone . t)
                        (embed-resources . t)
                        (citeproc . t)))
  (org-pandoc-options-for-latex '((pdf-engine . "xelatex")
                                  (template . "evalue")
                                  (citeproc . t)))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")
                                      (template . "evalue")
                                      (citeproc . t)))
  (org-pandoc-options-for-html5 '((standalone . t)
				  (embed-resources . t)
				  (template . "easy-ev.html")
				  (css . "easy-ev.css")
				  (resource-path . "/home/moutsopoulosg/.local/share/pandoc/css")
				  (toc . t)
				  (toc-depth . 2)
				  (highlight-style . "tango")))
  ;; simplify menu
  (org-pandoc-menu-entry
  '((?h "to html5 and open." org-pandoc-export-to-html5-and-open)
    (?H "as html5." org-pandoc-export-as-html5)
    (?p "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
    (?P "to latex-pdf." org-pandoc-export-to-latex-pdf)
    (?o "to odt and open." org-pandoc-export-to-odt-and-open)
    (?O "to odt." org-pandoc-export-to-odt)
    (?, "as typst." org-pandoc-export-as-typst)
    (?X "to docx." org-pandoc-export-to-docx))
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
(defun gm/org-html-export-to-html-inline-images ()
  (interactive)
  (cl-flet ((org-html--format-image (source attributes info) (my-org-html--format-image source attributes info)))
    (org-html-export-to-html)))

(provide 'evalueoxrc)
;;; evalueoxrc.el ends here
