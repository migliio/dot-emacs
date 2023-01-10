(define-skeleton place-skeleton
  "Metadata for places to visit" nil
  ":PROPERTIES:
:LINK:
:VISITED:
:COMMENT:
:END:")

(define-skeleton challenge-skeleton
  "Headings for hacking challenges" nil
  "**** Commands\n**** Walkthrough\n**** Resources")

(define-skeleton equation-skeleton
  "Equation environment for LaTeX" nil
  "\\begin{equation}

\\end{equation}")

(define-skeleton latex-image-skeleton
  "Image environment for LaTeX documents" nil
  "#+BEGIN_CENTER
#+ATTR_LATEX: :height 0.5\\textwidth
#+CAPTION:

#+END_CENTER")

(provide 'skeletons-setup)
