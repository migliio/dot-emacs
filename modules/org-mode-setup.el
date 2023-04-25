(setq org-export-backends '(beamer html latex ascii ox-reveal ox-hugo icalendar))

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
	 ("C-c t" . org-insert-structure-template)
	 ("C-c i" . org-capture)
	 ("C-c l" . org-store-link))
  :config
  (require 'org-tempo)
  ;; Set org agenda directory
  (setq org-agenda-files '("~/Vault/pkm/pages/20220919103543-personal_agenda.org" "~/Vault/pkm/pages/20221122175451-personal_journal.org"))
  ;; Org-capture templates
  (setq org-capture-templates
	'(("j" "journal")
	  ("jp" "journal plain entry" plain
           (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
           "**** %U: %?\n")
	  ("ja" "journal archive resource entry" plain
           (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
           "**** %U: %? :archive:\n")
	  ("jm" "journal meeting entry" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** Meeting with %? on %U :schedule:meeting:work:\n:PROPERTIES:\n:WHERE:\n:NOTIFY_BEFORE:\n:END:\nSCHEDULED: %T\n***** Notes")
	  ("js" "journal seminar entry" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** Seminar hold by %? @<place> :schedule:work:\n:PROPERTIES:\n:NOTIFY_BEFORE:\n:END:\nSCHEDULED: %T\n***** Notes")
	  ("jc" "journal call entry" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** Call with %? @online on %U :schedule:\n:PROPERTIES:\n:NOTIFY_BEFORE:\n:END:\nSCHEDULED: %T\n***** Notes")
	  ("jh" "journal home chores entry" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** %? @ home :schedule:personal:\n:PROPERTIES:\n:NOTIFY_BEFORE:\n:END:\nSCHEDULED: %T\n")
	  ("P" "personal")
	  ("Pc" "contact" plain
	   (file "~/Vault/pkm/pages/20230216124800-personal_contacts.org")
	   "* %(org-contacts-template-name) %^g\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:NOTES: %^{NOTES}\n:PKM_LINK: %?\n:END:")
	  ("p" "plans")
	  ("py" "year" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** %U: Year plan :yearly:plan:\n- *Feelings*:: %^{Feelings|good|neutral|bad}\n- *Related*:: %?\n- *Date*:: %^{Date}u\n- *Keywords*::\n***** Overview\n***** Values review and life physolophy\n***** 5 Years Vision(s)\n***** Goal definition\n***** Financial review\n***** Time tracking review")
	  ("pq" "quarter" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** %U: Quarter plan :quarterly:plan:\n- *Feelings*:: %^{Feelings|good|neutral|bad}\n- *Related*:: %?\n- *Date*:: %^{Date}u\n- *Keywords*::\n***** Overview\n***** Projects review\n***** Financial review\n***** Time tracking review")
	  ("pm" "month" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** %U: Month plan :monthly:plan:\n- *Feelings*:: %^{Feelings|good|neutral|bad}\n- *Related*:: %?\n- *Date*:: %^{Date}u\n- *Keywords*::\n***** Overview\n***** Projects and task picking\n***** Financial review\n***** Time tracking review")
	  ("pw" "week" plain
	   (file+datetree+prompt "~/Vault/pkm/pages/20221122175451-personal_journal.org")
	   "**** %U: Week plan :weekly:plan:\n- *Feelings*:: %^{Feelings|good|neutral|bad}\n- *Related*:: %?\n- *Date*:: %^{Date}u\n- *Keywords*::\n***** Overview\n***** Time blocking\n***** Task picking")))
  
  ;; Export citations
  (setq org-cite-global-bibliography
	'("/home/claudio/Vault/library/org/main/main.bib"))
  (require 'oc-biblatex)
  (setq org-cite-export-processors
	'((latex biblatex)))
  (setq org-latex-pdf-process (list
			       "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

  
  ;; In org-mode, I want source blocks to be themed as they would in native mode
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil
	org-edit-src-content-indentation 0)

  ;; Set latex preview size
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Fold everything when opening org files
  (setq org-startup-folded t)

  ;; Not export drawers
  (setq org-export-with-drawers nil)

  ;; Set org-mode TODO keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)" "CANCELLED(c)" "INTR(i)")
	  (sequence "INBOX" "|" "ARCHIVED")))

  ;; Org-agenda custom commands
  (setq org-agenda-custom-commands
	'(
	  ("a" "Agenda"
	   ((agenda ""
		    ((org-agenda-span 1)
		     (org-deadline-warning-days 0)
		     (org-scheduled-past-days 14)
		     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
		     (org-agenda-format-date "%A %-e %B %Y")
		     (org-agenda-overriding-header "Today's schedule:\n")))
	    (todo "PROG"
		    ((org-agenda-time-grid nil)
		     (org-agenda-span 1)
		     (org-deadline-warning-days 0)
		     (org-scheduled-past-days 0)
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
		     (org-agenda-overriding-header "PROG tasks:\n")))
	   (todo "NEXT"
		     ((org-agenda-time-grid nil)
		     (org-agenda-span 1)
		     (org-deadline-warning-days 0)
		     (org-scheduled-past-days 0)
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
		     (org-agenda-overriding-header "NEXT tasks:\n")))
            (agenda "" ((org-agenda-time-grid nil)
			(org-agenda-start-day "+1d")
			(org-agenda-start-on-weekday nil)
			(org-agenda-span 30)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-entry-types '(:deadline))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
			(org-agenda-overriding-header "\nUpcoming deadlines (+30d)\n")))
	    (agenda ""
		    ((org-agenda-start-on-weekday nil)
		     (org-agenda-start-day "+1d")
		     (org-agenda-span 5)
		     (org-deadline-warning-days 0)
		     (org-scheduled-past-days 0)
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                     (org-agenda-overriding-header "\nWeek at a glance:\n")))
	    ))))

  ;; Enable DONE logging in org-mode
  (setq org-log-done 'time)
  
  ;; View LaTeX previews in better quality
  (setq org-latex-create-formula-image-program 'dvisvgm)

  ;; org-export-latex
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
	       '("res"
		 "\\documentclass[margin]{res}\n
\\setlength{\textwidth}{5.1in}"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("memoir"
		  "\\documentclass[article]{memoir}\n
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
  	       '("letter"
		  "\\documentclass{letter}\n"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes	       
	       '("tuftebook"
		 "\\documentclass{tufte-book}\n
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("tuftehandout"
		 "\\documentclass{tufte-handout}
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("tufnotes"
		 "\\documentclass{tufte-handout}
				   \\usepackage{xcolor}
					 \\usepackage{graphicx} %% allow embedded images
					 \\setkeys{Gin}{width=\\linewidth,totalheight=\\textheight,keepaspectratio}
					 \\usepackage{amsmath}  %% extended mathematics
					 \\usepackage{booktabs} %% book-quality tables
					 \\usepackage{units}    %% non-stacked fractions and better unit spacing
					 \\usepackage{multicol} %% multiple column layout facilities
					 \\RequirePackage[many]{tcolorbox}
					 \\usepackage{fancyvrb} %% extended verbatim environments
					   \\fvset{fontsize=\\normalsize}%% default font size for fancy-verbatim environments

			  \\definecolor{g1}{HTML}{077358}
			  \\definecolor{g2}{HTML}{00b096}

			  %%section format
			  \\titleformat{\\section}
			  {\\normalfont\\Large\\itshape\\color{g1}}%% format applied to label+text
			  {\\llap{\\colorbox{g1}{\\parbox{1.5cm}{\\hfill\\color{white}\\thesection}}}}%% label
			  {1em}%% horizontal separation between label and title body
			  {}%% before the title body
			  []%% after the title body

			  %% subsection format
			  \\titleformat{\\subsection}%%
			  {\\normalfont\\large\\itshape\\color{g2}}%% format applied to label+text
			  {\\llap{\\colorbox{g2}{\\parbox{1.5cm}{\\hfill\\color{white}\\thesubsection}}}}%% label
			  {1em}%% horizontal separation between label and title body
			  {}%% before the title body
			  []%% after the title body

							\\newtheorem{note}{Note}[section]

							\\tcolorboxenvironment{note}{
							 boxrule=0pt,
							 boxsep=2pt,
							 colback={green!10},
							 enhanced jigsaw, 
							 borderline west={2pt}{0pt}{Green},
							 sharp corners,
							 before skip=10pt,
							 after skip=10pt,
							 breakable,
						  }"

		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Set up org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
			       (shell . t)
			       (python .t)
			       (emacs-lisp . t)
			       (org . t)
			       (latex . t)
			       (ditaa . t)
			       (scheme . t)
			       (lisp . t)
			       (haskell . t)
			       (R . t))))

(use-package org-wild-notifier
  :ensure t
  :config
  (setq org-wild-notifier-notification-title "Org agenda reminder"
	org-wild-notifier-alert-times-property "NOTIFY_BEFORE"))

(use-package org-contacts
  :ensure t
  :after org
  :custom (org-contacts-files '("~/Vault/pkm/pages/20230216124800-personal_contacts.org")))

(use-package ox-hugo
  :ensure t
  :after ox)

(provide 'org-mode-setup)
