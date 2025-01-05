(use-package mg-org
  :after (org)
  :ensure nil
  :bind (("C-c o c d" . mg-org-compute-deep-work-minutes)
	 ("C-c o b" . mg-org-block-time)
	 ("C-c o m" . mg-org-compile-tex-from-assets)))

(use-package org
  :straight t
  :init
  (require 'mg-bib)
  :bind (("C-c a" . org-agenda)
	 ("C-c C-;" . org-insert-structure-template)
	 ("C-c c" . org-capture)
	 ("C-c C-z" . org-add-note)
	 ("C-c o p" . org-do-promote)
	 ("C-c o d" . org-do-demote)
	 ("C-c p o r" . org-clock-report)
	 ("C-c l" . org-store-link))
  :init
  (require 'mg-bib)
  :custom
  (org-bookmark-names-plist nil)
  (org-src-tab-acts-natively t)
  (org-agenda-files (list mg-work-projects-file mg-personal-projects-file mg-agenda-file mg-inbox-file mg-capture-notes-file))
  (org-archive-location "~/Vault/pkm/.archive/archive.org::* From %s")
  (org-export-backends '(beamer html latex icalendar ascii))
  (org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("L" . "src emacs-lisp")
     ("t" . "src emacs-lisp :tangle FILENAME")
     ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")))
  (org-startup-folded nil)
  (org-log-into-drawer t)
  (org-export-with-drawers nil)
  (org-clock-clocked-in-display 'mode-line)
  (org-clock-idle-time nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "DOING(p@/!)" "HOLD(h)" "|" "DONE(d)")))
  (org-stuck-projects '("+project/" ("NEXT" "TODO") ("course") "\\(Details\\|Artifacts\\|Resources\\)\\>"))
  (org-log-done 'time)
  (org-agenda-hide-tags-regexp ".")
  (org-id-link-to-org-use-id nil)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-clock-sources '(agenda))
  (org-capture-templates
   '(("b" "Bibliography")
     ("bp" "Paper/book" entry (file mg-references-file)
      #'mg-bib-denote-org-capture-paper-biblio
      :kill-buffer t
      :jump-to-captured nil)
     ("bi" "ISBN" entry (file mg-references-file)
      #'mg-bib-denote-org-capture-book-isbn-biblio
      :kill-buffer t
      :jump-to-captured nil)
     ("bw" "Website" entry (file mg-references-file)
      #'mg-bib-denote-org-capture-website-biblio
      :kill-buffer t
      :jump-to-captured nil)
     ("i" "Inbox")
     ("it" "Todo entry" entry (file mg-inbox-file)
	"* TODO %? :inbox:\n:PROPERTIES:\n:CATEGORY: INBOX\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:")
     ("im" "Mail entry" entry (file mg-inbox-file)
	"* TODO Process \"%a\" %? :inbox:\n:PROPERTIES:\n:CATEGORY: INBOX\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:")
     ("in" "Notes entry" entry (file mg-capture-notes-file)
	"* %U (%a) :inbox:\n:PROPERTIES:\n:CATEGORY: INBOX\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:")
     ("a" "Agenda")
     ("am" "Meeting entry" entry (file+headline mg-agenda-file "Future")
	"* Meeting with %? :meeting:\n:PROPERTIES:\n:LOCATION:\n:CATEGORY: %^{Category}\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:\n%^T\n")
     ("ae" "Event entry" entry (file+headline mg-agenda-file "Future")
	"* %? :event:\n:PROPERTIES:\n:LOCATION:\n:CATEGORY:\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:\n%^T\n")
     ("ac" "Call entry" entry (file+headline mg-agenda-file "Future")
	"* Call with %? :call:\n:PROPERTIES:\n:CATEGORY:\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:\n%^T\n")
     ("ap" "Coaching session with Prot entry" entry (file+headline mg-agenda-file "[[denote:20240510T212918][Protesilaos Stavrou]]")
	"* Coaching session :@home:@personal:\n:PROPERTIES:\n:LOCATION: @home\n:CATEGORY: PROT\n:LINK:\n:END:\n:LOGBOOK:\n- Entry inserted on %U \\\\\n:END:\n%^T\n** Topics to discuss\n** Questions from last time\n")
     ("r" "Resources")
     ("ra" "Conference attendance" entry (file mg-conferences-file)
	"* %^{Conference name}\n:PROPERTIES:\n:WHERE: %?\n:WEBSITE: %?\n:END:\n")
     ("rb" "Book archiving" entry (file+headline mg-books-file "Inbox")
	"* %^{Book title}\n:PROPERTIES:\n:TITLE: %^{Book title}\n:AUTHOR: %^{Author}\n:YEAR: %^{Year}\n:PAGES: %^{Pages}\n:RATING: %^{Rating (From * to *****)}\n:LINK: %^{Book link}\n:END:\n")
     ("P" "Planning")
     ("Py" "Year plan" plain (file mg-planning-file)
	"* %^{Year} %U\n- Overview ::\n- Feelings :: %^{Feelings|good|neutral|bad}\n- Milestones ::\n- Values and life philosophy ::\n- 5 years vision(s) ::\n- Financial goals ::\n- [ ] Review ::\n")
     ("Pq" "Quarter plan" plain (file mg-planning-file)
	"** %^{Quarter} %U\n- Overview ::\n- Feelings :: %^{Feelings|good|neutral|bad}\n- Long-term projects ::\n- Financial/expenses planning ::\n- [ ] Review ::\n")
     ("Pm" "Month plan" plain (file mg-planning-file)
	"*** %^{Month} %U\n- Overview ::\n- Feelings :: %^{Feelings|good|neutral|bad}\n- Short-term projects ::\n- [ ] Review ::\n")
     ("f" "Flashcards")
     ("fp" "Physics flaschard" entry (file+headline mg-flashcards-file "Physics") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: Physics\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("fc" "Computer science flashcard" entry (file+headline mg-flashcards-file "Computer science") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: Computer science\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("fk" "Kernel flashcard" entry (file+headline mg-flashcards-file "Kernel") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: Kernel\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("fs" "Security flashcard" entry (file+headline mg-flashcards-file "Security") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: Security\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("fm" "Mathematics flashcard" entry (file+headline mg-flashcards-file "Mathematics") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: Mathematics\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("fe" "English flashcard" entry (file+headline mg-flashcards-file "English") "* %(mg-org-capture-generate-flash-header)\n:PROPERTIES:\n:ANKI_DECK: English\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n")
     ("p" "Projects")
     ("pl" "Learning project" plain (file+headline mg-personal-projects-file "Learning")
	"** %^{Project name} [/]\n:PROPERTIES:\n:WHAT: %?\n:REPOSITORY:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")
     ("ph" "Home project" plain (file+headline mg-personal-projects-file "Home")
	"** %^{Project name} [/]\n:PROPERTIES:\n:WHAT: %?\n:REPOSITORY:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")
     ("pp" "Productivity and tooling project" plain (file+headline mg-personal-projects-file "Productivity and tooling")
	"** %^{Project name} [/]\n:PROPERTIES:\n:WHAT: %?\n:REPOSITORY:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")
     ("pw" "Work project" plain (file+headline mg-work-projects-file "Work")
	"** %? [/]\n:PROPERTIES:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")
     ("pb" "Blogging project" plain (file+headline mg-personal-projects-file "Blogging")
	"** %? [/]\n:PROPERTIES:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")
     ("ps" "Study project" plain (file+headline mg-work-projects-file "Study")
	"** %? [/]\n:PROPERTIES:\n:VISIBILITY: hide\n:COOKIE_DATA: recursive todo\n:END:\n*** Details\n*** Tasks\n*** Resources\n*** Artifacts\n*** Logs\n")))
  (org-refile-targets '((mg-work-projects-file :regexp . "\\(?:\\(?:Log\\|Task\\)s\\)")
			(mg-personal-projects-file :regexp . "\\(?:\\(?:Log\\|Task\\)s\\)")
			(mg-books-file :regexp . "\\(?:\\(?:2023\\|2024\\)s\\)")
			(mg-agenda-file :regexp . "\\(?:Past\\)")))
  (org-agenda-block-separator "==============================================================================")
  (org-agenda-custom-commands
   '(("a" "Agenda"
	((agenda ""
	       ((org-agenda-span 1)
		(org-agenda-skip-function
		 (lambda ()
		   (org-agenda-skip-entry-if 'done)))
		(org-deadline-warning-days 0)
		(org-scheduled-past-days 14)
		(org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		(org-agenda-format-date "%A %-e %B %Y")
		(org-agenda-overriding-header "Today's schedule:\n")))
	 (todo "DOING"
	     ((org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'deadline))
	      (org-agenda-prefix-format "  %i %-12:c [%e] ")
	      (org-agenda-overriding-header "\nDOING Tasks:\n")))
	 (todo "NEXT"
	     ((org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'deadline))
	      (org-agenda-prefix-format "  %i %-12:c [%e] ")
	      (org-agenda-overriding-header "\nNEXT Tasks:\n")))
	 (agenda "" ((org-agenda-time-grid nil)
		   (org-agenda-start-day "+1d")
		   (org-agenda-start-on-weekday nil)
		   (org-agenda-span 30)
		   (org-agenda-show-all-dates nil)
		   (org-deadline-warning-days 0)
		   (org-agenda-entry-types '(:deadline))
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
		   (org-agenda-overriding-header "\nUpcoming deadlines (+30d):\n")))
	 (tags-todo "inbox"
		  ((org-agenda-prefix-format "  %?-12t% s")
		   (org-agenda-overriding-header "\nInbox:\n")))
	 (tags "CLOSED>=\"<today>\""
	     ((org-agenda-overriding-header "\nCompleted today:\n")))
	 (agenda ""
	       ((org-agenda-start-on-weekday nil)
		(org-agenda-skip-function
		 (lambda ()
		   (org-agenda-skip-entry-if 'done)))
		(org-agenda-start-day "+1d")
		(org-agenda-span 5)
		(org-deadline-warning-days 0)
		(org-scheduled-past-days 0)
		(org-agenda-overriding-header "\nWeek at a glance:\n")))
	 ))))
  :config
  (when (display-graphic-p)
    (progn
	(require 'oc-biblatex)
	(setq org-cite-export-processors
	    '((latex biblatex))
	    org-latex-pdf-process mg-latex-cmds)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
	org-format-latex-options (plist-put org-format-latex-options :background "Transparent")
	org-latex-create-formula-image-program 'dvisvgm)
  (require 'ox-latex)
  ;; discard all intermediary files when exporting to latex
  (add-to-list 'org-latex-logfiles-extensions "tex")
  (setq org-latex-remove-logfilest t)
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
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
			       (shell . t)
			       (python .t)
			       (emacs-lisp . t)
			       (org . t)
			       (gnuplot . t)
			       (latex . t)
			       (scheme . t)
			       (lisp . t)
			       (haskell . t)
			       (R . t))))

;; Enable and set org-crypt
(use-package org-crypt
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote (crypt)))
  ;; GPG key to use for encryption
  (setq org-crypt-key nil))

(use-package mg-pkm-utils
  :ensure nil
  :bind (("C-c p c d" . mg-org-compute-deep-work-minutes))
  ("C-c p p" . mg-toggle-pdf-presentation-mode))

(use-package org-transclusion
  :after org
  :straight t)

(provide 'mg-emacs-org)
