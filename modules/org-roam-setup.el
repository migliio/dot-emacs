(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Vault/pkm/slip-box/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n t" . org-roam-tag-add)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture))
  :config
  (add-hook 'after-save-hook
	    (defun org-rename-to-new-title ()
	      (when-let*
		  ((old-file (buffer-file-name))
		   (is-roam-file (org-roam-file-p old-file))
		   (file-node (save-excursion
				(goto-char 1)
				(org-roam-node-at-point)))
		   (file-name  (file-name-base (org-roam-node-file file-node)))
		   (file-time  (or (and (string-match "^\\([0-9]\\{14\\}\\)-" file-name)
					(concat (match-string 1 file-name) "-"))
				   ""))
		   (slug (org-roam-node-slug file-node))
		   (new-file (expand-file-name (concat file-time slug ".org")))
		   (different-name? (not (string-equal old-file new-file))))

		(rename-buffer new-file)
		(rename-file old-file new-file)
		(set-visited-file-name new-file)
		(set-buffer-modified-p nil))))  
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-id-extra-files (directory-files-recursively "~/Vault/pkm/pages" "org"))
  ;; org-roam templates
  (setq org-roam-capture-templates
	'(("d" "default" plain "\n:ORG_META:\n- *Date*::%?\n- *Resources*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unnarrowed t)
	  ("u" "university")
	  ("uc" "course" plain	"\n:ORG_META:\n- *Lecturer*:: %?\n- *University*:: \n- *Academic Year*:: %^{Academic Year}\n- *Semester*:: %^{Semester}\n- *Resources*:: \n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("ul" "lecture" plain
	   "\n:ORG_META:\n- *Topics*::\n- *Lecturer*::\n- *Date*:: %^{Date}u\n- *Resources*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("p" "personal")
	  ("pp" "people" plain
	   "\n:ORG_META:\n- *Phone number*:: %?\n- *E-mail*::\n- *Twitter*::\n- *GitHub*::\n- *dblp*:: \n- *Website*::\n- *Company*::\n- *Role*::\n- *Location*::\n- *How we met*::\n- *Birthdate*:: %^{Birthdate}u\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("pP" "place" plain
	   "\n:ORG_META:\n- *Address*:: %?\n- *City*::\n- *Why I know this place*::\n- *First time I visited it*:: %^{First time I visited it}u\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("ps" "software" plain
	   "\n:ORG_META:\n- *Developer(s)*:: %?\n- *Status*:: %^{Status|@maintained|@unmaintained}\n- *Repository*::\n- *Recommended by*::\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("r" "resources")
	  ("rb" "book" plain
	   "\n:ORG_META:\n- *Author*:: %?\n- *Status*:: %^{Status|@buyed|@reading|@read}\n- *Recommended by*::\n- *Start date*:: %^{Start date}u\n- *Completed date*:: %^{Completed date}u\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("rm" "manual" plain
	   "\n:ORG_META:\n- *Author(s)*:: %?\n- *Areas*::\n- *Start date*:: %^{Start date}u\n- *Completed date*:: %^{Completed date}u\n- *Reference Entry*::\n- *Resources*::\n- *Keywords*::\n:END:\n* Notes\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("rp" "paper" plain
	   "\n:ORG_META:\n- *Author(s)*:: %?\n- *Areas*::\n- *Reference Entry*::\n- *Related papers*::\n- *Resources*::\n- *Keywords*::\n:END:\n* Notes\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("ra" "article" plain
	   "\n:ORG_META:\n- *Author*:: %?\n- *URL*:: %^{URL}\n- *Related*::\n- *Recommended by*::\n- *Date*:: %^{Date}u\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("rv" "video" plain
	   "\n:ORG_META:\n- *Creator*:: %?\n- *URL*::\n- *Recommended by*::\n- *Date*:: %^{Date}u\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("rc" "conference" plain
	   "\n:ORG_META:\n- *Speaker(s)*:: %?\n- *Where*::\n- *What*::\n- *Date*:: %^{Date}u\n- *Related*::\n- *Resources*::\n- *Keywords*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)

	  ("j" "project")
	  ("jo" "overview" plain
	   "\n:ORG_META:\n- *What*:: %?\n- *Repository*::\n- *Status*:: %^{Status|@active|@completed|@ready|@abandoned}\n- *Date*:: %^{Date}u\n- *Due date*:: %^{Due date}t\n- *Completed date*:: %^{Completed date}u\n- *Success criteria*::\n:END:\n* Details\n* Tasks\n* Resources\n* Artifacts"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("jt" "task" plain
	   "\n:ORG_META:\n- *Project*:: %?\n- *Taken by*::\n- *Status*:: %^{Status|@active|@completed|@picked|@abandoned}\n- *Due date*:: %^{Due date}t\n- *Completed date*:: %^{Completed date}u\n- *Resources*::\n- *Success criteria*::\n:END:\n* Details\n* Sub-tasks\n* Roadmap"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("z" "Zettelkasten")
	  ("zr" "reference" plain
	   "\n:ORG_META:\n- *Date*:: %^{Date}u\n- *Type*:: #reference\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  ("zz" "zettel" plain
	   "\n:ORG_META:\n- *Date*:: %^{Date}u\n- *Type*:: #zettel\n- *References*::\n:END:\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unarrowed t)
	  )))

;; Configuring org-roam-ui to visualize my knowledge graph

(use-package websocket
  :ensure t
  :after org-roam)

(use-package simple-httpd
  :ensure t
  :after org-roam)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t))

(provide 'org-roam-setup)
