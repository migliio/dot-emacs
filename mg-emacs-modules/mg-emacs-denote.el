(use-package denote
  :straight (denote :type git :host github :repo "protesilaos/denote" :branch "main")
  :bind (("C-c n n" . denote)
	 ("C-c n x" . denote-region)
	 ("C-c n N" . denote-type)
	 ("C-c n d" . denote-date)
	 ("C-c n s" . denote-sort-dired)
	 ("C-c n t" . denote-template)
	 ("C-c n i" . denote-link)
	 ("C-c n I" . denote-add-links)
	 ("C-c n b" . denote-backlinks)
	 ("C-c n y q c" . denote-query-contents-link)
	 ("C-c n y q f" . denote-query-filenames-link)
	 ("C-c n g f" . denote-find-link)
	 ("C-c n g b" . denote-find-backlink)
	 ("C-c n r" . denote-rename-file)
	 ("C-c n R" . denote-rename-file-using-front-matter))
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  :custom
  (denote-known-keywords '("emacs" "security" "kernel" "mathematics" "algorithms"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords file-type template signature))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-date-format nil)
  (denote-rename-buffer-format "[D] %s %t (%k)")
  (denote-backlinks-show-context t)
  (denote-dired-directories
   (list denote-directory
	 (thread-last denote-directory (expand-file-name "assets"))))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  (denote-templates
   '((plain . "")
     (course . "#+include: \"~/.emacs.d/headers/header_notes_document_small.org\"\n* Course details\n- Lecturer ::\n- University ::\n- Academic year ::\n- Resources ::\n- Description ::\n* Lecture notes\n")
     (place . "* Details\n- Link ::\n- Visited ::\n- Description ::\n* Notes\n")
     (contact . "* Contact details\n- E-mail ::\n- Company ::\n- Phone number ::\n- Website ::\n- Twitter ::\n- Additional information ::\n* Notes")))
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode 0))

(use-package denote-journal
  :straight t
  :bind (("C-c n j n" . denote-journal-new-entry)
	 ("C-c n j l" . denote-journal-link-or-create-entry)
	 ("C-c n j j" . denote-journal-new-or-existing-entry)))

(use-package denote-org
  :straight t
  :bind (("C-c n h" . denote-org-backlinks-for-heading)
	 ("C-c n y f" . denote-org-extras-dblock-insert-files)
	 ("C-c n y l" . denote-org-extras-dblock-insert-links)
	 ("C-c n y b" . denote-org-extras-dblock-insert-backlinks)
	 ("C-c n y h" . denote-org-extras-link-to-heading)))

(use-package denote-sequence
  :straight (denote-sequence :host github :type git :repo "protesilaos/denote-sequence" :branch "main")
  :bind (("C-c n y s s" . denote-sequence)
	 ("C-c n y s r" . denote-sequence-reparent)
	 ("C-c n y s d" . denote-sequence-dired)
	 ("C-c n y s f" . denote-sequence-find)
	 ("C-c n y s l" . denote-sequence-link)
	 ("C-c n y s c s" . denote-sequence-new-sibling-of-current)
	 ("C-c n y s c c" . denote-sequence-new-child-of-current))
  :custom
  (denote-sequence-scheme 'alphanumeric))

(use-package mg-denote
  :ensure nil
  :bind
  (("C-c n f f" . mg-denote-find-file)
   ("C-c n z f" . mg-denote-find-zettel)
   ("C-c n z g" . mg-denote-grep-on-zettels)
   ("C-c n y s i" . mg-denote-get-index-in-dired)
   ("C-c n u" . mg-denote-copy-timestamp-to-killring)
   ("C-c n o r" . mg-denote-copy-to-assets-and-rename)
   ("C-c n z i" . mg-denote-insert-zettel-link)))

(use-package denote-search
  :straight (denote-search :type git :host github :repo "lmq-10/denote-search" :branch "main")
  :bind
  (("C-c n f s" . denote-search)))

(use-package denote-explore
  :straight t
  :custom
  (denote-explore-network-directory (concat denote-directory "/.graphs"))
  (denote-explore-network-filename "denote-network")
  (denote-explore-network-format 'gexf)
  (denote-explore-network-graphviz-filetype "gexf"))

(use-package denote-menu
  :straight t
  :config
  :bind (("C-c n m l" . list-denotes)
	 ("C-c n m f" . denote-menu-filter-by-keyword)))

(use-package consult-denote
  :straight t
  :bind (("C-c n f g" . consult-denote-grep)
	 ("C-c n f c" . consult-denote-find)))

(provide 'mg-emacs-denote)
