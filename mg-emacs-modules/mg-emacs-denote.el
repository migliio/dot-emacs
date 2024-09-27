(use-package denote
  :straight (denote :type git :host github :repo "protesilaos/denote" :branch "main")
  :bind (("C-c n n" . denote)
	 ("C-c n x" . denote-region)
	 ("C-c n N" . denote-type)
	 ("C-c n d" . denote-date)
	 ("C-c n y f" . denote-org-extras-dblock-insert-files)
	 ("C-c n y l" . denote-org-extras-dblock-insert-links)
	 ("C-c n y b" . denote-org-extras-dblock-insert-backlinks)
	 ("C-c n y h" . denote-org-extras-link-to-heading)
	 ("C-c n s" . denote-sort-dired)
	 ("C-c n e n" . denote-silo-extras-create-note)
	 ("C-c n e f" . denote-silo-extras-open-or-create)
	 ("C-c n t" . denote-template)
	 ("C-c n i" . denote-link)
	 ("C-c n I" . denote-add-links)
	 ("C-c n b" . denote-backlinks)
	 ("C-c n h" . denote-org-extras-backlinks-for-heading)
	 ("C-c n g f" . denote-find-link)
	 ("C-c n g b" . denote-find-backlink)
	 ("C-c n r" . denote-rename-file)
	 ("C-c n R" . denote-rename-file-using-front-matter))
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  :custom
  (denote-directory (expand-file-name mg-pkm-base-directory))
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
     (course . "#+include: \"/home/claudio/Repositories/knock-files/org-headers/header_notes_document_small.org\"\n* Course details\n- Lecturer ::\n- University ::\n- Academic year ::\n- Resources ::\n- Description ::\n* Lecture notes\n* COMMENT Flashcards\n")
     (zettel . "#+references: \n\n\n-----\n")
     (place . "* Details\n- Link ::\n- Visited ::\n- Description ::\n* Notes\n")
     (contact . "* Contact details\n- E-mail ::\n- Company ::\n- Phone number ::\n- Website ::\n- Twitter ::\n- BBDB entry ::\n- Additional information ::\n* Notes")))
  (denote-date-prompt-use-org-read-date t)
  :config
  (denote-rename-buffer-mode 1)
  ;; Due to an org-mode bug, some ~dblock~ functions are not loaded automatically
  (require 'denote-org-extras)
  ;; (custom-set-variables '(org-link-parameters (quote (("store" . denote-link-ol-store)))))
  )

(use-package mg-denote
  :ensure nil
  :after denote
  :bind
  (("C-c n f f" . mg-denote-find-file)
   ("C-c n z f" . mg-denote-find-zettel)
   ("C-c n u" . mg-copy-denote-like-timestamp-to-killring)
   ("C-c n z i" . mg-denote-insert-zettel-link)))

(use-package denote-explore
  :straight t
  :custom
  (denote-explore-network-directory (concat denote-directory "/.graphs"))
  (denote-explore-network-filename "denote-network")
  (denote-explore-network-format 'gexf)
  (denote-explore-network-graphviz-filetype "gexf"))

(use-package denote-menu
  :after (denote)
  :straight t
  :config
  :bind (("C-c n m l" . list-denotes)
	 ("C-c n m f" . denote-menu-filter-by-keyword)))

(use-package consult-denote
  :straight (consult-denote :type git :host github :repo "protesilaos/consult-denote" :branch "main")
  :bind (("C-c n f g" . consult-denote-grep)
	 ("C-c n f c" . consult-denote-find)))

(provide 'mg-emacs-denote)
