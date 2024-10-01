(use-package citar
  :straight t
  :init
  (defconst mg-bibliography-path "~/Vault/library/org/main/main.bib")
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-templates
   '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher}.\n")
     (note . "@${author editor}, ${title}")))
  (citar-symbol-separator "  ")
  :bind
  (("C-c n c o" . citar-open)
   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))
  :config
  (setq citar-bibliography (list mg-bibliography-path)))

(use-package citar-denote
  :straight t
  :after (denote)
  :custom
  (citar-open-always-create-notes nil)
  (citar-denote-file-type 'org)
  (citar-denote-subdir nil)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode)
  :bind (("C-c n c c" . citar-create-note)
	 ("C-c n c n" . citar-denote-open-note)
	 ("C-c n c d" . citar-denote-dwim)
	 ("C-c n c e" . citar-denote-open-reference-entry)
	 ("C-c n c a" . citar-denote-add-citekey)
	 ("C-c n c k" . citar-denote-remove-citekey)
	 ("C-c n c r" . citar-denote-find-reference)
	 ("C-c n c f" . citar-denote-find-citation)
	 ("C-c n c l" . citar-denote-link-reference)))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(provide 'mg-emacs-bib)
