(use-package citar
  :ensure t
  :custom
  (org-cite-global-bibliography '("~/Vault/library/org/main/main.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (("C-c c o" . citar-open)
   ("C-c c e" . citar-open-entry)
   ("C-c c n" . citar-open-notes)
   ("C-c c l" . citar-open-links)
   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))
  :config
  (setq citar-templates
	'((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher}.\n")
          (note . "@${author editor}, ${title}")))
  (setq citar-file-notes-extensions '("org")
	citar-notes-paths '("~/Vault/pkm/slip-box"))
  (setq citar-symbol-separator "  "))

(provide 'citar-setup)
