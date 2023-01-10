(setq org-plantuml-jar-path (expand-file-name "/home/claudio/Repositories/plantuml/plantuml-1.2022.1.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(provide 'plantuml-setup)
