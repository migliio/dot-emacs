;; (use-package ox-pandoc
;;   :ensure t)

(use-package ox-twbs
  :ensure t)

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///home/claudio/Repositories/reveal.js"))

(provide 'org-export-backends-setup)
