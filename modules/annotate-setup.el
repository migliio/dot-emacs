(use-package annotate
  :ensure t
  :bind ("C-c x a" . annotate-annotate)
  ("C-c x d" . annotate-delete-annotation)
  ("C-c x ]" . annotate-goto-next-annotation)
  ("C-c x [" . annotate-goto-previous-annotation))

(provide 'annotate-setup)
