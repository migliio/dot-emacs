(use-package side-notes
  :ensure t
  :config
  (setq-default side-notes-file "notes.org")
  (define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes))

(provide 'side-notes-setup)
