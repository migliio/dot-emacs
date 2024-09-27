(use-package magit
  :straight t
  :bind (("C-c x" . magit))
  :config
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setq magit-refresh-status-buffer t)
  (setq git-commit-fill-column 75))

(provide 'mg-emacs-git)
