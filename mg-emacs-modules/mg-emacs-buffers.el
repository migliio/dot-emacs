(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode))

(use-package avy
  :straight t
  :after org
  :init
  (eval-after-load 'org
    (progn
      (define-key org-mode-map (kbd "C-c ,") nil)
      (define-key org-mode-map (kbd "C-c ;") nil)))
  :bind
  (("C-c ;" . avy-goto-line)
   ("C-c ," . avy-goto-char)))

(provide 'mg-emacs-buffers)
