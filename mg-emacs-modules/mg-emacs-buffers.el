(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package buffier
  :straight (buffier :type git :host github :repo "migliio/buffier" :branch "master")
  :bind (("C-c u b w" . buffier-new-buffer)
	 ("C-c u b l" . buffier-buffers)))

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
