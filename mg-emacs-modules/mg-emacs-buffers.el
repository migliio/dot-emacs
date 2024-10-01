(use-package vundo
  :straight t
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :height 1.0))

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
