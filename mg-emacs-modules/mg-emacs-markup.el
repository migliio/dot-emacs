(use-package outline
  :ensure nil
  :bind
  ("C-c u c o" . outline-minor-mode)
  :custom
  (outline-minor-mode-highlight nil)
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons nil)
  (outline-minor-mode-use-margins nil))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'mg-emacs-markup)
