(use-package olivetti
  :straight t)

(use-package logos
  :bind (("C-c p f" . logos-focus-mode))
  :straight t
  :custom
  (logos-outlines-are-pages t)
  :config
  (setq-default logos-hide-cursor nil
		  logos-hide-mode-line t
		  logos-hide-header-line t
		  logos-hide-buffer-boundaries t
		  logos-hide-fringe t
		  logos-variable-pitch nil
		  logos-buffer-read-only nil
		  logos-scroll-lock nil
		  logos-olivetti t)
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)))

(provide 'mg-emacs-layouts)
