(use-package doom-themes
  :ensure t
  :config
  (unless (display-graphic-p)
    (load-theme 'doom-gruvbox t)))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package spaceline
  :ensure t
  :config
  (setq spaceline-org-clock-p t)
  (setq spaceline-org-clock-format-function (lambda ()
					      (truncate-string-to-width (org-clock-get-clock-string) (- (window-total-width) 70) 0 nil t)))
  (when (display-graphic-p)
    (spaceline-spacemacs-theme)))

(use-package berrys-theme
  :ensure t)

(use-package unicode-fonts
  :ensure t
  :config
  (when (display-graphic-p)
    (unicode-fonts-setup)))

(if (display-graphic-p)
    (progn
      (load-theme 'berrys t)
      (set-frame-font "iA Writer Duospace 14" nil t))
  (set-face-background 'default "undefined"))

(provide 'theme-setup)
