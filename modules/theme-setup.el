(defun miglio/guisetup()
  (use-package unicode-fonts
    :ensure t
    :config (unicode-fonts-setup))
  
  (use-package writeroom-mode
    :ensure t
    :config (global-writeroom-mode t)
    (setq writeroom-major-modes '("text-mode" "org-mode" "markdown-mode")))
  
  (use-package focus
    :ensure t
    :config
    (add-hook 'org-mode-hook #'focus-mode))

  (setq-default cursor-type 'bar)
  (blink-cursor-mode)
  (set-face-foreground 'vertical-border "#282828")
  (set-frame-font  "iA Writer DuoS 15" nil t))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(if (display-graphic-p)
    (miglio/guisetup)
  (set-face-background 'default "undefined"))

(provide 'theme-setup)
