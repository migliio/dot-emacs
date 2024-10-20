(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (set-frame-font "Iosevka 19" nil t)
  (use-package gruvbox-theme
    :straight t
    :config (load-theme 'gruvbox t))
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 0.5))
  (use-package all-the-icons :straight t
    :if (display-graphic-p))
  (use-package all-the-icons-dired :straight t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
    (set-frame-font "Iosevka Comfy 16" nil t)
    (load-theme 'modus-vivendi t))

(if (display-graphic-p)
    (progn
      (if (equal system-type 'darwin)
	  (mg-init-macos-theme)
	(mg-init-linux-theme))
      (set-fringe-mode 0))
  (set-face-background 'default "undefined"))

(use-package hide-mode-line
  :straight t)

(provide 'mg-emacs-themes)
