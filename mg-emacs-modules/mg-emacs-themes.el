(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (progn
    (set-frame-font "Iosevka Comfy 19" nil t)
    (use-package ef-themes :straight t
      :if (display-graphic-p) :config (load-theme 'ef-autumn t))
    (use-package spacious-padding :straight t
      :if (display-graphic-p) :config
      (setq spacious-padding-widths
	    '( :internal-border-width 5
               :header-line-width 0
               :mode-line-width 5
               :tab-width 5
               :right-divider-width 5
               :scroll-bar-width 1
               :fringe-width 1))
      (spacious-padding-mode 1))
    (use-package all-the-icons :straight t
      :if (display-graphic-p))
    (use-package all-the-icons-dired :straight t
      :config
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (progn
    (set-frame-font "Iosevka Comfy 16" nil t)
    (load-theme 'modus-vivendi t)))

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
