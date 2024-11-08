(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (set-frame-font "Iosevka Comfy 17" nil t)
  (use-package ef-themes :straight t :config
    (load-theme 'ef-autumn t)))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (set-frame-font "Iosevka Comfy 14" nil t)
  (use-package ef-themes :straight t :config
    (load-theme 'ef-autumn t)))

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
