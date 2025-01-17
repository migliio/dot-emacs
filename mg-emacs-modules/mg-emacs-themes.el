(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (use-package modus-themes
    :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes")
    :config
    (load-theme 'modus-vivendi t))
  (set-frame-font "Iosevka Comfy 18" nil t))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (use-package modus-themes
    :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes")
    :config
    (load-theme 'modus-vivendi t))
  (set-frame-font "Iosevka Comfy 16" nil t))

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
