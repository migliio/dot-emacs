(if (display-graphic-p)
    (progn
	(if (equal system-type 'darwin)
	    (set-frame-font "Iosevka Comfy 18" nil t)
	  (set-frame-font "Iosevka Comfy 16" nil t))
	(load-theme 'modus-vivendi t)
	(set-fringe-mode 0))
  (set-face-background 'default "undefined"))

(use-package hide-mode-line
  :straight t)

(provide 'mg-emacs-themes)
