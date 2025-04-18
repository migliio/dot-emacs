(defun mg-init-macos-theme ()
  "Enable the theme environment of choice for darwin."
  (use-package standard-themes
    :straight t
    :config
    (load-theme 'standard-dark t))
  (set-frame-font "DejaVu Sans Mono 17" nil t))

(defun mg-init-cli-theme ()
  "Enable the theme environment of choice when from CLI."
  (load-theme 'modus-vivendi)
  (custom-set-faces '(mode-line ((t (:background "grey75" :foreground "black"
						 :box (:line-width -1 :style released-button)))))
		    '(mode-line-inactive ((t (:background "grey60" :foreground "black"
							  :box (:line-width -1 :style released-button))))))
  (menu-bar-mode -1))

(defun mg-init-linux-theme ()
  "Enable the theme enviroment of choice for linux."
  (use-package standard-themes
    :straight t
    :config
    (load-theme 'standard-dark t))
  (set-frame-font "DejaVu Sans Mono 11" nil t))

(if (display-graphic-p)
    (if (equal system-type 'darwin)
  	(mg-init-macos-theme)
      (mg-init-linux-theme))
  (mg-init-cli-theme))
(set-fringe-mode 0)

(use-package hide-mode-line
  :straight t)

(provide 'mg-emacs-themes)
