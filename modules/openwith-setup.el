(use-package openwith
  :ensure t
  :config
  (setq openwith-associations '(
				("\\.mp4\\'" "mpv" (file))
				("\\.webm\\'" "mpv" (file))								
				("\\.mkv\\'" "mpv" (file))
				("\\.m4a\\'" "mpv --force-window" (file))
				("\\.ppt\\'" "libreoffice" (file))
				("\\.pptx\\'" "libreoffice" (file))
				("\\.doc\\'" "libreoffice" (file))
				("\\.docx\\'" "libreoffice" (file))
				))
  (openwith-mode t))

(provide 'openwith-setup)
