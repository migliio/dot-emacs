;; Enable Mac OS support and set the Command key as the Meta key
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
		mac-option-modifier nil
		mac-command-modifier 'meta
		x-select-enable-clipboard t)
  (when (fboundp 'mac-auto-operator-composition-mode)
	(mac-auto-operator-composition-mode 1)))

;; PDFs are fuzzy with Retina display  
;; uses more memory; see https://github.com/politza/pdf-tools/issues/51
(when (memq window-system '(mac ns))
  (setq pdf-view-use-scaling t
		pdf-view-use-imagemagick nil))

(provide 'macos-setup)
