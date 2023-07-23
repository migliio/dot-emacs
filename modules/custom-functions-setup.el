;; Copy a file in the current Dired directory
(defun mg/dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))
(eval-after-load "dired"
  '(define-key dired-mode-map "\M-c" 'dired-copy-file-here))
(global-set-key (kbd "C-c f c") 'mg/copy-file)

;; Copy file name to clipboard
(defun mg/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
					(buffer-file-name))))
	(when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c f n") 'mg/copy-file-name-to-clipboard)

;; Perform org-capture with a side org agenda
(defun mg/agenda-w-capture ()
  (interactive)
  (split-window-right)
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda nil "c"))
  (org-capture))

(provide 'custom-functions-setup)
