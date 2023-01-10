(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(global-visual-line-mode t)               ; Enable visual line
(winner-mode t)                           ; Enable winner mode
(setq-default tab-width 8)                ; Set indentation characters
(setq-default c-basic-offset 8)           ; Set indentation in C


;; Some useful global keybinds
(global-set-key (kbd "C-c h") 'winner-undo)
(global-set-key (kbd "C-c l") 'winner-redo)
(global-set-key (kbd "C-c c k s") 'replace-string)
(global-set-key (kbd "C-c c k c") 'comment-region)
(global-set-key (kbd "C-c c k u") 'uncomment-region)

(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
(setq linum-format "%4d ")                ; Line number format
(global-auto-revert-mode t)               ; Auto-update buffer if file has changed on disk
(add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
          #'display-line-numbers-mode
          #'linum-mode)
(setq display-line-numbers-type 'relative); Show relative line numbers

;; Disable some boring modes
(dolist (mode
         '(tool-bar-mode                  ; No toolbars, more room for text
           scroll-bar-mode                ; No scroll bars either
           menu-bar-mode                  ; Menu bar is useless
           blink-cursor-mode))            ; The blinking cursor gets old
  (funcall mode 0))

;; Solves a bug with xdg-open
(setq process-connection-type nil)

;; System locale to use for formatting time values
(setq system-time-locale "C")

;; Use UTF-8 as a preferred coding system
(set-language-environment "UTF-8")

(provide 'useful-defaults-setup)
