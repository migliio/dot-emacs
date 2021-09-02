;;; -*- lexical-binding: t -*-

;;
;; +----- CONFIGURATIONS ----- +
;;

;; I like to keep a few things private, so we
;; load a private.el if it exists

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file))
     (when custom-file
       (load-file custom-file))
     (server-start))))

;; Initialize package to download new packages

(require 'package)
(package-initialize)

;; Choose melpa as default mirror
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA" . 5)
        ("gnu" . 0)))

;; The packages listed below are assumed to be installed

      (let* ((package--builtins nil)
             (packages
              '(auto-compile         ; Automatically compile Emacs Lisp libraries
				all-the-icons        ; Nice icons
				doom-modeline        ; A nice modeline taken from Doom Emacs
				ccls                 ; ccls support in Emacs
                org-brain            ; An incredibly powerful org mode wiki
                counsel              ; Various completion functions using Ivy
                deft                 ; Browse and filter plain text notes
				counsel-gtags        ; Good integration between counsel and gtags
                counsel-projectile   ; Ivy integration for Projectile
				dashboard            ; A nice dashboard
                define-word          ; display the definition of word at point
                diff-hl              ; Highlight uncommitted changes using VC
                doom-themes          ; An opinionated pack of modern color-themes
				emojify              ; Display emojis in Emacs
                erlang               ; Erlang major mode
				ein                  ; Jupyter Python notebooks inside Emacs
				elfeed               ; RSS reader in Emacs
                expand-region        ; Increase selected region by semantic units
				flycheck             ; Modern on-the-fly syntax checking extension
                focus                ; Dim color of text in surrounding sections
				ghub                 ; Display GitHub notifications
                golden-ratio         ; Automatic resizing windows to golden ratio
                haskell-mode         ; A Haskell editing mode
                jedi                 ; Python auto-completion for Emacs
                js2-mode             ; Improved JavaScript editing mode
				ledger-mode          ; Use ledger as my accounting system
				lsp-ivy              ; Interactive ivy interface with lsp-mode
                lsp-java             ; Java support for lsp-mode
                lsp-mode             ; LSP mode
				lsp-python-ms        ; Python support for lsp-mode
				lsp-ui               ; UI for LSP mode
                magit                ; control Git from Emacs
                markdown-mode        ; Emacs Major mode for Markdown-formatted files
                multiple-cursors     ; Multiple cursors for Emacs
				notmuch              ; A fast and lightweight e-mail client
                undo-tree            ; A nice tree visualization of previous changes
                olivetti             ; Minor mode for a nice writing environment
				ol-notmuch           ; Enable linking to notmuch messages
				openwith             ; Open files with external applications
                org                  ; Outline-based notes management and organizer
				org-board            ; Nice web archival system
                org-bullets          ; Show bullets in org-mode as UTF-8 characters
                org-journal          ; Nice functions to maintain a simple diary
				org-noter            ; A nice package to deal with pdf notes in org mode
				org-roam             ; Roam Research replice for org-mode
				org-sidebar          ; Enable sidebars in org buffer
				org-super-agenda     ; Org agenda, improved
                paredit              ; minor mode for editing parentheses
                pdf-tools            ; Emacs support library for PDF files
				polymode             ; Fixing an ein bug
				popwin               ; Popup window manager
                projectile           ; Manage and navigate projects in Emacs easily
                proof-general        ; A generic Emacs interface for proof assistants
				quelpa               ; Install Emacs Lisp packages directly from source
                slime                ; Superior Lisp Interaction Mode for Emacs
				spacemacs-theme      ; A nice theme similar to spacemacs
				tramp-term           ; A super powerful terminal emulator
				vterm                ; They say that this is a better terminal
                smex                 ; M-x interface with Ido-style fuzzy matching
                which-key            ; Display available keybindings in popup
                )))
        (when (memq window-system '(mac ns))
          (push 'exec-path-from-shell packages)
          (push 'reveal-in-osx-finder packages))
        (let ((packages (seq-remove 'package-installed-p packages)))
          (print packages)
          (when packages
            ;; Install uninstalled packages
            (package-refresh-contents)
            (mapc 'package-install packages))))

;; Enable Mac OS support and set the Command key as the Meta key

(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
        mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t)
  ;;(exec-path-from-shell-initialize)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode 1)))

;; PDFs are fuzzy with Retina display

;; uses more memory; see https://github.com/politza/pdf-tools/issues/51
(when (memq window-system '(mac ns))
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

;; System locale to use for formatting time values.

(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English

;; Set some defaults

(setq auto-revert-interval 1            ; Refresh buffers fast
      echo-keystrokes 0.1               ; Show keystrokes asap
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
	  lsp-keymap-prefix "C-c l"         ; Change the lsp keymap
      scroll-margin 1                   ; Space between cursor and top/bottom
      sentence-end-double-space nil     ; No double space
      custom-file                       ; Customizations in a separate file
      (concat user-emacs-directory "custom.el"))
;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

;; Set some defaults globally

(setq-default tab-width 4                       ; Smaller tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil        ; Split verticly by default
	      frame-resize-pixelwise t)         ; Fine-grained frame resize

;; Use a custom directory where keep all extensions installed manually

(let ((default-directory (concat user-emacs-directory "site-lisp/")))
  (when (file-exists-p default-directory)
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
             (normal-top-level-add-subdirs-to-load-path)) load-path))))

;; Cannot load custom elisp files in MacOS

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Answering 'yes' and 'no' is tedious

(fset 'yes-or-no-p 'y-or-n-p)

;; Put auto-saved files in a single directory

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
     directory called autosaves located wherever your .emacs.d/ is
     located.")

;; sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; Use UTF-8 as a preferred coding system

(set-language-environment "UTF-8")

;; Automatically revert doc-view buffers when the file changes on disk

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Disable some boring modes

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
	   menu-bar-mode                ; Menu bar is useless
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))

;; Enable some useful modes

(dolist (mode
         '(abbrev-mode                  ; E.g. sopl -> System.out.println
           column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           dirtrack-mode                ; Directory tracking in *shell*
           global-diff-hl-mode          ; Highlight uncommitted changes
		   global-flycheck-mode         ; Enable on the fly syntax checking
           counsel-projectile-mode      ; Manage and navigate projects
		   global-emojify-mode          ; Enable emojify
           recentf-mode                 ; Recently opened files
           show-paren-mode              ; Highlight matching parentheses
           which-key-mode))             ; Available keybindings in popup
  (funcall mode 1))

(when (version< emacs-version "24.4")
  (eval-after-load 'auto-compile
    '((auto-compile-on-save-mode 1))))  ; compile .el files on save

;; Enable visual-line-mode globally

(global-visual-line-mode)

;; Enable winner mode globally

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Enable undo-tree globally

(global-undo-tree-mode)

;; Enable openwith globally

(openwith-mode t)
(setq openwith-associations '(
                              ("\\.mp4\\'" "vlc" (file))
							  ("\\.mkv\\'" "vlc" (file))
							  ("\\.m4a\\'" "vlc" (file))
                              ("\\.ppt\\'" "libreoffice" (file))
                              ("\\.pptx\\'" "libreoffice" (file))
                              ("\\.doc\\'" "libreoffice" (file))
                              ("\\.docx\\'" "libreoffice" (file))
                              ))

;; Install Nano theme

(quelpa '(nano-theme :fetcher git :url "https://github.com/rougier/nano-theme.git"))

;; Change the color theme

;; (load-theme 'spacemacs-dark t)
;; (load-theme 'doom-xcode t)
(load-theme 'doom-one t)

;; Change font and size

(set-frame-font "JetBrains Mono 14" nil t)
(when (memq window-system '(mac ns))
  (set-frame-font "JetBrains Mono 16" nil t))

;; Enable doom-modeline

(require 'doom-modeline)
(doom-modeline-mode 1)

;; Customize doom-modeline

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be displayed.
(setq doom-modeline-window-width-limit fill-column)

;; How to detect the project root.
;; The default priority of detection is `ffip' > `projectile' > `project'.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'project)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon (display-graphic-p))

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Let's try out ivy

(setq ivy-wrap t
      ivy-height 25
      ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-on-del-error-function 'ignore)
(ivy-mode 1)

;; Show directories first in dired

(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; Toggle dashboard at startup

(require 'dashboard)
(setq dashboard-startup-banner 'logo)
(dashboard-setup-startup-hook)

;; Switch between line mode and char mode in term

(require 'term)

(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

;; Make pdf tools working and do pdf-tools-install whenever a PDF is opened

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
(add-hook 'pdf-view-mode-hook
          (lambda () (setq header-line-format nil)))

;; In org-mode, I want source blocks to be themed as they would in native mode

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; Enable org-bullets when opening org-files

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Use org-crypt

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; Set org-mode TODO keywords

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)"))))

;; Enable DONE logging in org-mode

(setq org-log-done 'time)

;; Set org-export backends

(require 'ox-twbs)
(require 'ox-reveal)
(setq org-export-backends '(html latex ox-twbs ox-reveal))

;; View LaTeX previews in better quality

(setq org-latex-create-formula-image-program 'dvisvgm)

;; Set org agenda directory

(setq org-agenda-files (list"~/Dropbox/org-files/roam/journal"))

;; Enable org-super-agenda

(require 'org-super-agenda)
(setq org-super-agenda-groups
      '((:name ":timer:"
	       :time-grid t
	       :tag "timeblocking")
		(:name ":boy_tone1:"
		       :tag "personal")
		(:name ":heavy-check-mark:"
		       :tag "todo")
 		(:name ":bookmark-tabs:"
		       :tag "rush")
		(:name ":inbox_tray:"
		       :tag "inputs")
		(:name ":exclamation:"
               ;; Single arguments given alone
			   :tag "urgent"
			   :tag "exam"
               :priority "A")))
(org-super-agenda-mode)

;; Enable and set org-crypt

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; gpg key to use for encryption
(setq org-crypt-key nil)

;; Settings for elfeed
(setq elfeed-feeds
      '("https://awealthofcommonsense.com/feed"
		"https://ofdollarsanddata.com/feed"
		"https://www.smbc-comics.com/comic/rss"
		"https://xkcd.com/rss.xml"
		"https://fs.blog/blog/feed/"
		"https://gwern.substack.com/feed"
		"https://moretothat.com/feed/"
		"https://putanumonit.com/feed/"
		"https://www.ribbonfarm.com/feed/"
		"https://retireinprogress.com/feed/"
		))

;; Set up notmuch e-mail client

(require 'notmuch)

;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "migliorelliclaudio@gmail.com"
      user-full-name "Claudio Migliorelli")
;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/Repositories/Maildir/Gmail/[Gmail].Bozze")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/Repositories/Maildir/Gmail/")

(require 'popwin)
(defun notmuch-exec-offlineimap ()
  "execute offlineimap"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "offlineimap"
                                "*offlineimap*"
                                "offlineimap -o")
   '(lambda (process event)
      (notmuch-refresh-all-buffers)
      (let ((w (get-buffer-window "*offlineimap*")))
        (when w
          (with-selected-window w (recenter (window-end)))))))
  (popwin:display-buffer "*offlineimap*"))

(add-to-list 'popwin:special-display-config
             '("*offlineimap*" :dedicated t :position bottom :stick t
               :height 0.4 :noselect t))

;; Link e-mails in org mode files

(require 'ol-notmuch)

;; Set the deft directory and file extensions

(setq deft-directory "~/Dropbox/org-files/roam/")
(setq deft-extensions '("org" "md" "txt"))
(add-to-list 'deft-extensions "tex")
(setq deft-recursive t)

;; Set the org brain directory

(setq org-brain-path "~/Dropbox/org-files/brain")

;; Enable all-the-icons for org-brain

(defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        ((string-prefix-p "info:" link)
                         (all-the-icons-octicon "info"))
                        ((string-prefix-p "help:" link)
                         (all-the-icons-material "help"))
                        ((string-prefix-p "http" link)
                         (all-the-icons-icon-for-url link))
                        (t
                         (all-the-icons-icon-for-file link))))))

(add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)

;; Set up org-roam
(setq org-roam-v2-ack t)
(require 'org-roam)
(setq org-roam-directory "~/Dropbox/org-files/roam")
(setq org-roam-completion-everywhere t)
(org-roam-setup)
;; ;; Auto load org-roam buffer
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (and (org-roam-file-p)
;;                  (not (eq 'visible (org-roam-buffer--visibility)))
;;                  (org-roam-buffer-toggle))))
;; org-roam templates
(setq org-roam-capture-templates
      '(("d" "default" plain "\n- *Keywords*::\n\n%?"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unnarrowed t)

		("u" "university")
		("uc" "course" plain
		 "\n- *Lecturer*:: %?\n- *University*:: \n- *Academic Year*:: %^{Academic Year}\n- *Semester*:: %^{Semester}\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("ul" "lecture" plain
		 "\n- *Course*:: %?\n- *Lecture #*:: %^{Lecture #}\n- *Lecturer*::\n- *Date*:: %^{Date}u\n- *Resources*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)

		("p" "people" plain
		 "\n- *Phone number*:: %^{Phone number}\n- *E-mail*:: %^{E-mail}\n- *Twitter*:: %^{Twitter}\n- *GitHub*:: %^{GitHub}\n- *Website*:: %^{Website}\n- *Company*:: %?\n- *Role*:: %^{Role}\n- *Location*::\n- *How we met*:: %^{How we met}\n- *Birthdate*:: %^{Birthdate}u\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("s" "software" plain
		 "\n- *Developer(s)*:: %?\n- *Status*:: %^{Status|@maintained|@unmaintained}\n- *Repository*:: %^{Repository}\n- *Recommended by*::\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("P" "place" plain
		 "\n- *Address*:: %^{Address}\n- *City*::%?\n- *Why I know this place*:: %^{Why I know this place}\n- *First time I visited it*:: %^{First time I visited it}u\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		
		("r" "resources")
		("rb" "book" plain
		 "\n- *Author*:: %?\n- *Status*:: %^{Status}\n- *Recommended by*::\n- *Start date*:: %^{Start date}u\n- *Completed date*:: %^{Completed date}u\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("ra" "article" plain
		 "\n- *Author*:: %?\n- *URL*:: %^{URL}\n- *Related*:: %^{Related}\n- *Recommended by*::\n- *Date*:: %^{Date}u\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("rv" "video" plain
		 "\n- *Creator*:: %?\n- *URL*:: %^{URL}\n- *Recommended by*::\n- *Date*:: %^{Date}u\n- *Keywords*::\n\n"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)

		("j" "project")
		("jo" "overview" plain
		 "\n- *What*:: %^{What}\n- *Areas*:: %?\n- *Repository*:: %^{Repository}\n- *Status*:: %^{Status|@active|@ready|@abandoned}\n- *Date*:: %^{Date}u\n- *Due date*:: %^{Due date}t\n- *Completed date*:: %^{Date}u\n- *Success criteria*::\n- *Keywords*::\n\n* Details\n* Tasks\n* Resources\n* Artifacts"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("jt" "task" plain
		 "\n- *Taken by*:: %?\n- *Status*:: %^{Status|@active|@picked|@abandoned}\n- *Due date*:: %^{Due date}t\n- *Completed date*:: %^{Date}u\n- *Success criteria*::\n\n* Details\n* Roadmap"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		
		("R" "plans")
		("Ry" "year" plain
		 "\n- *Feelings*:: %^{Feelings|:smile:|:neutral_face:|:disappointed:}\n- *Related*:: %^{Related}\n- *Date*:: %^{Date}u\n- *Keywords*:: %?\n\n* Overview\n* Values review and life physolophy\n* 5 Years Vision(s)\n* Goal definition\n* Financial review"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("Rq" "quarter" plain
		 "\n- *Feelings*:: %^{Feelings|:smile:|:neutral_face:|:disappointed:}\n- *Related*:: %^{Related}\n- *Date*:: %^{Date}u\n- *Keywords*:: %?\n\n* Overview\n* Projects review\n* Financial review"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("Rm" "month" plain
		 "\n- *Feelings*:: %^{Feelings|:smile:|:neutral_face:|:disappointed:}\n- *Related*:: %^{Related}\n- *Date*:: %^{Date}u\n- *Keywords*:: %?\n\n* Overview\n* Projects and task picking\n* Financial review"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		("Rw" "week" plain
		 "\n- *Feelings*:: %^{Feelings|:smile:|:neutral_face:|:disappointed:}\n- *Related*:: %^{Related}\n- *Date*:: %^{Date}u\n- *Keywords*:: %?\n\n* Overview\n* Time blocking\n* Task picking"
		 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							"#+title: ${title}\n")
		 :unarrowed t)
		))
;; journaling in org-roam
(setq org-roam-dailies-directory "journal/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

;; Jump from LaTeX source code to exported pdf

(defun org-sync-pdf ()
  (interactive)
  (let ((headline (nth 4 (org-heading-components)))
        (pdf (concat (file-name-base (buffer-name)) ".pdf")))
    (when (file-exists-p pdf)
      (find-file-other-window pdf)
      (pdf-links-action-perform
       (cl-find headline (pdf-info-outline pdf)
                :key (lambda (alist) (cdr (assoc 'title alist)))
                :test 'string-equal)))))

;; Disable the current custom theme after load-theme

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

;; Set default directory for find-file
(setq default-directory "~/")

;;
;; +----- MODE SPECIFIC ----- +
;;

;; LSP mode works really well, enabling it

(with-eval-after-load 'lsp
  (lsp-enable-which-key-integration t))

;; Enable ccls

(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls")

;; Enable python support for lsp-mode

(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook #'lsp-deferred) ; or lsp

;; Enable Java support for lsp-mode

(add-hook 'java-mode-hook 'lsp-deferred)

;; Setting up the LaTeX environment

;; Associate .tex files with latex-mode instead of tex-mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

;; Use biblatex for bibliography
(setq-default bibtex-dialect 'biblatex)

;; When opening a just-exported pdf file use Emacs to open it
(require 'org)
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

;;
;; +----- KEY BINDINGS ----- +
;;

;; General keybindings

(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

;; Keybindings for coding

(global-set-key (kbd "C-c c") 'compile)

;; Keybindings for org

(global-set-key (kbd "C-c s") 'org-store-link)

;; Keybindings for org-agenda

(global-set-key (kbd "C-c a") 'org-agenda)

;; Keybindings for org-brain

(global-set-key (kbd "C-x c b") 'org-brain-visualize)

;; Keybindings for org-journal

(global-set-key (kbd "C-x c j") 'org-journal-new-entry)

;; Keybindings for org-roam

(global-set-key (kbd "C-x c r t") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-x c r f") 'org-roam-node-find)
(global-set-key (kbd "C-x c r i") 'org-roam-node-insert)
(global-set-key (kbd "C-x c r s") 'org-roam-protocol-store-links)
(global-set-key (kbd "C-c i") 'completion-at-point)

;; Keybindings for org-roam-dailies

(global-set-key (kbd "C-x c r d t") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-x c r d T") 'org-roam-dailies-capture-tomorrow)
(global-set-key (kbd "C-x c r d y") 'org-roam-dailies-capture-yesterday)
(global-set-key (kbd "C-x c r d d") 'org-roam-dailies-capture-date)
(global-set-key (kbd "C-x c r d f t") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-x c r d f y") 'org-roam-dailies-goto-yesterday)
(global-set-key (kbd "C-x c r d f d") 'org-roam-dailies-goto-date)

;; Keybindings for olivetti

(global-set-key (kbd "C-c o") 'olivetti-mode)

;; Keybindings for org-board

(global-set-key (kbd "C-x c w n") 'org-board-new)
(global-set-key (kbd "C-x c w a") 'org-board-archive)
(global-set-key (kbd "C-x c w o") 'org-board-open)

;; Keybindings for counsel

(global-set-key (kbd "C-s")     'swiper-isearch)
(global-set-key (kbd "C-r")     'swiper-isearch-backward)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y")     'counsel-yank-pop)
(global-set-key (kbd "C-x b")   'ivy-switch-buffer)

;; Keybindings for notmuch

(global-set-key (kbd "C-x c n") 'notmuch)

;; Keybindings for elfeed
(global-set-key (kbd "C-x c w") 'elfeed)

;; Keybindings for term

(global-set-key (kbd "C-x c t") 'term)

;; Keybindings for vterm

(global-set-key (kbd "C-x c v") 'vterm)

;; Keybindings for deft

(global-set-key (kbd "C-x c d") 'deft)

;; Keybindings for magit

(global-set-key (kbd "C-x g") 'magit)

;; Keybindings for focus-mode

(global-set-key (kbd "C-c C-q")
  '(lambda ()
     (interactive)
     (focus-mode 1)
     (focus-read-only-mode 1)))

;; Keybindings for synching org to LaTeX-exported PDF

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") 'org-sync-pdf))

;;
;; +----- END ----- +
;;
