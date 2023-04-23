(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :bind (("C-c m" . mu4e))
  :config
  (setq mu4e-maildir (expand-file-name "~/Maildir")
	mu4e-use-fancy-chars nil
	mu4e-attachment-dir  "~/Downloads"
	message-send-mail-function 'message-send-mail-with-sendmail
	sendmail-program "/home/claudio/.nix-profile/bin/msmtp"
	message-kill-buffer-on-exit t
	mu4e-get-mail-command "mbsync -a"
	mu4e-update-interval 300
	mu4e-context-policy 'pick-first
	mu4e-headers-auto-update t
	mu4e-contexts
	`(,(make-mu4e-context
	    :name "polimi"
	    :enter-func (lambda () (mu4e-message "Switch to the polimi context"))
	    :match-func (lambda (msg)
			  (when msg
			    (mu4e-message-contact-field-matches msg
								:to "claudio.migliorelli@mail.polimi.it")))
	    :vars '((mu4e-sent-folder       . "/polimi/sent")
		    (mu4e-drafts-folder     . "/polimi/drafts")
		    (mu4e-trash-folder      . "/polimi/trash")
		    (user-mail-address	   . "claudio.migliorelli@mail.polimi.it")
		    (user-full-name	   . "Claudio Migliorelli" )
		    (mu4e-maildir-shortcuts . (("/polimi/INBOX" . ?i)
					       ("/polimi/sent" . ?s)
					       ("/polimi/drafts" . ?d)
					       ("/polimi/trash" . ?t)))
		    (mu4e-sent-messages-behavior . delete)))))
  (setq mu4e-headers-thread-single-orphan-prefix '("└>" . " ")
	mu4e-headers-thread-child-prefix '("└> " . " ")
	mu4e-headers-thread-last-child-prefix '("└> " . " ")
	mu4e-headers-thread-connection-prefix '("│ " . " ")
	mu4e-headers-thread-orphan-prefix '("└>" . " ")
	mu4e-headers-thread-root-prefix '("> " . " "))
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  (defun mg/message-insert-citation-line ()
    "Based off `message-insert-citation-line`."
    (when message-reply-headers
      (insert "On " (format-time-string "%a, %d %b %Y %H:%M:%S %z" (date-to-time (mail-header-date message-reply-headers))) " ")
      (insert (mail-header-from message-reply-headers) " wrote:")
      (newline)
      (newline)))

  (setq message-citation-line-function 'mg/message-insert-citation-line))

(provide 'email-setup)
