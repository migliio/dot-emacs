(defun mg-notmuch-update-mail ()
  (interactive)
  (async-shell-command "mbsync -a && notmuch new" "*notmuch-mbsync*"))
