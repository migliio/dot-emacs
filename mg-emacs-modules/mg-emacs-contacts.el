(if (not (equal (system-name) mg-work-laptop-hostname))
    (use-package bbdb
	:straight t
	:commands bbdb
	:bind (("C-x c b" . bbdb)
	       ("C-x c c" . bbdb-create))
	:init
	(defconst mg-bbdb-data-file (format "%s/%s" mg-dot-private-file "bbdb/.bbdb"))
	:custom
	(bbdb-mua-pop-up-window-size 1)
	(bbdb-file mg-bbdb-data-file)
	(bbdb-mua-pop-up t)
	(bbdb-mua-pop-up-window-size 5)
	:config
	(autoload 'bbdb-insinuate-mu4e "bbdb-mu4e")))

(provide 'mg-emacs-contacts)
