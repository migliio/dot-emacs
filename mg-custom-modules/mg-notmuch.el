;;; mg-notmuch.el --- Custom functions for `notmuch' -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.com/emacs/dot-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This library introduces a series of custom function I use for the
;; `notmuch' e-mail client for Emacs.

;;; Code:

(defconst mg-notmuch-update-shell-commands
  (list "mbsync -a"
	"notmuch new")
  "Shell commands used to update the mail directory.")

(defconst mg-notmuch-update-buffer-name "*notmuch-mbsync*"
  "Name of the `notmuch' update buffer.")

(defun mg-notmuch-update-mail ()
  "Update the mail directory."
  (interactive)
  (dolist (cmd mg-notmuch-update-shell-commands)
    (async-shell-command cmd mg-notmuch-update-buffer-name)))

(provide 'mg-notmuch)

;;; mg-notmuch.el ends here
