;;; mg-emacs.el --- Custom emacs functionalities -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
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
;; This library introduces some customizations within the Emacs text
;; editor. Its purpose its mainly to have some convenient
;; functionalities built on top of the Emacs base.

;;; Code:

(defvar mg-screenshots-directory "~/.screenshots"
  "Directory for storing screenshots.

  This directory is used by the `mg-take-screenshot' function to
  store screenshots obtained with scrot.")

(defconst mg-scrot-command "/usr/bin/scrot -s "
  "This is the \"scrot\" command to use to take a screenshot and save it.")

(defun mg-insert-today-timestamp-formatted ()
  "Insert a timestamp of today at the current point.

  The timestamp is formatted around square brackets, which is the
  typical way I specify the date. The square bracketed date have a
  specific meaning in org-mode, but here we assume the date is
  inserted in a non-org file, or that it is not meaningful for
  agenda purposes."
  (interactive)
  (insert (format "[%s]" (mg-get-today-timestamp))))

(defun mg-take-screenshot ()
  "Take a screenshot using \"scrot\".

  The file will be saved under the `mg-screenshots-directory'. Since
  screenshots are meant to be further categorized, a \"RENAME\"
  string is added before the \"png\" extension."
  (interactive)
  (let ((screenshot-name (format "%s/%s" mg-screenshots-directory (format-time-string "%Y-%m-%d-%H-%M_screenshot_RENAME.png"))))
    (shell-command (concat mg-scrot-command screenshot-name))))

(defun mg-add-current-file-name-to-killring ()
  "Add the file name visited by the current buffer to the killring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Added file name '%s' to the killring." filename))))

(provide 'mg-emacs)
;;; mg-emacs.el ends here
