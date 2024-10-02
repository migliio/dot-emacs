;;; mg-utils.el --- Custom utility functions for Emacs Lisp coding -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway/emacs/dot-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.3"))

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
;; This library introduces some utility functions that I find useful when coding in Emacs Lisp.

;;; Code:

(defconst mg-work-laptop-hostname "nano"
  "This constant keeps track of the hostname I have on my Linux work laptop.")

(defconst mg-personal-laptop-hostname "think"
  "This constant keeps track of the hostname I have on my Linux personal laptop.")

(defconst mg-pkm-base-directory "~/Vault/pkm"
  "This constant keeps track of the base directory for my entire knowledge base.")

(defun mg--get-major-modes ()
  "Utility function to get loaded major modes.

  This function gets the major modes by leveraging on `auto-mode-alist', which is a list of cons nodes of the form \"(<name-pattern> . <major-mode-string>)\". However, there are multiple entries per each major mode, thus we shoud filter duplicates afterwards."
  (let ((modes
	 (mapcar (lambda (alist-cons) (cdr alist-cons)) auto-mode-alist)))
    (delete-dups modes)))

(defun mg-get-today-timestamp ()
  "Helper function to get today's timestamp with the abbreviated day name."
  (format-time-string "%Y-%m-%d %a"))

(provide 'mg-utils)
;;; mg-utils.el ends here
