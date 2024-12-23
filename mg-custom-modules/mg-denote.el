;;; mg-denote.el --- Custom code to extend denote -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
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
;; This library introduces some custom Emacs Lisp code to extend the
;; functionalities provided by `denote'. Along with `org', `denote' is
;; probably one of the packages that I use (and tweak) the most within
;; Emacs.

;;; Code:

(defun mg-denote-copy-timestamp-to-killring ()
  "Helper function to get a convenient denote-style timestamp."
  (interactive)
  (kill-new (denote-get-identifier)))

(defun mg-denote--get-item (filter-regex)
  "Get a file path interactively starting from the denote-directory."
  (let* ((candidates (denote-directory-files filter-regex))
	 (file-name (completing-read
		     "Choose FILE: "
		     candidates))
	 (file-path file-name))
    file-path))

(defun mg-denote--get-file ()
  "Get a denote file interactively starting from the denote-directory"
  (mg-denote--get-item denote-id-regexp))

(defun mg-denote-find-file ()
  "Find files interactively starting from the denote-directory."
  (interactive)
  (find-file (mg-denote--get-file)))

(defun mg-denote--get-zettel ()
  "Get zettel interactively starting from the denote directory."
  (mg-denote--get-item denote-signature-regexp))

(defun mg-denote-find-zettel ()
  "Find zettels interactively starting from the denote-directory."
  (interactive)
  (find-file (mg-denote--get-zettel)))

(defun mg-insert-denote-or-normal-link (name)
  "Insert a denote link if the file specified by buffer-name is a denote item, otherwise a normal link."
  (if (denote-file-is-note-p (format "%s" name))
      (mg-denote--insert-link-from-file-path name)
    (format "[[file:%s]]" name)))

(defun mg-denote--insert-link-from-file-path (file-path)
  "Insert a denote link provided FILE-PATH."
  (let ((file-description (denote--link-get-description file-path)))
    (denote-link file-path (denote-filetype-heuristics file-path) file-description)))

(defun mg-denote-generate-link-from-file-path (file-path)
  "Generate a denote link provided FILE-PATH."
  (let ((file-description (denote--link-get-description file-path)))
    (denote-format-link file-path file-description (denote-filetype-heuristics file-path) nil)))

(defun mg-denote-insert-zettel-link ()
  "Select a zettel from `denote-directory` and insert its link at current point."
  (interactive)
  (let ((file-path
	 (mg-denote--get-zettel)))
    (mg-denote--insert-link-from-file-path file-path)))

(defun mg-denote-grep-on-zettels ()
  "Grep for a search query, but only on zettels."
  (interactive)
  (let ((zettels
	 (denote-directory-files denote-signature-regexp)))
    (consult-grep zettels)))

(provide 'mg-denote)
;;; mg-denote.el ends here
