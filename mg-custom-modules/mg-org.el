;;; mg-org.el --- Org-mode custom extensions -*- lexical-binding: t -*-

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
;; This library introduces some custom Emacs Lisp code to extend
;; `org-mode', which is one of the Emacs packages that I use the most.

;;; Code:

(defconst mg-work-projects-file "~/Vault/pkm/20231210T220334--work-and-study-projects__project_work.org")
(defconst mg-inbox-file "~/Vault/pkm/20231211T145832--inbox__gtd_personal.org")
(defconst mg-agenda-file "~/Vault/pkm/20231210T224321--agenda__personal.org")
(defconst mg-archive-file "~/Vault/pkm/.archive/archive.org")
(defconst mg-capture-notes-file "~/Vault/pkm/20231213T172757--capture-notes__gtd_personal.org")
(defconst mg-conferences-file "~/Vault/pkm/20231210T222135--conferences__personal_research.org")
(defconst mg-personal-projects-file "~/Vault/pkm/20231210T220139--personal-projects__personal_project.org")
(defconst mg-books-file "/home/claudio/Vault/pkm/20240102T104309--books__personal_reading.org")
(defconst mg-planning-file "~/Vault/pkm/20240104T191508--planning__personal_planning.org")
(defconst mg-flashcards-file "~/Vault/pkm/20240220T165813--flashcards__learning_personal.org")
(defconst mg-latex-cmd "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")

;; Required for string-trim function
(require 'subr-x)
(defun mg-extract-heading-name (heading)
  "Extract the heading name, handling text before links, links, and task indicators."
  ;; Remove task progress indicators like [1/1] and trim trailing spaces
  (setq heading (string-trim (replace-regexp-in-string "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]\\s-*" "" heading)))
  ;; Function to extract and concatenate text before the link and the link description
  (let ((start 0) (parts '()))
    (while (string-match "\\(.*?\\)\\(\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]\\)" heading start)
	(push (match-string 1 heading) parts)  ; Text before the link
	(push (match-string 3 heading) parts)  ; Link description
	(setq start (match-end 0)))
    (push (substring heading start) parts)  ; Remaining text after last link
    (string-join (reverse parts) "")))

(require 'mg-emacs-org)

(defun mg-org-get-clock-minutes (file start-date end-date)
  "Get minutes from org-clock of a specific file and date."
  (interactive
   (list
    (read-file-name "Get clock data from FILE: ")
    (format "<%s>" (org-read-date))
    (format "<%s>" (org-read-date))))
  (let ((minutes (nth 1
		        (with-current-buffer (find-file-noselect file)
			  (org-clock-get-table-data file `( :maxlevel 4
							    :tstart ,start-date
							    :tend ,end-date))))))
    minutes))

(provide 'mg-org)
;;; mg-org.el ends here
