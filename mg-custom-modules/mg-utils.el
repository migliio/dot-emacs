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

(defconst denote-directory (expand-file-name mg-pkm-base-directory)
  "This constant keeps track of the denote directory for my entire knowledge base.")

(defconst mg-pkm-assets-directory (expand-file-name "assets/" denote-directory)
  "This constant keeps track of the assets directory for my entire knowledge base.")

(defconst mg-work-projects-file (format "%s/%s" denote-directory "20231210T220334--work-and-study-projects__project_work.org")
  "This constant keeps track of the work and project file within my knowledge base.")
(defconst mg-inbox-file (format "%s/%s" denote-directory "20231211T145832--inbox__gtd_personal.org")
  "This constant keeps track of the inbox file within my knowledge base.")
(defconst mg-agenda-file (format "%s/%s" denote-directory "20231210T224321--agenda__personal.org")
  "This constant keeps track of the agenda file within my knowledge base.")
(defconst mg-archive-file (format "%s/%s" denote-directory ".archive/archive.org")
  "This constant keeps track of the archive file within my knowledge base.")
(defconst mg-research-file (format "%s/%s" denote-directory "20231213T175339--research__metanote_planning.org")
  "This constant keeps track of the research metanote file within my knowledge base.")
(defconst mg-capture-notes-file (format "%s/%s" denote-directory "20231213T172757--capture-notes__gtd_personal.org")
  "This constant keeps track of the capture notes file within my knowledge base.")
(defconst mg-conferences-file (format "%s/%s" denote-directory "20231210T222135--conferences__personal_research.org")
  "This constant keeps track of the conferences file within my knowledge base.")
(defconst mg-personal-projects-file (format "%s/%s" denote-directory "20231210T220139--personal-projects__personal_project.org")
  "This constant keeps track of the personal projects file within my knowledge base.")
(defconst mg-books-file (format "%s/%s" denote-directory "20240102T104309--books__personal_reading.org")
  "This constant keeps track of the books file within my knowledge base.")
(defconst mg-planning-file (format "%s/%s" denote-directory "20240104T191508--planning__personal_planning.org")
  "This constant keeps track of the planning file within my knowledge base.")
(defconst mg-flashcards-file (format "%s/%s" denote-directory "20240220T165813--flashcards__learning_personal.org")
  "This constant keeps track of the flashcards file within my knowledge base.")
(defconst mg-reading-list-file (format "%s/%s" denote-directory "20241130T124328--reading-list__metanote_research.org")
  "This constant keeps track of the reading list file within my knowledge base.")
(defconst mg-references-file (format "%s/%s" denote-directory "20241204T111546--references__main_metanote_research.org")
  "This constant keeps track of the references file within my knowledge base.")
(defconst mg-bibliography-path "~/Vault/research/references.bib"
  "This constant keeps track of my references bibtex file.")
(defconst mg-latex-cmds '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
  "This constant keeps track of the latex command I use to export from org-mode.")

(defun mg-get-today-timestamp ()
  "Helper function to get today's timestamp with the abbreviated day name."
  (format-time-string "%Y-%m-%d %a"))

(provide 'mg-utils)
;;; mg-utils.el ends here
