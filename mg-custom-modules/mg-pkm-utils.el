;;; mg-pkm-utils.el --- Utility functions for my pkm -*- lexical-binding: t -*-

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
;; This library introduces several utility functions and commands to
;; manage my "Personal Knowledge Management System". Although the
;; system heavily relies on `org-mode' and `denote', functions in this
;; library tackle general purpose use-cases (i.e., not necessarily
;; related to `org-mode' or `denote').

;;; Code:

(require 'mg-org)

(defun mg-org-compute-deep-work-minutes (start-date end-date)
  "Compute the minutes of deep work from START-DATE to END-DATE.

This function searches into both projects and archive files to
retrive logbooks properties and their related timing."
  (interactive
   (list
    (format "<%s>" (org-read-date))
    (format "<%s>" (org-read-date))))
  (let* ((default-directory (denote-directory))
	 (target-files
	  '(mg-work-projects-file
	    mg-archive-file)))
    (insert (format "%s"
		    (apply '+ (mapcar (lambda (file)
					(mg-org-get-clock-minutes file start-date end-date)) target-files))))))

(defun mg-toggle-pdf-presentation-mode ()
  "Toggle a presentation mode to show PDFs in a clean layout.

  When showing a PDF in docview, the modeline should be hidden, and
  the document itself should be centered, for better visibility."
  (interactive)
  (toggle-frame-fullscreen)
  (if (not (bound-and-true-p hide-mode-line-mode))
	(hide-mode-line-mode 1)
    (hide-mode-line-mode 0))
  (pdf-view-fit-page-to-window))

(provide 'mg-pkm-utils)
;;; mg-pkm-utils.el ends here
