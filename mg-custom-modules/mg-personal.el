;;; mg-personal.el --- Personal environment customization -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

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
;; This library introduces some personal information I use to then
;; build documents, files, and knowledge-base related artifacts,
;; dynamically. Some of these utilities are related to my research
;; work.

;;; Code:

(defconst mg-personal-research-start-year 2023
  "A variable used to record the year I began conducting research.")

(defun mg-personal--get-raw-keywords-from-metanote ()
  "Get research areas I worked on from `mg-research-file'."
  (let ((research-areas '()))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Active research projects" nil t)
        (org-map-entries
         (lambda ()
           (let ((areas (org-entry-get (point) "RESEARCH_AREAS")))
             (when areas
               (push areas research-areas))))
         nil
         'tree))
      (nreverse research-areas))))

(defun mg-personal-get-research-areas ()
  "Get research areas I worked on, as a list of strings, based on produced artifacts."
  (let* ((raw-keywords
  	  (mg-personal--get-raw-keywords-from-metanote))
  	 (formatted-keywords (delete-dups
  			      (flatten-list
  			       (mapcar
  				(lambda (keywords)
  				  (let ((formatted-keywords
  					 (replace-regexp-in-string "_" " " keywords)))
  				    (string-split formatted-keywords ";" t nil)))
  				raw-keywords)))))
    (sort (mapcar (lambda (keyword)
  		    (capitalize keyword))
  		  formatted-keywords)
  	  #'string<)))

(provide 'mg-personal)
;;; mg-personal.el ends here
