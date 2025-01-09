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

(defun mg-personal-get-research-areas ()
  "Get research areas I worked on, as a list of strings, based on produced artifacts."
  ;; NOTE: preliminary implementation. It is currently a
  ;; work-in-progress
  (mapcar (lambda (area)
	    (capitalize area))
	  '("kernel allocators" "memory corruption" "kernel exploitation" "kernel development" "compartmentalization")))
;;; mg-personal.el ends here
