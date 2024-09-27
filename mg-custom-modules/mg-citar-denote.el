;;; mg-citar-denote.el --- citar-denote custom extensions -*- lexical-binding: t -*-

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
;; This source file contains several extensions I wrote for `citar-denote'. I use this package extensively, and fits my workflow as a researcher perfectly.

;;; Code:

(defun mg-open-resources-from-denote-file (file)
  "This function opens the `citar-denote' minibuffer to select
 the sources associated to the citekey extracted from FILE."
  (interactive
   (list
    (if (denote-file-is-note-p (buffer-file-name))
	(buffer-file-name)
      (user-error "Current buffer does not point to a 'denote' file"))))
  (when (not (string-empty-p file))
    (let ((cite-keys
	   (citar-denote--retrieve-references file)))
      (citar-open cite-keys))))
