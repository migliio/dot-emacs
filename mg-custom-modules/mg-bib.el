;;; mg-bib.el --- Extensions for bibliographic packages -*- lexical-binding: t -*-

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
;; This library introduces some extensions for packages I use to
;; manage bibliography files.

;;; Code:

(defun mg-bib-search-add-to-reading-list ()
  "Search for a bibliography entry in the minibuffer, and add it to `mg-reading-list-file'."
  (interactive)
  (let ((key (citar-select-ref)))
    (save-excursion
      (with-current-buffer (find-file-noselect mg-reading-list-file)
	(goto-char (point-max))
	(beginning-of-line)
	(insert (format "* TODO %s\n:PROPERTIES:\n:CITEKEY: [cite:@%s]\n:END:\n"
			(citar-get-value "title" key)
			key))))))

(provide 'mg-bib)
;;; mg-bib.el ends here
