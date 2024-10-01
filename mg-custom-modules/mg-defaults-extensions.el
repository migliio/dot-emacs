;;; mg-emacs-default-extensions.el --- Sane extensions to the defaults for Emacs  -*- lexical-binding: t -*-

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
;; This source file introduces some extensions (e.g., minor/major
;; modes) to the ones I usually enable when using Emacs.

;;; Code:
(define-minor-mode mg-line-numbers-highlight-line-mode
  "This minor mode shows line numbers in relative mode and
highlights the current line. I use it extensively when in coding."
  :initial nil
  (if mg-line-numbers-highlight-line-mode
	(progn
	  (display-line-numbers-mode 1)
	  (hl-line-mode 1)
	  (setq display-line-numbers-type 'relative))
    (progn
	(display-line-numbers-mode 0)
	(hl-line-mode 0))))

(provide 'mg-defaults-extensions)
;;; mg-defaults-extensions.el ends here
