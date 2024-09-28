;;; mg-macos.el --- Enable macos support -*- lexical-binding: t -*-

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
;; This source file just enables some options to make Emacs smooth on
;; `darwin' as well. Nothing more.

;;; Code:

(defun mg-macos-support-enable ()
  "This function simply turns some options on, for me to have a
 smooth experience with Emacs even on `darwin'."
  (progn
    (setenv "LANG" "en_US.UTF-8")
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (setq ns-use-proxy-icon nil
	    mac-option-modifier 'meta
	    frame-title-format nil
	    mac-frame-tabbing nil)))

(provide 'mg-macos)
;;; mg-macos.el ends here
