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

(defun mg-bib-denote-org-capture-paper-biblio ()
  "Custom `org-capture` template to add a paper reference."
  (let* ((old-bibtex (mg-bib--denote-bibtex-prompt))
         (old-key (mg-bib--denote-bibtex-key old-bibtex))
         (key (mg-bib--reformat-bib-key old-bibtex))
         (title (mg-bib--denote-bibtex-field old-bibtex "title"))
         (bibtex (s-replace-regexp old-key key old-bibtex))
         (heading (format "* %s [cite:@%s]\n" title key)))
    (mg-bib--denote-pull-resource-for-entry key)
    (concat heading (mg-bib--denote-bibtex-org-block
                     (mg-bib--downcase-first-line-bibtex bibtex)))))

(defun mg-bib-denote-org-capture-website-biblio ()
  "Custom `org-capture` template to add a website reference."
  (let* ((url (read-string "URL: "))
         (title (mg-bib--www-get-page-title url))
         (authors (read-string "Insert author(s) (name, surname + \"and\"): "))
         (date (org-read-date nil nil nil "Insert the article date: " nil nil nil))
         (old-bibtex 
	  (format "@misc{%s,\nauthor = {%s},\ntitle = {%s},\nurl = {%s},\ndate = {%s},\nnote = {[Accessed %s]},\n}"
                             "0000"
                             authors
                             title
                             url
                             date
                             (format-time-string "%Y-%m-%d")))
         (key (mg-bib--reformat-bib-key old-bibtex))
         (bibtex (s-replace-regexp "0000" key old-bibtex))
         (heading (format "* %s [cite:@%s]\n" title key)))
    (concat heading (mg-bib--denote-bibtex-org-block bibtex))))


(defun mg-bib--denote-bibtex-field (bibtex field)
  "Extract a specific FIELD (like 'title', 'year', 'author') from a BIBTEX entry."
  (when (string-match (format "\\s *%s\\s *=\\s *{\\(.*\\)}," field) bibtex)
    (match-string-no-properties 1 bibtex)))

(defun mg-bib-search-add-to-reading-list ()
  "Search for a bibliography entry in the minibuffer, and add it to `mg-reading-list-file`."
  (interactive)
  (let ((key (citar-select-ref)))
    (with-current-buffer (find-file-noselect mg-reading-list-file)
      (goto-char (point-max))
      (beginning-of-line)
      (insert (format "* TODO %s [cite:@%s]\n"
                      (citar-get-value "title" key)
                      key)))))

(defun mg-bib--denote-bibtex-author (bibtex)
  "Returns the author field from BIBTEX."
  (or (mg-bib--denote-bibtex-field bibtex "author") "Unknown Author"))

(defun mg-bib--denote-bibtex-get-author-surname (authors)
  "Get the first author's surname starting from the string of authors AUTHORS."
  (let ((surname
         (if (string-match "\\," authors)
             (nth 0 (string-split authors))
           (nth 1 (string-split authors)))))
    (mg-bib--denote-reformat-entry surname)))

(defun mg-bib--denote-reformat-entry (entry)
  "Reformat BibTeX entry ENTRY to later generate a key from it."
  (let ((case-fold-search t))
    (replace-regexp-in-string "[^a-z0-9]" "" (downcase (car (string-split entry))))))

(defun mg-bib--reformat-bib-key (bibtex)
  "Reformat the bibtex key for entry BIBTEX."
  (let* ((title (mg-bib--denote-reformat-entry (mg-bib--denote-bibtex-field bibtex "title")))
         (author (mg-bib--denote-bibtex-get-author-surname (mg-bib--denote-bibtex-author bibtex)))
         (year (mg-bib--denote-bibtex-field bibtex "year"))
         (new-key (format "%s_%s_%s" author title year)))
    new-key))

(defun mg-bib--denote-pull-resource-for-entry (key)
  "Prompt the user for file path of paper having key KEY, format the file name and move it in the `denote-directory`."
  (let* ((file-path (read-file-name "Select a PDF file: "))
         (file-exists (file-exists-p file-path))
         (is-pdf (string-match-p "\\.pdf$" file-path)))
    (unless file-exists
      (user-error "Error: File does not exist."))
    (unless is-pdf
      (user-error "Error: Selected file is not a PDF."))
    (let* ((keywords (denote-keywords-prompt))
           (identifier (denote-create-unique-file-identifier file-path))
           (new-file-name (format "%s--%s__%s"
                                  identifier key
                                  (mapconcat #'identity (delete-dups (copy-sequence keywords)) "_")))
           (new-file-path (format "%s/assets/%s.pdf" (denote-directory) new-file-name)))
      (rename-file file-path new-file-path))))

(defun mg-bib--downcase-first-line-bibtex (bibtex)
  "Downcase the first line of BIBTEX."
  (let ((lines (split-string bibtex "\n")))
    (if lines
        (concat (downcase (car lines)) "\n" (mapconcat 'identity (cdr lines) "\n"))
      bibtex)))

(defun mg-bib--denote-bibtex-org-block (bibtex)
  "Returns a string representing an org `bibtex` source block encompassing BIBTEX."
  (let* ((src (format "#+begin_src bibtex :tangle \"%s\"\n%s\n#+end_src" mg-bibliography-path bibtex))
         (entries ":PROPERTIES:\n:FILE:\n:NOTES:\n:END:\n"))
    (format "%s\n%s\n" entries src)))

(defun mg-bib--denote-bibtex-prompt (&optional default-bibtex)
  "Ask the user for a bibtex entry. Returns the sanitised version."
  (let* ((def default-bibtex)
         (format (if (and def (not (string-empty-p def)))
                     (format "Bibtex [%s]: " def)
                   "Bibtex: "))
         (sanitised-bibtex (mg-bib--denote-bibtex-sanitise (read-string format nil nil def))))
    (if sanitised-bibtex
        sanitised-bibtex
      (user-error "Invalid BiBTeX entry provided to `mg-bib--denote-bibtex-prompt'"))))

(defun mg-bib--denote-bibtex-sanitise (bibtex)
  "Returns a sanitised version of BIBTEX. Sanitisation entails removing all non-alphanumeric characters from the bibtex key."
  (when (string-match "@.*{\\(.*\\)," bibtex)
    (let* ((key (match-string-no-properties 1 bibtex))
           (sanitised-key (replace-regexp-in-string "[^A-Za-z0-9]" "" key)))
      (replace-regexp-in-string key sanitised-key bibtex))))

(defun mg-bib--www-get-page-title (url)
  "Get the page title from URL."
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1)))
    title))

(with-eval-after-load 'org
  (add-to-list 'org-capture-templates
	       '("b" "Bibliography"))
  (add-to-list 'org-capture-templates 
	       '("bp" "Bibliography (paper)" entry (file mg-references-file)
		 #'mg-bib-denote-org-capture-paper-biblio
		 :kill-buffer t
		 :jump-to-captured nil)))

(provide 'mg-bib)
;;; mg-bib.el ends here
