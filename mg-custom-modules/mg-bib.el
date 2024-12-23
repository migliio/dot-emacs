;;; mg-bib.el --- Extensions for bibliographic packages -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
;; Version: 0.0.5
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

(require 'mg-org)
(require 'mg-denote)

(defun mg-bib-denote-org-capture-paper-biblio ()
  "Custom org-capture template to add a paper reference."
  (let ((bibtex-input (mg-bib--bibtex-parse-entry
		       (mg-bib--denote-bibtex-prompt))))
    (when-let*
	((key (mg-bib--bibtex-generate-key bibtex-input))
	 (bibtex-list (mg-bib--bibtex-refactor-entry-header bibtex-input key))
    	 (title (mg-bib--bibtex-get-field-content bibtex-list "title"))
    	 (heading (format "* %s %s\n" title 
			  (mg-bib--denote-format-tags-as-org (mg-bib--denote-cycle-through-tags))))
	 (file-path (mg-bib--denote-pull-resource-for-entry key)))
      (concat heading
    	      (mg-bib--denote-bibtex-org-block
	       (mg-bib--bibtex-list-to-string
		(mg-bib--bibtex-append-field bibtex-list "file" (mg-bib--bibtex-format-file-for-field file-path)))
	       file-path)))))

(defun mg-bib--bibtex-format-file-for-field (file-path)
  "Format FILE-PATH to be inserted as file field in the bibtex entry."
  (let ((file-name (car (last (split-string file-path "/")))))
    (format "%s:../pkm/assets/%s:application/pdf" file-name file-name)))

(defun mg-bib--bibtex-generate-key (bibtex-list)
  "Generate a bibtex key for BIBTEX-LIST.
The key format is author-title-year."
  (let ((author (mg-bib--bibtex-get-author-for-key bibtex-list))
	(title (mg-bib--bibtex-get-title-for-key bibtex-list))
	(year (mg-bib--bibtex-get-year-for-key bibtex-list)))
    (format "%s_%s_%s" author title year)))

(defun mg-bib--bibtex-get-author-for-key (bibtex-list)
  "Get the author from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (let ((first-author (string-trim 
		       (car (string-split
			     (downcase (mg-bib--bibtex-get-field-content bibtex-list "author")) "and")))))
    (if (string-match "," first-author)
	(mg-bib--bibtex-sanitize-author-surname-for-key
	 (car (string-split first-author ",")))
      (mg-bib--bibtex-sanitize-author-surname-for-key
       (car (cdr (string-split first-author " ")))))))

(defun mg-bib--bibtex-sanitize-author-surname-for-key (surname)
  "Perform a sanitization over the author's surname to generate
 the bibtex key."
  ;; This is required because sometimes we have "John Jr., Doe". Then,
  ;; producing "john jr." as surname for the key to be generated is
  ;; WRONG."
  (let ((sanitized-surname (car (split-string surname " "))))
    sanitized-surname))

(defun mg-bib--bibtex-get-year-for-key (bibtex-list)
  "Get the author from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (let ((year (mg-bib--bibtex-get-field-content bibtex-list "year"))
	(date (mg-bib--bibtex-get-field-content bibtex-list "date")))
    (cond
     (year year)
     (date (substring date 0 4))
     (t (user-error "Can't retrieve a suitable field for date")))))

(defun mg-bib--bibtex-get-type (bibtex-list)
  "Get the entry type for BIBTEX-LIST."
  (let ((field (car bibtex-list)))
    (when (string-match "^@\\(\\w+\\){\\([^,]+\\)" field)
      (let ((type (downcase (match-string 1 field))))
	type))))

(defun mg-bib--bibtex-refactor-entry-header (bibtex-list key)
  "Refactor BIBTEX-LIST's header to be downcase and with custom
 KEY."
  (let ((type (mg-bib--bibtex-get-type bibtex-list)))
    (push (format "@%s{%s" type key) (cdr bibtex-list))))

(defun mg-bib--bibtex-get-title-for-key (bibtex-list)
  "Get the title from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (when-let* ((title-early (mg-bib--bibtex-get-field-content bibtex-list "title"))
	      ;; NOTE: Sometimes titles have `'`, `:`, etc. signs in
	      ;; them. Let's rule those diacritics out.
	      (title (replace-regexp-in-string "'" "" (replace-regexp-in-string ":" "" title-early))))
    (downcase (car (split-string title " ")))))

(defun mg-bib--denote-bibtex-prompt (&optional default-bibtex)
  "Ask the user for a bibtex entry. Returns the sanitised
          version. See `mg-bib--denote-sanitise-bibtex' for details."
  (let* ((def default-bibtex)
      	 (format (if (and def (not (string-empty-p def)))
      		     (format "Bibtex [%s]: " def)
      		   "Bibtex: "))
      	 (sanitised-bibtex (mg-bib--denote-bibtex-sanitise (read-string format nil nil def))))
    (if sanitised-bibtex
      	sanitised-bibtex
      (user-error "Invalid BiBTeX entry provided to `mg-bib--denote-bibtex-prompt'"))))

(defun mg-bib--denote-bibtex-org-block (bibtex &optional file)
  "Returns a string representing an org `bibtex' source block
          encompassing BIBTEX, a string of a bibtex entry."
  (let* ((src
          (format "#+begin_src bibtex :tangle \"%s\"\n%s\n#+end_src" mg-bibliography-path bibtex))
         (entries (format
          	   ":PROPERTIES:\n:FILE: %s\n:NOTES:\n:END:\n" (mg-denote-generate-link-from-file-path file))))
    (format "%s\n%s\n" entries src)))

(defun mg-bib--bibtex-early-sanitize (bibtex)
  "Perform some early initialization on BIBTEX."
  (replace-regexp-in-string "[ \t]+=" " =" (replace-regexp-in-string "\n[ \t]+" "\n" (replace-regexp-in-string "[ \t]+\n" "\n" bibtex))))

(defun mg-bib--bibtex-parse-entry (bibtex)
  "Parse BIBTEX to make a list.
Each entry is a bibtex field with a value."
  (let* ((bibtex-fields
	  (string-split (mg-bib--bibtex-early-sanitize bibtex) ",$"))
	 (bibtex-list
	  (mapcar (lambda (string)
		    (string-trim (replace-regexp-in-string "\n" " " string)))
		  bibtex-fields))
	 (bibtex-final (reverse bibtex-list))
	 (before-check (nreverse
			(append (list (setcar bibtex-final (replace-regexp-in-string "}.*?}" "}" (car (last bibtex-list)))))
				(cdr bibtex-final)))))
    (if (and before-check (string= "}" (car (last before-check))))
	(butlast before-check)
      before-check)))

(defun mg-bib--bibtex-get-field (bibtex-list field)
  (seq-find (lambda (string)
	      (string-match-p (format "%s.*?=" field) string)) bibtex-list))

(defun mg-bib--bibtex-get-field-content (bibtex-list field)
  (when-let ((raw-field (mg-bib--bibtex-get-field bibtex-list field)))
    (when (string-match (format "%s\\s-*=[[:space:]]*{\\([^}]*\\)}" field) raw-field)
      (match-string 1 raw-field))))

(defun mg-bib--bibtex-list-to-string (bibtex-list)
  "Reformat BIBTEX-LIST as a bibtex entry (string)."
  (mapconcat #'identity bibtex-list ",\n"))

(defun mg-bib--bibtex-append-field (bibtex-list field value)
  "Append FIELD with value VALUE to BIBTEX-LIST."
  (let ((field-formatted (format "%s = {%s}}" field value)))
    (nreverse
     (append (list field-formatted) (nreverse bibtex-list)))))

(defun mg-bib--bibtex-list-find-field-and-replace (bibtex-list field replace)
  "Find and replace a specific field in a BibTeX entry list."
  (mapcar (lambda (entry)
            (if (string-match (format "%s.*?=" field) entry)
                (replace-regexp-in-string 
                 (format "%s.*?= {[^}]*}" field)
                 (format "%s = {%s}" field replace)
                 entry)
              entry))
          bibtex-list))

(defun mg-bib--denote-cycle-through-tags ()
  "Cycle through tags in the references file prompting the user for an input."
  (let* ((tags (mg-org-get-tags-from-file mg-references-file))
	(selected-tags (completing-read-multiple "Select tags: " tags)))
    (sort selected-tags #'string<)))

(defun mg-bib--denote-format-tags-as-org (tags-list)
  "Format TAGS-LIST as a series of org tags."
  (concat ":" (mapconcat #'identity tags-list ":") ":"))

(provide 'mg-bib)
;;; mg-bib.el ends here
