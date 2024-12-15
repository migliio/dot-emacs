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

(require 'mg-org)

(defun mg-bib-denote-org-capture-paper-biblio ()
  "Custom org-capture template to add a paper reference."
  (let* ((old-bibtex (mg-bib--denote-bibtex-early-sanitize
    		      (mg-bib--denote-bibtex-prompt)))
    	 (old-key (mg-bib--denote-bibtex-key old-bibtex))
    	 (key (mg-bib--reformat-bib-key old-bibtex))
    	 (title (mg-bib--denote-bibtex-title old-bibtex))
    	 (bibtex (replace-regexp-in-string old-key key old-bibtex))
    	 (heading (format "* %s [cite:@%s]\n" title key)))
    (concat heading
    	    (mg-bib--denote-bibtex-org-block (mg-bib--add-bibtex-file-field bibtex)
					     (mg-bib--denote-pull-resource-for-entry key)))))

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
    	 (bibtex (replace-regexp-in-string "0000" key old-bibtex))
    	 (heading (format "* %s [cite:@%s]\n" title key))
    	 (keywords (mg-bib--keywords-prompt)))
    (concat heading (mg-bib--denote-bibtex-org-block bibtex))))

(defun mg-bib-search-add-to-reading-list ()
  "Search for a bibliography entry in the minibuffer, and add it to `mg-reading-list-file'."
  (interactive)
  (let ((key (citar-select-ref)))
    (save-excursion
      (with-current-buffer (find-file-noselect mg-reading-list-file)
        (goto-char (point-max))
        (beginning-of-line)
        (insert (format "* TODO %s [cite:@%s]\n"
        		(citar-get-value "title" key)
        		key))))))

(defun mg-bib--denote-bibtex-early-sanitize (bibtex)
  "Perform early sanitization on BIBTEX."
  (replace-regexp-in-string "\nand " " and " (replace-regexp-in-string "[ \t]+=" " =" (replace-regexp-in-string "\n[ \t]+" "\n" bibtex))))

(defun mg-bib-count-references ()
  "Return the number of references in `mg-references-file'."
  (interactive)
  (let ((count 
    	 (mg-org-get-number-headings-in-file mg-references-file)))
    (message "The bibliography org file contains %s entries" count)))

;; TODO
(defun mg-bib--keywords-prompt ()
  "Prompt the user for a series of keywords."
  (while-let
      ((keywords (completing-read "Insert keywords: "
      				  (find-file-noselect mg-bibliography-path
      						      (org-map-entries (lambda (entry)
      									 (org-get-tags)))))))))

(defun mg-bib--normalize-bibtex-title (title)
  "Normalize a BibTeX title by removing line breaks and extra whitespaces."
  (when title
    (replace-regexp-in-string 
     "\\s-+" " "  ; Replace multiple whitespace characters with a single space
     (replace-regexp-in-string 
      "\n" " "   ; Replace newlines with spaces
      (string-trim title)))))

(defun mg-bib--denote-bibtex-title (bibtex)
  "Returns the bibtex title from BIBTEX."
  (when (string-match "\\s *title\\s *=\\s *{\\(\\(?:.\\|\n\\)*?\\)}," bibtex)
    (mg-bib--normalize-bibtex-title (match-string-no-properties 1 bibtex))))

(defun mg-bib--add-bibtex-file-field (bibtex-entry file-path)
  "Add a file field with path FILE-PATH to the end of
     BIBTEX-ENTRY."
  (mg-bib--add-bibtex-field bibtex-entry
    			    (format "file = {%s:../pkm/assets/%s:application/pdf}" file-path file-path)))

(defun mg-bib--normalize-bibtex-entry (bibtex)
  "Normalize BIBTEX entry by downcasing the type, removing
 newlines, and fixing spacing."
  (replace-regexp-in-string "\n[ \t]*," "," (replace-regexp-in-string "\n+" " " (replace-regexp-in-string ",file" "file" bibtex))))

(defun mg-bib--add-bibtex-field (bibtex-entry field)
  "Add FIELD to the end of BIBTEX-ENTRY."
  (let* ((entry-without-closing (replace-regexp-in-string "}\s*$" "" bibtex-entry))
    	 (file-field field))
    (mg-bib--normalize-bibtex-entry
     (concat entry-without-closing "," file-field "\n}"))))

(defun mg-bib--denote-bibtex-key (bibtex)
  "Returns the bibtex key from BIBTEX."
  (when (string-match "@.*{\\(.*\\)," bibtex)
    (match-string-no-properties 1 bibtex)))

(defun mg-bib--denote-bibtex-year (bibtex)
  "Returns the year from BIBTEX, handling various possible formats."
  (let ((year-match-1 (string-match "\\s *year\\s *=\\s *\\([0-9]+\\)" bibtex))
        (date-match-1 (string-match "\\s *date\\s *=\\s *{?\\([0-9]+\\)" bibtex)))
    (cond
     (year-match-1 (match-string-no-properties 1 bibtex))
     (date-match-1 (match-string-no-properties 1 bibtex))
     ((string-match "\\s *year\\s *=\\s *[\"'{]\\([0-9]+\\)" bibtex)
      (match-string-no-properties 1 bibtex))
     (t (user-error "Can't find a year or date field in the bibtex entry")))))

(defun mg-bib--denote-bibtex-author (bibtex)
  "Extracts the author field from a BibTeX entry, handling multiline formatting and different delimiters."
  (when (string-match "author\\s *=\\s *[{\"]\\([^}\"]*\\)[}\"]" bibtex)
    (let ((authors (match-string-no-properties 1 bibtex)))
      (when (string-match "\\\"{\\([^}]+\\)}" authors)
	(let ((char (match-string 1)))
    (replace-match char))))
      (replace-regexp-in-string "\\s-+" " " authors)))

(defun mg-bib--denote-bibtex-get-author-surname (authors)
  "Get the first author's surname starting from the string of
       authors AUTHORS."
  ;; NOTE: When the authors string has a ",", it means that the format
  ;; is <surname>,<name>. Therefore, we must take it into account."
  (let ((surname 
    	 (if (string-match-p "\\," authors)
    	     (nth 0 (string-split authors))
    	   (nth 1 (string-split authors)))))
    (mg-bib--denote-reformat-entry surname)))

(defun mg-bib--denote-reformat-entry (entry)
  "Reformat BibTeX entry ENTRY to later generate a key from it."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "[^a-z0-9]" ""
     (downcase (car (string-split entry))))))

(defun mg-bib--reformat-bib-key (bibtex)
  "Reformat the bibtex key for entry BIBTEX."
  (let* ((title (mg-bib--denote-reformat-entry (mg-bib--denote-bibtex-title bibtex)))
         (author (mg-bib--denote-bibtex-get-author-surname (mg-bib--denote-bibtex-author bibtex)))
         (year (mg-bib--denote-bibtex-year bibtex))
         (new-key (format "%s_%s_%s" author title year)))
    new-key))

(defun mg-bib--denote-identifier-from-attrs (file)
  "Get the creation date of FILE from its attributes.
  Returns the creation date timestamp, otherwise nil."
  (let ((file-attrs (file-attributes file)))
    (if file-attrs
        (let ((creation-time (nth 6 file-attrs)))
          (if creation-time
              (format-time-string denote-id-format creation-time)
  	    nil)))))

(defun mg-bib--denote-pull-resource-for-entry (key)
  "Prompt the user for file path of paper having key KEY, format
       the file name and move it in the `denote-directory'."
  (let* ((file-path (read-file-name "Select a PDF file: "))
    	 (file-exists (file-exists-p file-path))
    	 (is-pdf (string-match-p "\\.pdf$" file-path)))
    (cond
     ((not file-exists)
      (user-error "Error: File does not exist."))
     ((not is-pdf)
      (user-error "Error: Selected file is not a PDF.")))
    (let* ((keywords (denote-keywords-prompt))
      	   (identifier (mg-bib--denote-identifier-from-attrs file-path))
      	   (new-file-name (format "%s--%s__%s" identifier key
      				  (mapconcat #'identity 
      					     (delete-dups (copy-sequence keywords))
      					     "_")))
      	   (new-file-path (format "%s/assets/%s.pdf" (denote-directory) new-file-name)))
      (rename-file file-path new-file-path)
      new-file-path)))

(defun mg-bib--denote-bibtex-org-block (bibtex &optional file)
  "Returns a string representing an org `bibtex' source block
        encompassing BIBTEX, a string of a bibtex entry."
  (let* ((src
          (format "#+begin_src bibtex :tangle \"%s\"\n%s\n#+end_src" mg-bibliography-path bibtex))
         (entries (format
        	   ":PROPERTIES:\n:FILE: [[%s]]\n:NOTES:\n:END:\n" file)))
    (format "%s\n%s\n" entries src)))

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

(defun mg-bib--denote-bibtex-sanitise (bibtex)
  "Returns a santised version of BIBTEX. Sanitisation entails remove
        all non alpha-numeric characters from the bibtex key, and
         returning this updated bibtex entry. If BIBTEX is not a valid
         bibtex entry, returns nil."
  (when (string-match "@.*{\\(.*\\)," bibtex)
    (let* ((key (match-string-no-properties 1 bibtex))
    	   (sanitised-key (replace-regexp-in-string "[^A-Za-z0-9]" "" key)))
      (replace-regexp-in-string key sanitised-key bibtex))))

;; TODO and to double check (may be buggy)
(defun mg-bib--retrieve-keywords-from-bib-file (&optional file)
  "Retrieve keywords from my bibliography file, or, if specified, from FILE."
  (let* ((entries 
      	  (org-map-entries (lambda ()
      			     (when-let
      				 ((keywords (org-get-tags)))
      			       (list
      				:keywords keywords
      				:title (nth 4 (org-heading-components))
      				:line (line-number-at-pos))))))
      	 (user-choice
      	  (completing-read "Choose a keyword you want to filter: "
      			   (delete-dups (flatten-list
      					 (mapcar (lambda (entry)
      						   (string-split
      						    (string-trim (plist-get entry :keywords)) ", "))
      						 entries)))))
      	 (filtered-entries
      	  (seq-filter (lambda (entry)
      			(string-match-p user-choice (plist-get entry :keywords)))
      		      entries)))
    ))

(defun mg-bib--www-get-page-title (url)
  "Get the page title from URL."
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1)))
    title))

(provide 'mg-bib)
;;; mg-bib.el ends here
