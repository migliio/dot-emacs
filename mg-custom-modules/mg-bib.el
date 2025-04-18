;;; mg-bib.el --- Extensions for bibliographic packages -*- lexical-binding: t -*-

;; Copyright (C) 2025  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://crawlingaway.org/emacs/dot-emacs
;; Version: 0.1.1
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
;; This library introduces some extensions that I use to manage
;; bibliography files. The idea of this whole library is to leverage
;; org-mode to organize my bibliography.

;;; Code:

(require 'mg-org)
(require 'mg-denote)
(require 'org-element)
(require 'xref)

(defun mg-bib-search-add-to-reading-list ()
  "Search for a bibliography entry in the minibuffer, and add it to `mg-reading-list-file'."
  (interactive)
  (when-let* ((key (citar-select-ref))
	      (title (citar-get-value "title" key)))
    (save-excursion
      (with-current-buffer (find-file-noselect mg-reading-list-file)
  	(goto-char (point-max))
  	(beginning-of-line)
  	(insert (format "* TODO %s [cite:@%s]\n" title key))))))

(defun mg-bib-count-references ()
  "Return the number of references in `mg-references-file'."
  (interactive)
  (let ((count
    	 (mg-org-get-number-headings-in-file mg-references-file)))
    (message "The bibliography org file contains %s entries" count)))

(defun mg-bib-denote-org-capture-book-isbn-biblio ()
  "Custom `org-capture' template to add a book reference from a isbn code."
  (let ((isbn (mg-bib--denote-isbn-prompt)))
    (mg-bib-denote-org-capture-paper-biblio isbn)))

(defun mg-bib-denote-org-capture-website-biblio ()
  "Custom `org-capture' template to add a website reference."
  (let* ((url (read-string "URL: "))
    	 (title (mg-bib--www-get-page-title url))
    	 (authors (mg-bib--denote-prompt-authors))
    	 (date (org-read-date nil nil nil "Insert the article date: " nil nil nil))
	 (year (if (string-match "^\\([0-9]\\{4\\}\\)-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
		   (match-string 1 date)
		 (error "Invalid date format when parsing year")))
	 (bibtex (format "@misc{%s,\nauthor = {%s},\ntitle = {%s},\nurl = {%s},\ndate = {%s},\nyear = {%s},\nnote = {[Accessed %s]},\n}"
    			 "0000"
    			 authors
    			 title
    			 url
    			 date
			 year
    			 (format-time-string "%Y-%m-%d")))
	 (bibtex-list (mg-bib--bibtex-parse-entry bibtex)))
    (setq bibtex-list (push (format "@misc{%s"
				    (mg-bib--bibtex-generate-key bibtex-list))
			    (cdr bibtex-list)))
    (when-let* ((title (mg-bib--bibtex-get-field-content bibtex-list "title"))
    		(heading (format "* %s\n" title)))
      (format "%s%s%s" heading
    	      (mg-bib--denote-bibtex-org-block
	       (mg-bib--bibtex-list-to-string bibtex-list))
	      "- Overview :: \n- Findings :: \n- Significance :: \n"))))

(defun mg-bib-denote-org-capture-paper-biblio (&optional isbn)
  "Custom `org-capture' template to add a paper/book reference."
  (let ((bibtex-input
         (if isbn
             (mg-bib--bibtex-parse-entry
	      (mg-bib--isbn-to-bibtex isbn))
           (mg-bib--bibtex-parse-entry
            (mg-bib--denote-bibtex-prompt)))))
    (when (not bibtex-input)
      (user-error "Couldn't retrieve the bibtex entry."))
    (when-let*
	((key (mg-bib--bibtex-generate-key bibtex-input))
	 (bibtex-list (mg-bib--bibtex-refactor-entry-header bibtex-input key))
    	 (title (mg-bib--bibtex-get-field-content bibtex-list "title"))
    	 (heading (format "* %s\n" title))
	 (file-path (mg-bib--denote-pull-resource-for-entry key)))
      (format "%s%s%s" heading
    	      (mg-bib--denote-bibtex-org-block
	       (mg-bib--bibtex-list-to-string
		(mg-bib--bibtex-append-field bibtex-list "file" (mg-bib--bibtex-format-file-for-field file-path)))
	       file-path)
	      "- Overview :: \n- Findings :: \n- Significance :: \n"))))

(defun mg-bib--denote-identifier-from-attrs (file)
  "Get the creation date of FILE from its attributes.
  Returns the creation date timestamp, otherwise nil."
  (let ((file-attrs (file-attributes file)))
    (if file-attrs
        (let ((creation-time (nth 6 file-attrs)))
          (if creation-time
              (format-time-string denote-id-format creation-time)
  	    nil)))))

(defun mg-bib--denote-remove-identifier-from-key (key)
  "Remove the identifier part from KEY, as its structure is timestamp_author_title_year."
  (let ((parsed-key (string-split key "_" t nil)))
    (string-join (delq (car parsed-key) parsed-key) "_")))

(defun mg-bib--denote-pull-resource-for-entry (key)
  "Prompt the user for file path of paper having key KEY, format the
file name and move it in the `denote-directory'.

KEY has the form timestamp_author_title_year, therefore this
function rules out the timestamp, which is not needed to create
the unique filename, as it's inserted by `denote', and uses the
author, title and year only to rename the resource file."
  (let* ((file-path (read-file-name "Select a PDF file: "))
    	 (file-exists (file-exists-p file-path))
    	 (is-pdf (string-match-p "\\.pdf$" file-path)))
    (cond
     ((not file-exists)
      (user-error "Error: File does not exist."))
     ((not is-pdf)
      (user-error "Error: Selected file is not a PDF.")))
    (setq key (mg-bib--denote-remove-identifier-from-key key))
    (let* ((keywords (denote-keywords-prompt))
      	   (identifier (mg-bib--denote-identifier-from-attrs file-path))
      	   (new-file-name (format "%s--%s__%s" identifier key
      				  (mapconcat #'identity 
      					     (delete-dups (copy-sequence keywords))
      					     "_")))
      	   (new-file-path (format "%s/assets/%s.pdf" (denote-directory) new-file-name)))
      (rename-file file-path new-file-path)
      new-file-path)))

(defun mg-bib--bibtex-format-file-for-field (file-path)
  "Format FILE-PATH to be inserted as file field in the bibtex entry."
  (let ((file-name (car (last (split-string file-path "/")))))
    (format "%s:../pkm/assets/%s:application/pdf" file-name file-name)))

(defun mg-bib--bibtex-generate-key (bibtex-list)
  "Generate a bibtex key for BIBTEX-LIST.
The key format is timestamp-author-title-year."
  (let ((author (or (mg-bib--bibtex-get-author-for-key bibtex-list) "noauthor"))
	(title (or (mg-bib--bibtex-get-title-for-key bibtex-list) ""))
	(year (or (mg-bib--bibtex-get-year-for-key bibtex-list) "nodate"))
	(timestamp (format-time-string denote-id-format)))
    (format "%s_%s_%s_%s" timestamp author title year)))

(defun mg-bib--bibtex-get-author-for-key (bibtex-list)
  "Get the author from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (let* ((first-author (string-trim
			(car (string-split
			      (downcase (mg-bib--bibtex-get-field-content bibtex-list "author")) "and"))))
	 (author-tag
	  (if (string-match "," first-author)
	      (car (string-split first-author ","))
	    (car (cdr (string-split first-author " "))))))
    ;; NOTE: In case there was no way of getting the surname, just
    ;; grab the whole author string.
    (if author-tag
	(mg-bib--bibtex-sanitize-author-for-key author-tag)
      (mg-bib--bibtex-sanitize-author-for-key first-author))))

(defun mg-bib--bibtex-sanitize-author-for-key (surname)
  "Perform a sanitization over the author's surname to generate
 the bibtex key."
  ;; This is required because sometimes we have "John Jr., Doe". Then,
  ;; producing "john jr." as surname for the key to be generated is
  ;; WRONG. Moreover, sometimes there are "-" signs in author keys,
  ;; e.g., Anthropic-AI. Therefore, remove those.
  (let ((sanitized-surname (car (split-string surname " "))))
    (replace-regexp-in-string "-" "" sanitized-surname)))

(defun mg-bib--bibtex-get-year-for-key (bibtex-list)
  "Extract year from BIBTEX-LIST entry for use in bibtex key.
Looks first in the 'year' field, then in the 'date' field.
Returns the first 4-digit year (between 1000-2999) found or signals an error."
  (let* ((year (mg-bib--bibtex-get-field-content bibtex-list "year"))
         (date (mg-bib--bibtex-get-field-content bibtex-list "date"))
         (year-regexp "\\b\\(1[0-9][0-9][0-9]\\|2[0-9][0-9][0-9]\\)\\b"))
    (cond
     ((and year
           (string-match year-regexp year))
      (match-string 1 year))
     ((and date
           (string-match year-regexp date))
      (match-string 1 date))
     (t
      (user-error "No valid year found in 'year' or 'date' fields")))))

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

(defun mg-bib--bibtex-sanitize-title-for-key (title)
  "Sanitize TITLE string for use in a bibtex citation key.
Removes common punctuation marks and special characters."
  (let* ((chars-to-remove '(":" "/" "." "'" "-" "," "?" "!" ";" "&" "(" ")" "[" "]"))
         (case-fold-search nil)
         (sanitized title))
    (dolist (char chars-to-remove)
      (setq sanitized (replace-regexp-in-string (regexp-quote char) " " sanitized)))
    sanitized))

(defconst mg-bib--title-keys-to-rule-out '("a" "the" "an" "for" "on")
  "List of some title keys I'd prefer not to use.")

(defun mg-bib--bibtex-get-title-for-key (bibtex-list)
  "Get the title from BIBTEX-LIST, formatted and ready to be used
in the bibtex key."
  (when-let* ((title-early (mg-bib--bibtex-get-field-content bibtex-list "title"))
	      ;; NOTE: Sometimes titles have `'`, `:`, etc. signs in
	      ;; them. Let's rule those diacritics out.
	      (title (mg-bib--bibtex-sanitize-title-for-key title-early))
	      (first (downcase (car (split-string title " ")))))
    ;; NOTE: titles have articles, clauses, etc. I'd like to rule them
    ;; out and have more meaningful keys.
    (when (member first mg-bib--title-keys-to-rule-out)
      (setq first (downcase (car (cdr (split-string title " "))))))
    first))

(defun mg-bib--denote-bibtex-prompt (&optional default-bibtex)
  "Ask the user for a bibtex entry. Returns the sanitised
          version. See `mg-bib--denote-sanitise-bibtex' for details."
  (let* ((def default-bibtex)
      	 (format (if (and def (not (string-empty-p def)))
      		     (format "Bibtex [%s]: " def)
      		   "Bibtex: "))
      	 (sanitised-bibtex (read-string format nil nil def)))
    (if sanitised-bibtex
      	sanitised-bibtex
      (user-error "Invalid BiBTeX entry provided to `mg-bib--denote-bibtex-prompt'"))))

(defun mg-bib--denote-isbn-prompt ()
  "Ask the user for a isbn entry."
  (let ((isbn (read-string "ISBN: ")))
    isbn))

(defun mg-bib--denote-bibtex-org-block (bibtex &optional file)
  "Returns a string representing an org `bibtex' source block
          encompassing BIBTEX, a string of a bibtex entry."
  (let* ((src
          (format "#+begin_src bibtex :tangle \"%s\"\n%s\n#+end_src" mg-bibliography-path bibtex))
	 (keywords (mg-bib--denote-cycle-through-keywords))
         (entries (format
          	   ":PROPERTIES:\n:FILE: %s\n:NOTES:\n:KEYWORDS: %s\n:END:\n"
		   (if file (mg-denote-generate-link-from-file-path file) "")
		   (if keywords (mg-bib--denote-format-keywords-from-properties keywords) "")
		   )))
    (format "%s\n%s\n" entries src)))

(defun mg-bib--bibtex-early-sanitize (bibtex)
  "Perform some early initialization on BIBTEX."
  (replace-regexp-in-string "[ \t]+=" " =" (replace-regexp-in-string "\n[ \t]+" "\n" (replace-regexp-in-string "[ \t]+\n" "\n" bibtex))))

(defun mg-bib--denote-prompt-authors ()
  "Get authors from user, possibly using autocomplete."
  (let ((entries (parsebib-parse mg-bibliography-path :fields (list "author")))
        authors)
    (maphash (lambda (key entry)
               (let ((author (assoc "author" entry)))
		 (when author
                   (push (string-split (cdr author) " and " t nil) authors))))
             entries)
    (if-let ((candidates
	      (delq 'nil
		    (mapcar (lambda (entry)
			      (if (string-match-p "," entry)
				  (let ((parsed (string-split entry ", " t nil)))
				    (format "%s %s" (cadr parsed) (car parsed)))
				entry))
			    (flatten-list authors)))))
	(car (completing-read-multiple "Select authors: " candidates))
      (user-error "Can't retrieve suggestions for authors"))))

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
	      (string-match-p (format "%s.*?=" field) string))
	    bibtex-list))

(defun mg-bib--bibtex-get-field-content (bibtex-list field)
  "Get the content for FIELD in parsed bibtex entry BIBTEX-LIST.

This function rules out all { and } signs from the field and gets
the string after the equal sign. This is the less convoluted way
of correctly parsing a bibtex field's content."
  (when-let ((raw-field (mg-bib--bibtex-get-field bibtex-list field)))
    (setq raw-field (replace-regexp-in-string "{" "" raw-field))
    (setq raw-field (replace-regexp-in-string "}" "" raw-field))
    (when (string-match (format "%s\\s-*=[[:space:]]*\\([^}]*\\)" field) raw-field)
      (match-string 1 raw-field))))

(defun mg-bib--bibtex-list-to-string (bibtex-list)
  "Reformat BIBTEX-LIST as a bibtex entry (string)."
  (let ((closed-list
	 (mg-bib--bibtex-close-entry bibtex-list)))
    (mapconcat #'identity closed-list ",\n")))

(defun mg-bib--bibtex-close-entry (bibtex-list)
  "Close the bibtex entry represented by BIBTEX-LIST."
  (nreverse
     (append (list "}") (nreverse bibtex-list))))

(defun mg-bib--bibtex-append-field (bibtex-list field value)
  "Append FIELD with value VALUE to BIBTEX-LIST."
  (let ((field-formatted (format "%s = {%s}" field value)))
    (add-to-list 'bibtex-list field-formatted :append nil)))

(defun mg-bib--bibtex-list-find-field-and-replace (bibtex-list field replace)
  "Find and replace a specific field in a bibtex entry list."
  (mapcar (lambda (entry)
            (if (string-match (format "%s.*?=" field) entry)
                (replace-regexp-in-string 
                 (format "%s.*?= {[^}]*}" field)
                 (format "%s = {%s}" field replace)
                 entry)
              entry))
          bibtex-list))

(defun mg-bib--denote-cycle-through-keywords ()
  "Cycle through keywords in the references file prompting the user for an input."
  (let* ((keywords 
	  (delete-dups (flatten-list (mapcar (lambda (entry) (string-split (cdr entry) ";" t nil))
					     (mg-bib--get-keywords-from-file mg-references-file)))))
	 (selected-keywords (completing-read-multiple "Select keywords: " keywords)))
    (sort selected-keywords #'string<)))

(defun mg-bib--field-empty-p (entry)
  "Return t if ENTRY contains only empty braces (i.e. '{}'), nil otherwise.
ENTRY should be a string containing a bibtex field value."
  (string-match-p "^[[:space:]]*{[[:space:]]*}[[:space:]]*$" entry))

(defun mg-bib--bibtex-from-isbn-sanitize (bibtex)
  "Sanitize BIBTEX, assuming that it has been generated by a ISBN."
  (when (mg-bib--field-empty-p bibtex)
    (kill-new bibtex)
    (user-error "The bibtex entry is malformed, it has been copied to the killring.")))

(defun mg-bib--isbn-to-bibtex (isbn)
  "Given ISBN for a book, generate a bibtex entry.
This code is adapted from the one developed by John Kitchin for org-ref."
  (let* ((url (format "https://openlibrary.org/isbn/%s.json" isbn))
         (json (with-current-buffer (url-retrieve-synchronously url)
                 (json-read-from-string (string-trim (buffer-substring url-http-end-of-headers (point-max))))))
	 (title (cdr (assoc 'title json)))
	 (publishers-list (cdr (assoc 'publishers json)))
	 (publisher (mapconcat #'identity publishers-list ", "))
	 (date (cdr (assoc 'publish_date json)))
	 (author-urls (cdr (assoc 'authors json)))
	 (authors (mapconcat
		   #'identity
		   (cl-loop for aurl across author-urls
			    collect
			    (with-current-buffer (url-retrieve-synchronously
						  (format "https://openlibrary.org%s.json"
							  (cdr (assoc 'key aurl))))
			      (cdr (assoc 'personal_name
					  (json-read-from-string
					   (string-trim (buffer-substring url-http-end-of-headers (point-max))))))))
		   " and "))
	 (burl (format "https://openlibrary.org/%s" (cdr (assoc 'key json))))
	 (bibtex (format "@book{,
  author = {%s},
  title = {%s},
  publisher = {%s},
  date = {%s},
  url = {%s}
}"
			 authors
			 title
			 publisher
			 date
			 burl)))
    bibtex))

(defun mg-bib-kill-bibtex-from-isbn ()
  "Prompt a ISBN interactively and kill the generated bibtex entry."
  (interactive)
  (when-let ((bibtex (mg-bib--isbn-to-bibtex
		      (mg-bib--denote-isbn-prompt))))
    (kill-new bibtex)))

(defun mg-bib--www-get-page-title (url)
  "Get the page title from URL."
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1)))
    title))

(defun mg-bib--denote-prompt-and-return-point ()
  "Prompt the user for org headings in `mg-references-file' and return the point related to the notes part of the selected heading."
  (save-excursion
    (with-current-buffer (find-file-noselect mg-references-file)
      (let* ((current-pos (point))
	     (headings (org-map-entries
			(lambda ()
			  (cons (org-get-heading t t t t)
				(point)))
			t 'file))
	     (headings-names (mapcar #'car headings))
	     (selected (completing-read "Select heading: " headings-names nil t))
	     (heading-pos (cdr (assoc selected headings))))
	(if (not heading-pos)
	    (error "Heading not found")
	  (goto-char heading-pos)
	  (let ((end-of-subtree (save-excursion
				  (org-end-of-subtree t t))))
            (if (re-search-forward "^[ \t]*#\\+end_src" end-of-subtree t)
		(progn
                  (end-of-line)
                  (let ((target-pos (point)))
		    target-pos))
	      (error "No source block found in the selected heading"))))))))

(defun mg-bib-denote-goto-notes-interactively ()
  "Prompt the user for headings in `mg-references-file' and go to the notes section of the selected heading. The subtree is then narrowed for convenience."
  (interactive)
  (let ((point (mg-bib--denote-prompt-and-return-point)))
    (with-current-buffer (find-file mg-references-file)
      (goto-char point)
      (org-reveal)
      (org-narrow-to-subtree))))

(defun mg-bib--get-keywords-from-file (file-path)
  "Return a list with all keywords in the KEYWORDS field of the
 properties drawer found in FILE-PATH.

Tags are returned as a single string, where each tag is separated
 by a ';' sign from the other tag."
    (with-current-buffer (find-file-noselect file-path)
      (org-element-map (org-element-parse-buffer) 'headline
	(lambda (headline)
          (save-excursion
            (goto-char (org-element-property :begin headline))
            (let ((keywords (org-entry-get (point) "KEYWORDS")))
              (when keywords
		(cons (org-element-property :raw-value headline)
                      keywords))))))))

(defun mg-bib--denote-format-keywords-from-properties (keywords)
  "Format KEYWORDS to be ready to be inserted in the properties
 drawer."
  (string-join (mapcar (lambda (keyword)
			 (replace-regexp-in-string " " "_" (downcase keyword)))
		       keywords) ";"))

(defun mg-bib--org-bibtex-xref-item (heading-title pos author-match)
  "Create an xref item for a heading.
HEADING-TITLE is the heading’s title. POS is the beginning position of
the heading. AUTHOR-MATCH is a snippet from the author field."
  (let* ((line-num (line-number-at-pos pos))
         (location (xref-make-file-location
                    (or (buffer-file-name) (buffer-name))
                    line-num 0))
	 (author-label (format "(authors: %s)" author-match))
	 (display (format "%s  %s" heading-title (propertize author-label 'face 'shadow))))
    (xref-make display location)))

(defun mg-bib--org-bibtex-find-author-definitions (identifier)
  "Search org headings that contain bibtex babel blocks for an author matching IDENTIFIER.
For every heading that has a babel block in which an `author` field contains IDENTIFIER,
return an xref item whose display text includes the heading title.
Assumes that the heading text is the reference title."
  (let ((results nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (let* ((heading-title (org-element-property :raw-value hl))
               (begin (org-element-property :begin hl))
               (bibtex-blocks (org-element-map hl 'src-block
                                  (lambda (src)
                                    (when (string-equal (org-element-property :language src) "bibtex")
                                      (org-element-property :value src)))
                                  nil nil)))  ;; Note: using nil instead of t to ensure a list is returned.
          (when bibtex-blocks
            (dolist (code bibtex-blocks)
              (when (string-match
                     (concat "author[ \t]*=[ \t]*[{\\\"]\\([^}\"]*"
                             (regexp-quote identifier)
                             "[^}\"]*\\)[}\\\"]")
                     code)
                (let ((author-match (match-string 1 code)))
                  (push (mg-bib--org-bibtex-xref-item heading-title begin author-match)
                        results)))))))
      nil t)
    (nreverse results)))

(defun mg-bib-xref-bibtex-backend ()
  "A simple xref backend for author definitions in org-babel bibtex blocks."
  'bibtex-authors)

(cl-defmethod xref-backend-definitions ((_backend (eql bibtex-authors)) identifier)
  "Return xref locations for the given IDENTIFIER.
Identifies the author field in a bibtex block which matches IDENTIFIER."
  (mg-bib--org-bibtex-find-author-definitions identifier))

(add-hook 'xref-backend-functions #'mg-bib-xref-bibtex-backend)

(defun mg-bib--xref-show-org-bibtex-author-matches (identifier)
  "Display an xref buffer of org headings (representing bibtex entries) where
the author of the bibtex block field matches IDENTIFIER.
The heading title is used as the reference title."
  (let ((matches (mg-bib--org-bibtex-find-author-definitions identifier)))
    (if matches
        (xref--show-xrefs matches nil)
      (message "No matches found for '%s'" identifier))))

(defun mg-bib-find-references-for-author (author)
  "Find authors occurrences in `mg-references-file' through `xref'."
  (interactive
   (list
    (read-string "Author: ")))
  (with-current-buffer (find-file-noselect mg-references-file)
    (mg-bib--xref-show-org-bibtex-author-matches author)))

(provide 'mg-bib)
;;; mg-bib.el ends here
