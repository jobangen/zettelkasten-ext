;;; zettelkasten-ext.el --- Functions to extend zettelkasten.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jan Ole Bangen.

;; Author: Jan Ole Bangen <jobangen@gmail.com>

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extensions for zettelkasten.el
;;
;;; Code:
(require 'zettelkasten)
(require 'hydra)

;;; begin: Context filter: defunct right now
(defcustom zettelkasten-context-filter-list '()
  "List of context filter."
  :group 'zettelkasten
  :type 'list)

(defun zettelkasten-context-work-fun (entry)
  (and (not (member "journal" (plist-get entry :collections)))
       (not (member "@Rezept" (plist-get entry :descriptors)))
       (not (member "priv" (plist-get entry :collections)))))

(setq zettelkasten-context-filter-list
      '(("All" . (lambda (entry) t))
        ("Work" . zettelkasten-context-work-fun)))

(defvar zettelkasten-context-filter '("All" . (lambda (entry) t)))


;;;###autoload
(defun zettelkasten-set-context-filter ()
  (interactive)
  (let* ((completions zettelkasten-context-filter-list)
         (filter
          (assoc (completing-read "Filter: " completions) completions)))
    (setq zettelkasten-context-filter filter)))

;;; begin: ephemera
(push '("e" "Zettel ephemera" plain
        (file (lambda ()
                (let ((name (or zettel-capture-filename (read-string "Name: "))))
                  (expand-file-name
                   (concat zettelkasten-zettel-directory "/eph/"
                           (format-time-string "%Y-%m-%d-%H%M-")
                           name
                           ".org")))))
        (function zettelkasten-zettel-template)
        :immediate-finish t
        :jump-to-captured t)
      org-capture-templates)

;;; begin: elfeed
(defun zettelkasten-elfeed-get-feed-title ()
  "Get feed title from elfeed buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Feed: ")
    (replace-regexp-in-string
     "\n\\'" ""
     (s-replace "Feed: " "" (thing-at-point 'line t)))))

(defun zettelkasten-elfeed-get-title ()
  "Get feed title from elfeed buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Title: ")
    (replace-regexp-in-string
     "\n\\'" ""
     (s-replace "Title: " "" (thing-at-point 'line t)))))

;; (defvar zettelkasten-capture-db
;;   (db-make
;;    `(db-hash
;;      :filename ,(format "/home/job/.emacs.d/var/zettelkasten/zettelkasten-capture-db"))))

(defun zettelkasten-elfeed-clean-feedtitle (feedtitle)
  (s-replace-all
   '((": Table of Contents" . "")
     ("tandf: " .  "")
     ("SAGE Publications: " . "")
     ("SAGE Publications Inc: " . "")
     ("SAGE Publications Ltd: " . "")
     ("SAGE Publications Ltd STM: " . "")
     ("SAGE Publications India: " . "")
     ("Wiley: " . ""))
   feedtitle))

;;;###autoload
(defun zettelkasten-elfeed-report ()
  (interactive)
  (let* ((feeds (-flatten
                 (zettelkasten-db-query [:select :distinct [feed] :from capture])))
         (data
          (mapcar
           (lambda (feed)
             `(,(caar (zettelkasten-db-query [:select (funcall count feed)
                                             :from capture :where (= feed $s1)]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "A")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "B")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "C")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "D")]
                                             feed))
               ,(caar (zettelkasten-db-query [:select (funcall count feed)
                                              :from capture
                                              :where (= feed $s1)
                                              :and (= priority "E")]
                                             feed))
               ,(zettelkasten-elfeed-clean-feedtitle feed)))
           feeds))
         (sorted (--sort (cond ((not (= (nth 1 it) (nth 1 other)))
                                (> (nth 1 it) (nth 1 other)))
                               ((not (= (nth 2 it) (nth 2 other)))
                                (> (nth 2 it) (nth 2 other)))
                               ((not (= (nth 3 it) (nth 3 other)))
                                (> (nth 3 it) (nth 3 other)))
                               ((not (= (nth 4 it) (nth 4 other)))
                                (> (nth 4 it) (nth 4 other)))
                               ((not (= (nth 5 it) (nth 5 other)))
                                (> (nth 5 it) (nth 5 other)))
                               (t (< (nth 0 it) (nth 0 other))))
                         data)))
    (switch-to-buffer-other-window "*Info*")
    (erase-buffer)
    (insert "  # | A B C D E | Title\n")
    (insert "-------------------------------------------\n")
    (dolist (entry sorted)
      (insert (format "%s | %s %s %s %s %s | %s\n"
                      (s-pad-left 3 " " (number-to-string (car entry)))
                      (nth 1 entry)
                      (nth 2 entry)
                      (nth 3 entry)
                      (nth 4 entry)
                      (nth 5 entry)
                      (nth 6 entry) ;; title
                      )))
    (goto-char (point-min))))

;;; from scimax, adapted
(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (require 'org-ref)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link elfeed-show-entry))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (entry-id (elfeed-entry-id elfeed-show-entry))
         (entry-link (elfeed-entry-link elfeed-show-entry))
         (entry-id-str (concat (car entry-id)
                               "|"
                               (cdr entry-id)
                               "|"
                               url))
         (bibfile (car org-ref-default-bibliography)))
    (if (string-match "DOI: \\(.*\\)$" content)
        (doi-add-bibtex-entry (match-string 1 content)
                              (ido-completing-read
                               "Bibfile: "
                               (append (f-entries "." (lambda (f)
                                                        (and (not (string-match "#" f))
                                                             (f-ext? f "bib"))))
                                       org-ref-default-bibliography)))
      (let ((dois (org-ref-url-scrape-dois url)))
        (cond
         ;; One doi found. Assume it is what we want.
         ((= 1 (length dois))
          (doi-utils-add-bibtex-entry-from-doi
           (car dois)
           bibfile))
         ;; Multiple DOIs found
         ((> (length dois) 1)
          (ivy-read "Select a DOI" (let ((dois '()))
                                     (with-current-buffer (url-retrieve-synchronously url)
                                       (cl-loop for doi-pattern in org-ref-doi-regexps
                                                do
                                                (goto-char (point-min))
                                                (while (re-search-forward doi-pattern nil t)
                                                  (cl-pushnew
                                                   ;; Cut off the doi, sometimes
                                                   ;; false matches are long.
                                                   (cons (format "%40s | %s"
                                                                 (substring
                                                                  (match-string 1)
                                                                  0 (min
                                                                     (length (match-string 1))
                                                                     40))
                                                                 doi-pattern)
                                                         (match-string 1))
                                                   dois
                                                   :test #'equal)))
                                       (reverse dois)))
                    :action
                    (lambda (x)
                      (let ((doi (cdr x)))

                        (doi-utils-add-bibtex-entry-from-doi
                         doi
                         bibfile)))))))

      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1 nil)
      (find-file bibfile)
      (goto-char (point-max))
      (bibtex-beginning-of-entry)
      (delete-char -1)
      (right-char 1)
      (capitalize-word 1)
      (right-word 1)
      (right-char 1)
      (call-interactively 'kill-line)
      (insert ",")
      (when (search-forward "year" nil t)
        (replace-match "date"))
      (bibtex-beginning-of-entry)
      (when (search-forward "DATE_ADDED" nil t)
        (replace-match "timestamp"))
      (bibtex-clean-entry t)
      (save-buffer))))

;;;###autoload
(defun zettelkasten-elfeed-new-zettel ()
  "Create new Zettel from elfeed entry.
Add row to capture db for feed."
  (interactive)
  (let ((title (zettelkasten-elfeed-get-title))
        (feed (zettelkasten-elfeed-get-feed-title))
        (capture (buffer-substring-no-properties (point-min) (point-max)))
        (priority (completing-read "Priority: " '("A" "B" "C" "D" "E"))))
    (zettelkasten-db-query [:insert :into capture
                                    :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") priority)

    (zettelkasten-new-zettel title "zktb:Article")
    (search-forward "Inhalt")
    (next-line)
    (insert capture)
    (zettelkasten-zettel-add-descriptor)
    (save-buffer)
    (kill-buffer)
    (elfeed-show-next)))

;;;###autoload
(defun zettelkasten-elfeed-new-bib ()
  "Add elfeed entry to bibtex and create zettel.
Add row to capture db for feed."
  (interactive)
  (doi-utils-add-entry-from-elfeed-entry)
  (other-window 1 nil)
  (let ((feed (zettelkasten-elfeed-get-feed-title))
        (priority (completing-read "Priority: " '("A" "B" "C" "D" "E")))
        (capture (buffer-substring-no-properties (point-min) (point-max))))
    (doi-utils-add-entry-from-elfeed-entry)
    (other-window 1 nil)
    (zettelkasten-db-query [:insert :into capture
                            :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") priority)
    (other-window 1 nil)
    (bibtex-completion-edit-notes (list (bibtex-completion-get-key-bibtex)))
    (goto-char (point-min))
    (when (search-forward "zktb:article" nil t)
      (replace-match "zktb:Article"))
    (search-forward "Inhalt")
    (next-line)
    (insert capture)
    (goto-char (point-min))))

;;;###autoload
(defun zettelkasten-elfeed-skip ()
  (interactive)
  (let ((feed (zettelkasten-elfeed-get-feed-title)))
    (zettelkasten-db-query [:insert :into capture
                                    :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") "Z"))
  (elfeed-show-next))

;;; begin: convenience functions
;;;###autoload
(defun zettelkasten-move-to-todays-tasks ()
  (interactive)
  ;; old file
  (goto-char (point-min))
  (search-forward "Today's Tasks" nil t)
  ;; today's file
  (org-journal-new-entry '4)
  (unless (search-forward "Today's Tasks" nil t)
    (progn
      (goto-char (point-max))
      (insert "** Today's Tasks\n:PROPERTIES:\n:CATEGORY: pers\n:END:\n")
      (insert (format-time-string "<%Y-%m-%d>\n"))))
  (job/org-add-tags-today)
  (org-todo "TODO")
  (org-priority ?A)
  (org-narrow-to-subtree)
  (goto-char (point-max))
  ;; old file
  (other-window 1)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (while (search-forward "- [ ] " nil t)
    (let ((substring (thing-at-point 'line)))
      (other-window 1)
      (insert substring)
      (other-window 1)
      (backward-char 4)
      (insert ">")))
  (org-todo "DONE")
  (widen)
  (other-window 1)
  (widen))


;;;###autoload
(defun zettelkasten-yearly-file (&optional year)
  (interactive)
  (let* ((yearp (or (when year (number-to-string year)) (read-string "Year: ")))
         (filepath (concat org-journal-dir yearp ".org")))
    (zettelkasten-db-query [:delete-from nodes
                            :where (= filename $s1)]
                           filepath)
    (zettelkasten-db-query [:delete-from edges
                            :where (= filename $s1)]
                           filepath)
    (when (file-exists-p filepath)
      (delete-file filepath))
    (switch-to-buffer filepath)
    (insert (format "#+TITLE: %s
#+RDF_TYPE: time:DateTimeInterval

* [[zk:%s::time:hasDateTimeDescription::dtd-%s][DTD]]
* DateTimeDescription
:PROPERTIES:
:RDF_TYPE: time:DateTimeDescription
:CUSTOM_ID: dtd-%s
:END:
- [[zk:dtd-%s::time:year::%s][%s]]
"
                    yearp yearp yearp yearp yearp yearp yearp))
    (write-file filepath)
    (kill-current-buffer)))

;;;###autoload
(defun zettelkasten-create-year-files ()
  (interactive)
  (let ((years (number-sequence 1900 2021 1)))
    (dolist (year years)
      (zettelkasten-yearly-file year))))

(defun zettelkasten-journal-daily-file ()
  (interactive)
  (let* ((filename (format-time-string "%Y-%m-%d.org"))
         (filepath (concat org-journal-dir filename)))
    (if (file-exists-p filepath)
        (find-file filepath)
      (org-journal-new-entry t)
      (insert (format-time-string "\n:PROPERTIES:\n"))
      (insert (format-time-string ":RDF_TYPE: time:DateTimeDescription\n"))
      (insert (format-time-string ":CUSTOM_ID: dtd-%Y-%m-%d\n"))
      (insert (format-time-string ":END:\n"))
      (insert (format-time-string "- [[zk:dtd-%Y-%m-%d::time:year::%Y][%Y]]-[[zk:dtd-%Y-%m-%d::time:month::%m][%m]]-[[zk:dtd-%Y-%m-%d::time:day::%d][%d]]\n"))
      (insert (format-time-string "- [[zk:dtd-%Y-%m-%d::time:week::%W][W%W]]\n"))
      (insert (format-time-string "- [[zk:dtd-%Y-%m-%d::time:dayOfWeek::%A][%A]]\n")))))

;;;###autoload
(defun zettelkasten-ext-journal-daily-journal ()
  "Create or open daily note."
  (interactive)
  (zettelkasten-journal-daily-file)
  (goto-char (point-min))
  (unless (search-forward "** Journal" nil t)
    (progn
      (search-forward ":RDF_TYPE: time:DateTimeDescription")
      (outline-next-heading)
      (open-line 1)
      (insert (format-time-string "** Journal %Y-%m-%d"))
      (org-set-property "CUSTOM_ID" (format-time-string "%Y-%m-%dT%H%M%S.%1N"))
      (org-set-property "RDF_TYPE" "zkt:Note")
      (org-set-property "GENERATED_AT_TIME"
                        (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                                (job/current-timezone-offset-hours)))))
  (outline-next-heading)
  (open-line 1))

;;;###autoload
(defun zettelkasten-ext-journal-daily-task ()
  "Create or open daily note."
  (interactive)
  (zettelkasten-journal-daily-file)
  (goto-char (point-min))
  (unless (search-forward "** Daily Task" nil t)
    (progn
      (search-forward ":RDF_TYPE: time:DateTimeDescription")
      (outline-next-heading)
      (open-line 1)
      (insert (format-time-string "** TODO [/] Daily Task %Y-%m-%d    :sticky:"))
      (org-set-property "CUSTOM_ID" (format-time-string "%Y-%m-%dT%H%M%S.%1N"))
      (org-set-property "RDF_TYPE" "zkt:Task")
      (org-set-property "GENERATED_AT_TIME"
                        (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                                (job/current-timezone-offset-hours)))))
  (outline-next-heading)
  (open-line 1)
  (insert "- [ ] "))

;;;###autoload
(defun zettelkasten-journal-weekly-file ()
  (interactive)
  (let* ((filename (format-time-string "%Y-w%W.org"))
         (filepath (concat org-journal-dir filename)))
    (if (file-exists-p filepath)
        (find-file filepath)
      (switch-to-buffer-other-window filename)
      (insert
       (format-time-string "#+TITLE: jr: %Y-W%W:\n#+RDF_TYPE: time:ProperInterval\n#+COLLECTION: journal\n#+DESCRIPTOR: #%Y #%A #W%W @journal\n\n* %Y-W%W"))
      (write-file filepath))))

;;;###autoload
(defun zettelkasten-link-archive ()
  (interactive)
  (let* ((file (read-file-name
                "File: " date-description-dir))
         (file-proc (car (split-string (s-replace date-description-dir "" file) "--")))
         (description (read-string "Description: "
                                   (last (split-string file-proc "/")))))
    (insert (format "[[arch:%s][%s]]" file-proc description))))

(defun zettelkasten-each-file ()
  (interactive)
  (let* ((path "/home/job/Dropbox/db/zk/zettel/txt/")
         (files
          (directory-files path nil ".org$")))
    (dolist (zettel files)
      (let ((fullfname
             (concat path "/" zettel)))
        (find-file fullfname)
        (zettelkasten-zettel-add-collection "txt")
        (goto-char (point-min))
        (while (search-forward "txt txt" nil t)
          (replace-match "txt"))
        (save-buffer)
        (kill-current-buffer)))))

(defun zettelkasten-each-file ()
  (interactive)
  (let* ((files (-flatten
                 (zettelkasten-db-query
                  [:select filename
                   :from nodes
                   :where (= rdftype "time:DateTimeDescription")]))))
    (dolist (zettel files)
      (find-file zettel)
      (goto-char (point-min))
      (while (search-forward "time:DateTimeDescription" nil t)
        (replace-match "time:DateTimeInterval"))
      (save-buffer)
      (kill-current-buffer))))



(defun zettelkasten-each-file-2 ()
  (interactive)
  (let* ((path "/home/job/Dropbox/db/zk/zettel/jr/")
         (files
          (directory-files path nil ".org$")))
    (dolist (zettel files)
      (let ((fullfname
             (concat path "/" zettel)))
        (find-file fullfname)
        (if (not (search-forward "RDF_TYPE:" nil t))
            (progn (zettelkasten-zettel-ensure-keyword "RDF_TYPE")
                   (goto-char (point-min))
                   (search-forward "RDF_TYPE:" nil t)
                   (end-of-line)
                   (insert " time:ProperInterval")
                   (save-buffer)))
        (kill-current-buffer)))))

(defun zettelkasten-each-file-3 ()
  (interactive)
  (let ((files (zettelkasten--get-all-files)))
    (dolist (zettel files)
      (goto-char (point-min))
      (ignore-errors
        (find-file zettel)
        (while (search-forward "#+ZK_LABEL: " nil t)
          (replace-match "#+CUSTOM_ID: "))
        (save-buffer)
        (kill-buffer)
        (message zettel)))))

(defun zettelkasten-each-file-4 ()
  (interactive)
  (let ((files (zettelkasten--get-all-files)))
    (dolist (zettel files)
      (find-file zettel)
      (message (format "zk: Processing: %s" zettel))
      (let* ((element (org-element-parse-buffer))
             (fileid (zettelkasten--filename-to-id zettel))
             (currentid (zettelkasten--get-file-id zettel element))
             (where-obj (-flatten (zettelkasten-db-query
                                   [:select :distinct [n:filename]
                                    :from nodes n
                                    :inner-join edges e
                                    :on (= n:zkid e:subject)
                                    :where (= object $s1)]
                                   fileid))))

        (unless (equal fileid currentid)
          (goto-char (point-min))
          (while (search-forward (concat ":" fileid "::") nil t)
            (replace-match (concat ":" currentid "::")))
          (save-buffer)
          (message (format "zk: Ensured triple consistency, subjects"))
          (kill-buffer)
          (dolist (zettel-obj where-obj)
            (find-file zettel-obj)
            (goto-char (point-min))
            (while (search-forward (concat ":" fileid) nil t)
              (replace-match (concat ":" currentid)))
            (save-buffer)
            (message (format "zk: Ensured triple consistency, objects"))
            (kill-buffer)))
        (message (format "zk: Processed: %s" zettel))))))

;;;###autoload
(defun zettelkasten-headline-set-followup ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-todo "TODO")
    (org-set-property "CATEGORY" "zkt")
    (org-set-tags '("zkt" "followup"))))

;;;###autoload
(defun zettelkasten-headline-reset ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-set-tags '())
    (org-todo "")
    (ignore-errors
      (org-priority ?\ ))))

;;;###autoload
(defun zettelkasten-zettel-open-external ()
  (interactive)
  (let ((element (org-element-parse-buffer))
        (zkid (zettelkasten--filename-to-id (buffer-file-name))))
    (cond ((s-starts-with? "zktb:" (zettelkasten-extract-value "RDF_TYPE" element))
           (org-link-open-from-string
            (concat
             "file:"
             zettelkasten-bibliography-file
             "::"
             (file-name-base (buffer-file-name)))))
          ((member
            "prov:Location"
            (split-string (zettelkasten-extract-value "RDF_TYPE" element)))
           (let ((latitude
                  (caar (zettelkasten-db-query [:select object
                                           :from edges
                                           :where (= subject $s1)
                                           :and (= predicate "geo:lat")]
                                          zkid)))
                 (longitude
                  (caar (zettelkasten-db-query [:select object
                                           :from edges
                                           :where (= subject $s1)
                                           :and (= predicate "geo:long")]
                                          zkid))))
             (browse-url (format
                          "https://www.google.com/maps/search/?api=1&query=%s,%s"
                          latitude longitude)))))))

(defun zettelkasten-open-zettel-todo ()
  (interactive)
  (let* ((completions (zettelkasten-db-query
                       [:select [title filename]
                        :from files
                        :where (= todo 't)]))
         (selection (assoc (completing-read "Zettel: " completions) completions)))
    (find-file (cadr selection))))

(defun zettelkasten--get-collection-zettel ()
  (let ((collection
         (completing-read
          "Collection: " (-flatten
                          (zettelkasten-db-query
                           [:select :distinct [collection]
                                    :from collection])))))
    (zettelkasten-db-query
     [:select [title files:filename collection]
              :from collection
              :left-outer-join files
              :on (= collection:filename files:filename)
              :where (= collection $s1)]
     collection)))

;;;###autoload
(defun zettelkasten-open-zettel-collection ()
  (interactive)
  ;; (let ((collection
  ;;        (completing-read
  ;;         "Collection: " (-flatten
  ;;                         (zettelkasten-db-query
  ;;                          [:select :distinct [collection]
  ;;                                   :from collection]))))))
  (ivy-read
   (format "Zettel: ")
   (zettelkasten--get-collection-zettel)
   :preselect "Inbox"
   :action
   (lambda (selection)
     (find-file
      (cadr selection)))))

;;;###autoload
(defun zettelkasten-open-zettel-random ()
  (interactive)
  (let* ((zettel
          (zettelkasten-db-query [:select [title filename zkid]
                                  :from nodes
                                  :where (= type "file")
                                  :and rdftype :is :null])
          ;; (zettelkasten-db--title-filename)
          )
         (rand-element
          (random (safe-length zettel))))
    (find-file (cadr (nth rand-element zettel)))))

(defun zettelkasten-db--values-predicate ()
  (-flatten
   (zettelkasten-db-query [:select :distinct [predicate]
                           :from edges])))

(defun zettelkasten-node-to-zettel ()
  "Create zettel as file from zettel heading."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((properties (org-entry-properties))
           (title (cdr (assoc "ITEM" properties)))
           (rdftype (cdr (assoc "RDF_TYPE" properties)))
           (label (cdr (assoc "CUSTOM_ID" properties)))
           (descriptor (cdr (assoc "DESCRIPTOR" properties)))
           (collection (cdr (assoc "COLLECTION" properties)))
           (gen-at-time (cdr (assoc "GENERATED_AT_TIME" properties)))
           (turtle (cdr (assoc "TURTLE" properties)))
           (content (progn
                      (org-forward-paragraph 2)
                      (buffer-substring-no-properties
                       (point) (org-end-of-subtree)))))
      (org-cut-subtree)
      (zettelkasten-new-zettel title rdftype)
      (zettelkasten-set-label label)
      (when collection
        (zettelkasten--ensure-keyword "COLLECTION")
        (insert (format " %s" collection)))
      (search-forward "Inhalt")
      (next-line)
      (insert content)
      (when gen-at-time
        (zettelkasten--ensure-keyword "GENERATED_AT_TIME")
        (insert (format " %s" gen-at-time)))
      (when turtle
        (zettelkasten--ensure-keyword "TURTLE")
        (insert (format " %s" turtle)))
      (if descriptor
          (progn
            (zettelkasten--ensure-keyword "DESCRIPTOR")
            (insert (format " %s" descriptor)))
        (zettelkasten-zettel-add-descriptor))
      (save-buffer))))

;;; begin: capture
(defvar zettelkasten-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'zettelkasten-capture-finalize)
    (define-key map "\C-c\C-k" #'zettelkasten-capture-kill)
    map)
  "Keymap for `zettelkasten-capture-mode', a minor mode.")

(defvar zettelkasten-capture-mode-hook nil
  "Hook for the `zettelkasten-capture-mode' minor mode.")

(define-minor-mode zettelkasten-capture-mode
  "Minor mode for special key bindings in a zettelkasten capture buffer.
Turning on this mode runs the hook `zettelkasten-capture-mode-hook'."
  nil " capture" zettelkasten-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<zettelkasten-capture-mode-map>Capture buffer, finish \
`\\[zettelkasten-capture-finalize]', abort `\\[zettelkasten-capture-kill]'.")))

;;;###autoload
(defun zettelkasten-capture ()
  (interactive)
  (if (equal zettelkasten-capture-state 'edit)
      (progn
        (org-tree-to-indirect-buffer)
        (other-window 1 nil))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1 nil)
    (switch-to-next-buffer)
    (zettelkasten-open-zettel)
    (goto-char (point-min))
    (end-of-line)
    (search-forward "*"))
  (zettelkasten-capture-mode))


;;;###autoload
(defun zettelkasten-capture-kill ()
  (interactive)
  (setq zettelkasten-capture-state nil)
  (kill-current-buffer))


;;;###autoload
(defun zettelkasten-capture-finalize ()
  (interactive)
  (cond ((equal zettelkasten-capture-state 'push)
         (progn
           (setq zettelkasten-capture-state t)
           (zettelkasten-link-zettel-other-window)))
        ((equal zettelkasten-capture-state 'push-heading)
         (progn
           (setq zettelkasten-capture-state t)
           (zettelkasten-link-zettel-other-window t)))
        ((equal zettelkasten-capture-state 'refile)
         (progn
           (zettelkasten-refile-subtree-other-window)
           (setq zettelkasten-capture-state t)))
        ((equal zettelkasten-capture-state 'edit)
         (progn
           (kill-current-buffer)
           (other-window 1 nil)
           (delete-other-windows)
           (hydra-zettelkasten-process/body)
           (setq zettelkasten-capture-state t)))
        ((equal zettelkasten-capture-state 'link-heading)
         (org-with-wide-buffer
          (org-back-to-heading t)
          (when (looking-at org-complex-heading-regexp)
            (let* ((predicate
                    (completing-read "Predicate: " (zettelkasten-flat-predicates)))
                   (turtle
                    (save-selected-window
                      (other-window 1)
                      (s-starts-with? ":TURTLE" (thing-at-point 'line t))))
                   (heading (match-string-no-properties 4))
                   (heading-edit (read-string "Heading: " heading))
                   (custom-id (zettelkasten-id-get-create))
                   )
              (save-buffer)
              (zettelkasten-capture-kill)
              (other-window 1)
              (let ((zk-id (org-entry-get nil "CUSTOM_ID")))
                (if turtle
                    (insert (format "%s::%s"
                                    predicate
                                    custom-id))
                  (insert (format "[[zk:%s::%s::%s][%s]]"
                                  zk-id
                                  predicate
                                  custom-id
                                  heading-edit))))))))
        (zettelkasten-capture-state
         (progn
           (save-buffer)
           (zettelkasten-capture-kill)
           (other-window 1)
           (when (equal (buffer-file-name) zettelkasten-inbox-file)
             (zettelkasten-inbox-process)
             (zettelkasten-zettel-info))))
        (t (zettelkasten-capture-kill))))

(defun zettelkasten-refile-subtree-other-window ()
  (let ((current-rfloc
         (list
          (org-display-outline-path t t nil t)
          (buffer-file-name (buffer-base-buffer))
          nil
          (org-with-wide-buffer
           (org-back-to-heading t)
           (point-marker)))))
    (other-window 1)
    (zettelkasten-headline-reset)
    (org-refile nil nil current-rfloc)
    (other-window 1)))

;;;###autoload
(defun zettelkasten-capture-refile ()
  (interactive)
  (setq zettelkasten-capture-state 'refile)
  (zettelkasten-capture))

;;;###autoload
(defun zettelkasten-capture-edit ()
  (interactive)
  (setq zettelkasten-capture-state 'edit)
  (zettelkasten-capture))

(defun zettelkasten-link-zettel-other-window (&optional heading)
  (let* ((filename-ow
          (save-excursion
            (save-selected-window
              (other-window 1)
              (buffer-file-name))))
         (id (if (not heading)
                 (zettelkasten--filename-to-id filename-ow)
               (save-excursion
                 (save-selected-window
                   (other-window 1)
                   (zettelkasten-id-get-create)))))
         (title (if (not heading)
                    (zettelkasten-db-query
                     [:select [title]
                      :from files
                      :where (= filename $s1)]
                     other)
                  (save-excursion
                    (save-selected-window
                      (other-window 1)
                      (org-back-to-heading t)
                      (when (looking-at org-complex-heading-regexp)
                        (match-string-no-properties 4)))))))
    (insert (format "** [[zk:%s][%s]]" id title))))

;;;###autoload
(defun zettelkasten-capture-push (&optional heading)
  (interactive)
  (if (not heading)
      (setq zettelkasten-capture-state 'push)
    (setq zettelkasten-capture-state 'push-heading))
  (zettelkasten-capture))

;;; begin: index
;; TODO: subitems, 
;;;###autoload
(defun zettelkasten-create-index-topic ()
  (interactive)
  (let* ((zettel-1
          (zettelkasten-db-query [:select [filename title] :from index
                                          :where (= entry "t")]))
         (zettel-2
          (zettelkasten-db-query [:select [filename entry title] :from index
                                          :where (not (= entry "t"))])))
    (switch-to-buffer-other-window "*zettelkasten-index*")
    (erase-buffer)
    (insert (format "#+TITLE: Zettelkasten index\n\n"))
    (dolist (row zettel-1)
      (let ((fname (car row))
            (title (cadr row)))
        (insert (format "- [[file:%s][%s]]\n" fname title))))
    (dolist (row zettel-2)
      (let ((fname (car row))
            (index (cadr row))
            (title (caddr row)))
        (goto-char (point-min))
        (while (search-forward (format "[%s]]" index) nil t)
          (end-of-line)
          (newline)
          (insert (format "  - [[file:%s][%s]]" fname title)))))
    (org-mode)
    (previous-line)
    (ignore-errors
      (org-sort-list t ?a))))
;;; end

;;; begin: inbox
;;;###autoload
 (defun zettelkasten-rfloc (file headline)
   (let ((pos
          (save-excursion
            (save-selected-window
              (if (string= file (buffer-file-name))
                  (find-file file)
                (find-file-other-window file))
              (org-find-exact-headline-in-buffer headline)))))
     (org-refile nil nil (list headline file nil pos))))

;;;###autoload
(defun zettelkasten-inbox-process ()
  (interactive)
  (ignore-errors
    (windmove-left))
  (find-file zettelkasten-inbox-file)
  (widen)
  (goto-char (point-min))
  (search-forward "* Elfeed")
  (org-sort-entries nil ?p)
  (org-next-visible-heading 1)
  (org-narrow-to-subtree)
  (org-show-all))

;;;###autoload
(defun zettelkasten-inbox-bury ()
  (interactive)
  (widen)
  (zettelkasten-rfloc zettelkasten-inbox-file "Elfeed")
  (zettelkasten-inbox-process))

;;;###autoload
(defun zettelkasten-inbox-trash ()
  (interactive)
  (widen)
  (org-todo "CANCELLED")
  (zettelkasten-rfloc zettelkasten-inbox-file "Trash")
  (zettelkasten-inbox-process))

;;;###autoload
(defun zettelkasten-ext-create-related-ressource (&optional incoming)
  (interactive)
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (source-id (caar (zettelkasten-get-property-or-keyword-upwards
                           filename element "CUSTOM_ID")))
         (source-types (car (zettelkasten-get-property-or-keyword-upwards
                             filename element "RDF_TYPE")))
         (generalized-types (zettelkasten--generalize-types
                             source-types))
         (predicate-options (if incoming (zettelkasten--get-predicates-by-range
                                          generalized-types)
                              (zettelkasten--get-predicates-by-domain
                               generalized-types)))
         (predicate (completing-read (if incoming "Predicate (incoming): "
                                       "Predicate (outgoing):")
                                     predicate-options))
         (predicate-domains-or-ranges (if incoming (zettelkasten--get-domains-of-predicates
                                                    (list predicate))
                                        (zettelkasten--get-ranges-of-predicates (list predicate))))
         (type-choices (delete-dups
                        (-flatten (zettelkasten--tree-children-rec
                                   (car predicate-domains-or-ranges)
                                   zettelkasten-classes))))
         (created-type (completing-read "Ressource type: " type-choices))
         (created-id (zettelkasten-id-get-create (org-id-new) t))
         (created-title (read-string "Title: ")))
    (unless incoming
      (insert "- ")
      (zettelkasten-insert-link source-id predicate `(,created-id ,created-title))
      (newline))

    (outline-next-heading)
    (open-line 1)
    (insert (concat "*** " created-title))
    (zettelkasten-set-type-headline created-type)
    (org-set-property "CUSTOM_ID" created-id)
    (when incoming
      (zettelkasten-heading-set-relation-to-context
       predicate source-id))
    ;; Gen at generation
    (when (member created-type
                  '("zkt:Zettel" "zkt:Mitschrift" "zkt:Task"))
      (org-set-property
       "GENERATED_AT_TIME"
       (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
               (job/current-timezone-offset-hours))))
    ;; Mitschrift
    (when (member created-type '("zkt:Mitschrift"))
      (zettelkasten-heading-set-relation-to-context
       "zktb:wasAuthoredBy" "@me")
      (org-schedule nil))
    ;; Task
    (when (member created-type '("zkt:Task"))
      (org-todo "TODO")
      (zettelkasten-heading-set-relation-to-context
       "zkt:hadAdressat" "@me")
      (when (y-or-n-p "Link to activity? ")
        (zettelkasten-heading-set-relation-to-context
         "prov:wasGeneratedBy"))
      (org-set-tags-command))))

(defun zettelkasten--get-predicates-by-range (subject-types)
  "Get all predicates which domains match type in SUBJECT-TYPES."
  (-flatten
   (zettelkasten-db-query
    [:select [name]
     :from predicates
     :where (in range $v1)]
    (vconcat subject-types))))

(defun zettelkasten--get-domains-of-predicates (predicates)
  (-flatten
   (zettelkasten-db-query
    [:select [domain]
     :from predicates
     :where (in name $v1)]
    (vconcat predicates))))


(defhydra hydra-zettelkasten-process ()
  "Process"
  ("p" org-previous-visible-heading "Previous heading" :column "Misc")
  ("n" org-next-visible-heading "Next heading")
  ("e" zettelkasten-capture-edit "Edit" :color blue)
  ("r" org-refile "Refile")
  ("R" zettelkasten-capture-refile "Capture Refile")
  ("i" (zettelkasten-rfloc "~/OneDrive - University of Bergen/archive/zk/zettel/2022-03-16-0946-inbox.org" "Tasks") "Inbox" :column "Targets")
  ("s" (zettelkasten-rfloc "~/OneDrive - University of Bergen/archive/zettel/2022-03-09-1112-position-systemutviklar-uib.org" "Tasks") "Systemutvikler, position")
  ("t" (zettelkasten-rfloc "~/OneDrive - University of Bergen/archive/zk/zettel/2022-03-07-1153-termportalen.org" "Tasks") "Termportalen")
  ("m" (zettelkasten-rfloc "~/OneDrive - University of Bergen/archive/zk/zettel/2023-03-14-0934-marcus-next.org" "Tasks") "Marcus Next")
  ("c" (zettelkasten-rfloc "~/OneDrive - University of Bergen/archive/zk/zettel/2024-11-18-0908-project-clarino.org" "Tasks") "Clarino")
  ("a" org-archive-subtree-default "Archive" :column "Archive")
  ;; ("b" zettelkasten-inbox-bury "Bury")
  ;; ("T" zettelkasten-inbox-trash "Trash")
  )

;;; begin: hydra
(defhydra hydra-zettelkasten (:color blue)
  "Zettelkasten"
  ("C-ä" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("C-æ" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("Ä" (zettelkasten-open-zettel t) "Open Nodes")
  ("Æ" (zettelkasten-open-zettel t) "Open Nodes")
  ("R" zettelkasten-open-zettel-random "Open random")
  ;; ("ä" zettelkasten-open-zettel-collection "Open collection")
  ("æ" zettelkasten-open-zettel-collection "Open collection")
  ("d" zettelkasten-open-zettel-descriptor "Open descriptor")
  ("s" zettelkasten-open-semantic "Open semantic")
  ("jd" zettelkasten-journal-daily-file "Daily file")
  ("e" (zettelkasten-get-top-zettel
        (caar (zettelkasten-get-property-or-keyword-upwards
               (buffer-file-name)
               (org-element-parse-buffer)
               "CUSTOM_ID")))
   "Heading entrypoints")
  ("E" (zettelkasten-get-top-zettel nil 2) "Entrypoints")

  ("l" zettelkasten-insert-link "Link" :column "Edit")
  ("L" zettelkasten-insert-link-loop "Link loop")
  ("jj" zettelkasten-ext-journal-daily-journal "Daily note")
  ("jt" zettelkasten-ext-journal-daily-task "Daily task")

  ("p" zettelkasten-capture-push "Push Link" :column "Zettelkasten")
  ("P" (zettelkasten-capture-push t) "Push Heading")
  ("D" zettelkasten-replace-descriptor "Replace Desc.")
  ;; ("I" zettelkasten-info "Info")

  ("c" zettelkasten-zettel-add-collection "Add collection" :column "Zettel")
  ;; ("#" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("'" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("x" zettelkasten-zettel-add-index "Add Index")
  ;; ("i" zettelkasten-zettel-info "Info")
  ;; ("v" zettelkasten-vis-buffer "Visualize")
  ("ü" zettelkasten-set-type-and-label "Set label and type")
  ("o" zettelkasten-zettel-open-external "Open external")
  ("z" zettelkasten-ext-create-related-ressource "Create Outgoing")
  ("Z" (zettelkasten-ext-create-related-ressource t) "Create Incoming")

  ("hc" zettelkasten-headline-add-collection "Add collection" :column "Heading")
  ("C-'" zettelkasten-headline-add-descriptor "Add descriptor")
  ("+" zettelkasten-heading-to-node "Heading to node")
  ("hf" zettelkasten-headline-set-followup "Set followup")
  ("hr" zettelkasten-headline-reset "Reset")
  ("hz" zettelkasten-node-to-zettel "Node to zettel")
  ("gw" job/add-tag-this-week-dwim "Task this week")

  ("n" org-noter "noter" :column "Other")
  ;; ("u" zettelkasten-update-org-agenda-files "Update agenda")
  ("r" hydra-zettelkasten-process/body "Refile hydra")

  ("q" nil "Quit"))

;;; begin: export
;;; TODO: add support for exporting links to headlines
(defun zettelkasten-org-zk-export (path desc format)
  "Format zk links for export."
  (let* ((filename
          (caar (zettelkasten-db-query [:select filename :from id
                                                :where (= zkid $s1)]
                                       path)))
         (html-name
          (s-replace-all '((zettelkasten-zettel-directory . "")
                           (".org" . ".html"))
                         (plist-get entry :file))))
    (when (eq format 'html)
      (format "<a href=\"./%s\">%s</a>"
              html-name
              desc))))

(org-link-set-parameters "zk" :export 'zettelkasten-org-zk-export)
;;; end


;;;begin: random
;;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
;;; refactor!
(defun org-global-props (&optional property buffer)
  "Get the alists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el)
        (when (string-match property (org-element-property :key el)) el)))))






;; (zettelkasten-db-query
;;            [:select :distinct [collection]
;;                     :from collection
;;                     :where (in collection ["index" "content"])])

;; (zettelkasten-db-query
;;  [:select [title files:filename collection]
;;           :from collection
;;           :left-outer-join files
;;           :on (= collection:filename files:filename)
;;           :where (= collection $s1)
;;           ] "content")


;; (zettelkasten-db-query [:select [e1:object e2:object e3:object]
;;                         :from edges e
;;                         :left-join edges e1 :on (= e:subject e1:subject)
;;                         :left-join edges e2 :on (= e:subject e2:subject)
;;                         :left-join edges e3 :on (= e:subject e3:subject)
;;                         :where (= e:predicate "rdf:type")
;;                         :and (= e:object "zkt:Wandern")
;;                         :and (= e1:predicate "time:intervalDuring")
;;                         :and (= e2:predicate "time:hours")
;;                         :and (= e3:predicate "zkt:distanceKM")
;;                         :order-by e1:object
;;                         ])

;;;###autoload
(defun zettelkasten-heading-to-docpart ()
  (interactive)
  (zettelkasten-set-type-headline '("zkt:DocumentPart"
                                    "zkt:FormalDocumentPart"))
  (zettelkasten-id-get-create (concat (file-name-base) "--"))
  (zettelkasten-heading-set-relation-to-context "dct:isPartOf"))

;;;###autoload
(defun zettelkasten-open-dir ()
  (interactive)
  (find-file (read-file-name "Zettel: " zettelkasten-zettel-directory)))

;;;###autoload
(defun zettelkasten-ag-query ()
   (interactive)
   (counsel-ag nil zettelkasten-zettel-directory nil))

;;;###autoload
(defun zettelkasten-ag-query-symbol-at-point ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol) zettelkasten-zettel-directory nil))

(defun zettelkasten--get-collections ()
  (zettelkasten-db-query
   [:select :distinct [zkid]
    :from edges e
    :inner-join nodes n
    :on (= e:subject n:zkid)
    :where (= e:predicate "rdf:type")
    :and (= e:object "prov:Collection")
    ]))

;;;###autoload
(defun zettelkasten-zettel-add-collection (&optional collection)
  (interactive)
  (save-excursion
    (zettelkasten--ensure-keyword "COLLECTION")
    (insert (concat " "
                    (or collection
                        (completing-read
                         "Collection: "
                         (zettelkasten--get-collections))))))
  (unless collection
    (zettelkasten-zettel-add-collection)))

;;;###autoload
(defun zettelkasten-zettel-add-index (&optional index)
  (interactive)
  (let* ((idx (or index
                  (completing-read
                   "Zettel: "
                   (zettelkasten-db-query [:select :distinct title :from index :where (= entry "t")]))))
         (zet-title (zettelkasten--extract-title))
         (new-title (read-string "Title: " zet-title))
         (ins-title (if (equal zet-title new-title)
                        t
                      new-title)))
    (zettelkasten--ensure-keyword "INDEX")
    (end-of-line)
    (insert (format " \"%s::%s\"" idx ins-title))))

;;;###autoload
(defun zettelkasten-headline-add-collection ()
  "Add descriptor to current headline."
  (interactive)
  (let* ((current
          (split-string
           (or (org-entry-get nil "COLLECTION") "")))
         (add
          (ivy-read "Collection [Headline]: " (zettelkasten--get-collections)))
         (join
          (sort (append current (list add)) 'string<))
         (string
          (mapconcat 'identity join " ")))
    (org-set-property "COLLECTION" string))
  (zettelkasten-headline-add-collection))

;;;###autoload
(defun zettelkasten-zettel-store-link ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (right-char 9)
    (kill-ring-save (point) (line-end-position))
    (call-interactively 'org-store-link)))


;; Zettel output
;;;###autoload
(defun zettelkasten-tangle-combined ()
  (interactive)
  (org-babel-tangle-file
   (concat zettelkasten-main-directory "/zettel-combined.org")))

;;;###autoload
(defun zettelkasten-gitstats ()
  (interactive)
  (shell-command-to-string (concat "cd ~/Dropbox/db/zk/zettel &&"
                                   "gitstats .git gitstats &&"
                                   "firefox 'gitstats/index.html'")))

(defun zettelkasten--select-zettel (zettel)
  (ivy-read
   "Zettel: "
   (mapcar (lambda (arg)
             (cons
              (plist-get arg :title)
              (plist-get arg :file)))
           zettel)
   :preselect "Inbox"
   :action
   (lambda (selection)
     (setq zettelkasten-zettel-selected selection)))
  zettelkasten-zettel-selected)


(defvar zettelkasten-ext-with-agent-id nil)
(defun zettelkasten-ext-get-person-id ()
  "Query for agents and set current agent id."
  (let* ((completions
          (zettelkasten-db-query [:select :distinct [n:title n:zkid]
                                  :from nodes n
                                  :inner-join edges e
                                  :on (= n:zkid e:subject)
                                  :where (in e:object $v1)]
                                 (vconcat (-flatten (zettelkasten--tree-children-rec
                                                     "prov:Agent"
                                                     zettelkasten-classes)))))
         (selection (assoc (completing-read "Agent: " completions) completions)))

    (setq zettelkasten-ext-with-agent-id (cadr selection))
    (car selection)))

(defun zettelkasten-task-overview ()
  "Count tasks ad invalidated."
  (interactive)
  (let ((tasks (caar
                (zettelkasten-db-query
                 [:select [(funcall count e:subject)]
                  :from edges e
                  :inner-join edges e2
                  :on (= e:subject e2:subject)
                  :where (= e:predicate "rdf:type")
                  :and (= e:object "zkt:Task")
                  :and (= e2:predicate "prov:generatedAtTime")])))
        (invalidated (caar
                      (zettelkasten-db-query
                       [:select [(funcall count e:subject)]
                        :from edges e
                        :inner-join edges e2
                        :on (= e:subject e2:subject)
                        :inner-join edges e3
                        :on (= e:subject e3:subject)
                        :where (= e:predicate "rdf:type")
                        :and (= e:object "zkt:Task")
                        :and (= e2:predicate "prov:generatedAtTime")
                        :and (= e3:predicate "prov:invalidatedAtTime")]))))
    (message (format "Tasks: %s, Done: %s, active: %s"
                     tasks invalidated (- tasks invalidated)))))

(defun zettelkasten-plot-data (data tdtask tdinval current)
  "Plot task overview from DATA, TDTASK TDINVAL CURRENT."
  (let* ((svgwidth 1150)
         (svgheight 200)

         (svg (svg-create svgwidth svgheight :stroke-width 1))
         (width 10)
         (ycenter (/ svgheight 2))
         (xspace 5)
         (xpos 5)
         (factor 10)
         (linex -5)
         (liney ycenter)
         (line `((-5 . ,ycenter))))
    
    (dolist (d data)
      (let ((green (* factor (car d)))
            (red (* factor (cadr d))))
        (svg-rectangle svg xpos (- ycenter green) width green :fill "green" :stroke "green" :id (concat "green" (number-to-string xpos)))
        (svg-rectangle svg xpos ycenter width red :fill "red" :stroke "red" :id (concat "red" (number-to-string xpos)))
        (setq xpos (+ xpos (+ width xspace)))
        (setq linex (+ linex (+ width xspace)))
        (setq liney (- liney (- (car d) (cadr d))))
        (add-to-list 'line `(,linex . ,liney) t)
        ))
    (svg-polyline svg line :stroke-width 2 :stroke "darkblue" :fill "transparent")

    (svg-line svg 0 ycenter (- svgwidth 80) ycenter :id "line1" :stroke "black")
    (svg-text svg
              (number-to-string tdtask)
              :font-size "20"
              :stroke "green"
              :x (- svgwidth 100)
              :y (- ycenter (* ycenter 0.1))
              :stroke-width 1)
    (svg-text svg
              (number-to-string tdinval)
              :font-size "20"
              :stroke "red"
              :x (- svgwidth 100)
              :y (+ ycenter (* ycenter 0.2))
              :stroke-width 1)
    (svg-text svg
              (number-to-string current)
              :font-size "25"
              :x (- svgwidth 60)
              :y (+ ycenter (* ycenter 0.05))
              :stroke-width 1)
    (format "%s" (svg-insert-image svg))))

;;;###autoload
(defun zettelkasten-task-overview-plot ()
  "Count tasks ad invalidated."
  (interactive)
  (let* ((tasks
          (zettelkasten-db-query
           [:select [(funcall substr e2:object 2 10)
                     (funcall count e:subject)]
            :from edges e
            :inner-join edges e2
            :on (= e:subject e2:subject)
            :where (= e:predicate "rdf:type")
            :and (= e:object "zkt:Task")
            :and (= e2:predicate "prov:generatedAtTime")
            :group-by (funcall substr e2:object 2 10)
            :order-by (funcall substr e2:object 2 10)
            ]))
         (invalidated
          (zettelkasten-db-query
           [:select [(funcall substr e3:object 2 10)
                     (funcall count e:subject)]
            :from edges e
            :inner-join edges e2
            :on (= e:subject e2:subject)
            :inner-join edges e3
            :on (= e:subject e3:subject)
            :where (= e:predicate "rdf:type")
            :and (= e:object "zkt:Task")
            :and (= e2:predicate "prov:generatedAtTime")
            :and (= e3:predicate "prov:invalidatedAtTime")
            :group-by (funcall substr e3:object 2 10)
            :order-by (funcall substr e3:object 2 10) :desc
            ]))
         (combined '()))

    (dolist (entry tasks)
      (if (assoc (car entry) invalidated)
          (let* ((taskc (cadr entry))
                 (invalc (car (alist-get (car entry) invalidated))))
            (setf invalidated (assoc-delete-all (car entry) invalidated))
            (add-to-list 'combined `(,(car entry) (,taskc ,invalc))))
        (add-to-list 'combined `(,(car entry) (,(cadr entry) 0)))))

    (dolist (entry invalidated)
      (add-to-list 'combined `(,(car entry) (0 ,(cadr entry)))))

    (let* ((sorted (-sort (lambda (a b)
                            (string< (car a) (car b)))
                          combined))
           (numbers (mapcar (lambda (entry)
                              (cadr entry))
                            sorted))
           (length (length numbers))
           (tail (seq-subseq numbers (- length 69)))
           (todaytask
            (or
             (caar (alist-get (intern (format-time-string "%Y-%m-%d"))
                              sorted))
             0))
           (todayinval
            (or
             (car (cdar (alist-get (intern (format-time-string "%Y-%m-%d"))
                                   sorted)))
             0))
           (activetasks (-sum
                         (mapcar (lambda (arg)
                                   (cadr arg))
                                 tasks)))
           (invaltasks (-sum
                        (mapcar (lambda (arg)
                                  (cadr (cadr arg)))
                                combined)))
           (taskcount (- activetasks invaltasks)))
      (zettelkasten-plot-data tail todaytask todayinval taskcount))))

;;;###autoload
(defun zettelkasten-task-finished-last-days ()
  "Display list of tasks finished in the last X days."
  (interactive)
  (let ((number (read-number "Times: " 7))
        (date (decode-time))
        (days '()))
    (dotimes (n number)
      (let ((upd-date (decoded-time-add date (make-decoded-time :day (- n)))))
        (add-to-list 'days (intern (format-time-string "%F" (apply #'encode-time upd-date))))))
    (let ((tasks (zettelkasten-db-query
                  [:select [n:title e:subject]
                   :from edges e
                   :inner-join edges e3
                   :on (= e:subject e3:subject)
                   :inner-join nodes n
                   :on (= e:subject n:zkid)
                   :where (= e:predicate "rdf:type")
                   :and (= e:object "zkt:Task")
                   :and (= e3:predicate "prov:invalidatedAtTime")
                   :and (in (funcall substr e3:object 2 10) $v1)
                   :order-by e3:object :desc]
                  (vconcat days))))
      (switch-to-buffer-other-window "*Tasks*")
      (erase-buffer)
      (insert "#+title: Tasks\n\n")
      (dolist (task tasks)
        (insert (format " [[zk:%s][%s]]\n" (cadr task) (car task))))
      (org-mode))))

(defun zettelkasten--get-children (zkid property &optional order-property)
  (if order-property
      (zettelkasten-db-query
       [:select :distinct [n:zkid n:title e2:object]
        :from nodes n
        :inner-join v_edges_union e
        :on (= n:zkid e:object)
        :and (= e:subject $s1)
        :and (= e:predicate $s2)
        :left-join v_edges_union e2
        :on (and (= e:object e2:subject)
                 (= e2:predicate $s3))
        :order-by [(desc e2:object)]
        ]
       zkid property order-property)
    (zettelkasten-db-query
     [:select :distinct [n:zkid n:title]
      :from nodes n
      :inner-join v_edges_union e
      :on (= n:zkid e:object)
      :and (= e:subject $s1)
      :and (= e:predicate $s2)
      ]
     zkid property)))

(defun increment-letter (letter)
  "Increment a letter, wrapping from z to a."
  (let ((char (string-to-char letter)))
    (char-to-string
     (cond
      ((= char ?z) ?a)
      ((= char ?Z) ?A)
      (t (1+ char))))))

(defun zettelkasten-insert-zettel-children (parent-signature
                                            folgecounter-in
                                            zkid
                                            &optional max-level level)
  ;; Branches
  (unless (and max-level level
               (= max-level level))
    (let* ((branches (zettelkasten--get-children zkid "zkt:hasBranch"))
           (branchcounter "a")
           (current-level (+ level 1)))
      (dolist (branchzettel branches)
        (let ((signature (format "%s%s%s" parent-signature folgecounter-in branchcounter)))
          (insert (format "  - %s [[zk:%s][%s]]\n" signature (car branchzettel) (cadr branchzettel)))
          (setq branchcounter (increment-letter branchcounter))
          (zettelkasten-insert-zettel-children signature 0 (car branchzettel) max-level current-level)))))

  ;; Folgezettel
  (let ((current-zkid zkid)
        (folgecounter (or folgecounter-in 0)))
    (let ((folgezettel (car (zettelkasten--get-children current-zkid "zkt:followedBy")))
          (signature (format "%2s%d" parent-signature folgecounter)))
      (when (car folgezettel)
        (setq folgecounter (1+ folgecounter))
        (insert (format "  - %s [[zk:%s][%s]]\n" (format "%2s%d" parent-signature folgecounter) (car folgezettel) (cadr folgezettel)))
        (zettelkasten-insert-zettel-children parent-signature folgecounter (car folgezettel) max-level level))
      (setq current-zettel (cadr folgezettel)))))

(defun zettelkasten-get-top-zettel (&optional zettel-id max-level)
  (interactive)
  (let* ((top-zettel
          (if zettel-id
              (zettelkasten-db-query
               [:select :distinct [title zkid]
                :from nodes
                :where (= zkid $s1)
                ]
               zettel-id)
            (cl-remove-if-not
             (lambda (x) (null (caddr x)))
             (zettelkasten-db-query
              [:select :distinct [n:title n:zkid e2:predicate]
               :from nodes n
               :inner-join v_edges_union e
               :on (= n:zkid e:subject)
               :and (= e:predicate "rdf:type")
               :and (= e:object "zkt:Zettel")
               :left-join v_edges_union e2
               :on (and (= n:zkid e2:subject)
                        (or (= e2:predicate "zkt:follows")
                            (= e2:predicate "zkt:branchesOffFrom")))
               :order-by n:title :collate :nocase
               ]))))
         (z-length (length top-zettel))
         (counter 1)
         (signature (format "%2d" counter)))
    (switch-to-buffer-other-window "*Zettel*")
    (erase-buffer)
    (insert (format "#+title: Zettel (%s)\n\n" z-length))
    (dolist (zettel top-zettel)
      (let ((signature (format "%2d" counter)))
        (insert (format "- %s [[zk:%s][%s]]\n" signature (cadr zettel) (car zettel)))
        (zettelkasten-insert-zettel-children (format "%s." signature) 0 (cadr zettel) max-level 1)
        (setq counter (1+ counter)))))
  (goto-char (point-min))
  (org-mode))

;;;###autoload
(defun zettelkasten-list-collection-members ()
  (interactive)
  (let* ((zkid (zettelkasten--get-zkid-at-point))
         (title (caar (zettelkasten-db-query
                       [:select :distinct [title]
                        :from nodes
                        :where (= zkid $s1)
                        ]
                       zkid)))
         (members (zettelkasten--get-children zkid "prov:hadMember" "prov:generatedAtTime")))
    (when title
      (switch-to-buffer-other-window "*Zettelkasten: Collection members")
      (erase-buffer)
      (insert (format "#+TITLE: %s\n\n" title))
      (dolist (member members)
        (insert (format "- %-100s %s\n"
                        (format "[[zk:%s][%s]]" (car member) (cadr member))
                        (nth 2 member))))
      (org-mode))))


(defun zettelkasten--insert-activities-rec (zkid property add-order-prop level)
  (let ((children (zettelkasten--get-children zkid property add-order-prop))
        (level-indent 2))
    (dolist (child children)
      (insert (format "%s- %s (%s)\n"
                      (make-string (* level-indent level) ?\s)
                      (format "[[zk:%s][%s]]" (car child) (cadr child))
                      (if (caddr child)
                          (car (split-string (caddr child) "T"))
                        "")))
      (zettelkasten--insert-activities-rec (car child) property add-order-prop (+ level 1)))))

;;;###autoload
(defun zettelkasten-list-child-activities ()
  (interactive)
  (let* ((zkid (zettelkasten--get-zkid-at-point))
         (title (caar (zettelkasten-db-query
                       [:select :distinct [title]
                        :from nodes
                        :where (= zkid $s1)
                        ]
                       zkid))))
    (switch-to-buffer-other-window "*Zettelkasten: Activities")
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n" title))
    (zettelkasten--insert-activities-rec zkid "dct:hasPart" "prov:startedAtTime" 0)
    (org-mode)))



(provide 'zettelkasten-ext)
;;; zettelkasten.el ends here
