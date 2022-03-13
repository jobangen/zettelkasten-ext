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
;;; end:


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
;;; end:


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
                               url)))
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
           (ido-completing-read
            "Bibfile: "
            (append (f-entries "." (lambda (f)
                                     (and (not (string-match "#" f))
                                          (f-ext? f "bib"))))
                    org-ref-default-bibliography)))
          action)
         ;; Multiple DOIs found
         ((> (length dois) 1)
          (ivy-read "Select a DOI" (let ((dois '()))
                                     (with-current-buffer (url-retrieve-synchronously url)
                                       (loop for doi-pattern in org-ref-doi-regexps
                                             do
                                             (goto-char (point-min))
                                             (while (re-search-forward doi-pattern nil t)
                                               (pushnew
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
                      (let ((bibfile (car org-ref-default-bibliography))
                            (doi (cdr x)))

                          (doi-utils-add-bibtex-entry-from-doi
                           doi
                           bibfile)
                          ;; this removes two blank lines before each entry.
                          (bibtex-beginning-of-entry)
                          (delete-char -2)
                        ;; edit entry
                        (delete-other-windows)
                        (split-window-horizontally)
                        (other-window 1 nil)
                        (find-file bibfile)
                        (goto-char (point-max))
                        (bibtex-beginning-of-entry)
                        ;; (when (search-forward "year" nil t)
                        ;;   (replace-match "date"))
                        ;; (bibtex-clean-entry t)
                        ;; (save-buffer)
                        ;; (org-ref-open-bibtex-notes)
                        )))))))))

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
  (let ((feed (zettelkasten-elfeed-get-feed-title))
        (priority (completing-read "Priority: " '("A" "B" "C" "D" "E"))))
    (doi-utils-add-entry-from-elfeed-entry)
    (zettelkasten-db-query [:insert :into capture
                                    :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") priority)))

;;;###autoload
(defun zettelkasten-elfeed-skip ()
  (interactive)
  (let ((feed (zettelkasten-elfeed-get-feed-title)))
    (zettelkasten-db-query [:insert :into capture
                                    :values ([nil $s1 $s2 $s3])]
                           feed (format-time-string "%Y-%m-%d") "Z"))
  (elfeed-show-next))
;;; end:


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
  (let* ((arch-path "/home/job/archive/date-description/")
         (file (read-file-name
                "File: " arch-path))
         (file-proc (car (split-string (s-replace arch-path "" file) " -- ")))
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
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((properties (org-entry-properties))
           (title (cdr (assoc "ITEM" properties)))
           (rdftype (cdr (assoc "RDF_TYPE" properties)))
           (label (cdr (assoc "CUSTOM_ID" properties)))
           (descriptor (cdr (assoc "DESCRIPTOR" properties)))
           (collection (cdr (assoc "COLLECTION" properties)))
           (content (progn
                      (org-forward-paragraph 2)
                      (buffer-substring-no-properties
                       (point) (org-end-of-subtree)))))
      (org-cut-subtree)
      (zettelkasten-new-zettel title rdftype)
      (zettelkasten-set-label label)
      (when collection
        (zettelkasten-zettel-ensure-keyword "COLLECTION")
        (insert (format " %s" collection)))
      (search-forward "Inhalt")
      (next-line)
      (insert content)
      (if descriptor
          (progn
            (zettelkasten-zettel-ensure-keyword "DESCRIPTOR")
            (insert (format " %s" descriptor)))
        (zettelkasten-zettel-add-descriptor))
      (save-buffer))))



;;; end:


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
Turning on this mode runs the normal hook `zettelkasten-capture-mode-hook'."
  nil " capture" zettelkasten-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<zettelkasten-capture-mode-map>Capture buffer, finish \
`\\[zettelkasten-capture-finalize]', abort `\\[zettelkasten-capture-kill]'.")))

;;;###autoload
(defun zettelkasten-capture ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (zettelkasten-open-zettel)
  (goto-char (point-min))
  (end-of-line)
  (search-forward "*")
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
;;; end:

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
;;; end:

(defun zettelkasten-generate-entity-from-activity-at-point ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (source-id (zettelkasten-get-property-or-keyword-upwards
                     filename element "CUSTOM_ID"))
         (source-type (zettelkasten-get-property-or-keyword-upwards
                       filename element "RDF_TYPE"))
         (type-choices (zettelkasten--tree-children-rec
                        "prov:Entity" zettelkasten-classes)))
    (if (member source-type (zettelkasten--tree-children-rec
                             "prov:Activity" zettelkasten-classes))
        (message "Zk: ressource is not an activity.")
      (let ((type (completing-read "Type: " type-choices)))
        (outline-next-heading)
        (open-line 1)
        (insert (concat "*** " (read-string "Title: ")))
        (zettelkasten-set-type-headline type)
        (zettelkasten-id-get-create)
        (zettelkasten-heading-set-relation-to-context
         "prov:wasGeneratedBy" source-id)
        (org-set-property "GENERATED_AT_TIME"
                          (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                                  (job/current-timezone-offset-hours)))
        (when (string= type "zkt:Task")
          (org-todo "TODO")
          (org-schedule))))))

(defun zettelkasten-derive-task-from-entity-at-point ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (element (org-element-parse-buffer))
         (source-id (zettelkasten-get-property-or-keyword-upwards
                     filename element "CUSTOM_ID"))
         (source-type (zettelkasten-get-property-or-keyword-upwards
                       filename element "RDF_TYPE")))
    (if (member source-type (zettelkasten--tree-children-rec
                             "prov:Entity" zettelkasten-classes))
        (message "Zk: ressource is not an entity.")
      (outline-next-heading)
      (open-line 1)
      (insert (concat "*** " (read-string "Title: ")))
      (zettelkasten-set-type-headline "zkt:Task")
      (zettelkasten-id-get-create)
      (zettelkasten-heading-set-relation-to-context
       "prov:wasDerivedFrom" source-id)
      (org-set-property "GENERATED_AT_TIME"
                          (concat (format-time-string "%Y-%m-%dT%H:%M:%S+")
                                  (job/current-timezone-offset-hours)))
      (org-todo "TODO")
      (org-schedule)
      (when (y-or-n-p "Link to activity?")
        (zettelkasten-heading-set-relation-to-context
         "prov:wasGeneratedBy")))))

;;; begin: hydra
(defhydra hydra-zettelkasten (:color blue)
  "Zettelkasten"
  ("C-ä" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("C-æ" zettelkasten-open-zettel "Open Zettel" :column "Open")
  ("Ä" (zettelkasten-open-zettel t) "Open Nodes")
  ("Æ" (zettelkasten-open-zettel t) "Open Nodes")
  ("R" zettelkasten-open-zettel-random "Open random")
  ("ä" zettelkasten-open-zettel-collection "Open collection")
  ("d" zettelkasten-open-zettel-descriptor "Open descriptor")
  ("s" zettelkasten-open-semantic "Open semantic")
  ("jw" zettelkasten-journal-weekly-file "Weekly file")

  ("C-r" zettelkasten-inbox-process (format "Process inbox [%s]" 5) :color red :column "Inbox")
  ("b" zettelkasten-inbox-bury "Bury" :color red)
  ("t" zettelkasten-inbox-trash "Trash" :color red)

  ("l" zettelkasten-insert-link "Link" :column "Edit")
  ("L" zettelkasten-insert-link-loop "Link loop")

  ("p" zettelkasten-capture-push "Push Link" :column "Zettelkasten")
  ("P" (zettelkasten-capture-push t) "Push Heading")
  ("D" zettelkasten-replace-descriptor "Replace Desc.")
  ("I" zettelkasten-info "Info")

  ("c" zettelkasten-zettel-add-collection "Add collection" :column "Zettel")
  ("#" zettelkasten-zettel-add-descriptor "Add descriptor")
  ("x" zettelkasten-zettel-add-index "Add Index")
  ("i" zettelkasten-zettel-info "Info")
  ("v" zettelkasten-vis-buffer "Visualize")
  ("ü" zettelkasten-set-type-and-label "Set label and type")
  ("o" zettelkasten-zettel-open-external "Open external")

  ("hc" zettelkasten-headline-add-collection "Add collection" :column "Heading")
  ("C-#" zettelkasten-headline-add-descriptor "Add descriptor")
  ("r" zettelkasten-capture-refile "Refile")
  ("+" zettelkasten-heading-to-node "Node")
  ("hf" zettelkasten-headline-set-followup "Set followup")
  ("hr" zettelkasten-headline-reset "Reset")
  ("hz" zettelkasten-node-to-zettel "Zettel")
  ("e" zettelkasten-generate-entity-from-activity-at-point "Entity generation")
  ("t" zettelkasten-derive-task-from-entity-at-point "Task derivation")

  ("n" org-noter "noter" :column "Other")
  ("u" zettelkasten-update-org-agenda-files "Update agenda")
  ("q" nil "Quit"))
;;; end:

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

;;; end:

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

;;;###autoload
(defun zettelkasten-zettel-add-collection (&optional collection)
  (interactive)
  (save-excursion
    (zettelkasten--zettel-ensure-keyword "COLLECTION")
    (insert (concat " "
                    (or collection
                        (completing-read
                         "Collection: "
                         (-flatten (zettelkasten-db-query
                                    [:select :distinct [collection]
                                             :from collection ])))))))
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
    (zettelkasten--zettel-ensure-keyword "INDEX")
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
          (ivy-read "Collection [Headline]: "
                    (-flatten (zettelkasten-db-query
                               [:select :distinct [collection]
                                        :from collection]))))
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

(provide 'zettelkasten-ext)
;;; zettelkasten.el ends here
