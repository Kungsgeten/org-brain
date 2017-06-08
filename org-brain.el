;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; Co-author (caching, storage, and extended visualize interface): https://github.com/analyticd
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25") (org "9"))
;; Version: 0.5

;;; Commentary:

;; org-brain implements a variant of concept mapping with org-mode, it is
;; inspired by The Brain software (http://thebrain.com). An org-brain is a
;; network of org-mode files, and you can get a visual overview of the
;; relationships between the files (kind of like a mind map): a file's parents
;; and its children. Files can also be browsed between via this overview.

;; All org files put into your `org-brain-path' directory will be considered as
;; "entries" (nodes, thoughts, pages, whatever you like to call them) in your
;; org-brain. An entry can link to other entries via an `org-mode' brain link,
;; for instance [[brain:index]]. You can also include search patterns in the
;; link, just like the `org-mode' file-link: [[brain:index::*Programming]].

;; You use `org-brain-visualize' to see the relationships between entries,
;; quickly add parents/children/pins/titles to an entry, and open them for
;; editing. In this view you may also have pinned entries, which will be shown
;; at all times. See the README file for more details.

;;; Code:

(require 'cl)
(require 'org-element)
(require 'org-attach)
(require 'picture)
(require 'subr-x)

(defgroup org-brain nil
  "Org-mode concept mapping"
  :prefix "org-brain-"
  :group 'org)

(defcustom org-brain-path (expand-file-name "brain" org-directory)
  "The root directory of your org-brain.

`org-mode' files placed in this directory, or its subdirectories,
will be considered org-brain entries."
  :group 'org-brain
  :type '(directory))

(defcustom org-brain-storage-directory (expand-file-name ".org-brain"
                                                         org-brain-path)
  "The directory where org-brain stores children, parents, and
  pins data. This directory will be automatically created, if it
  doesn't exist already, on the first run."
  :group 'org-brain
  :type '(directory))

(defcustom org-brain--children-file (convert-standard-filename
                                     (expand-file-name
                                      "org-brain-children.el"
                                      org-brain-storage-directory))
  "File to save the children cache into."
  :group 'org-brain
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                (org-brain--load-children)))))

(defcustom org-brain--parents-file (convert-standard-filename
                                     (expand-file-name
                                      "org-brain-parents.el"
                                      org-brain-storage-directory))
  "File to save the parents cache into."
  :group 'org-brain
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                (org-brain--load-parents)))))

(defcustom org-brain--pins-file (convert-standard-filename
                                     (expand-file-name
                                      "org-brain-pins.el"
                                      org-brain-storage-directory))
  "File to save the pins cache into."
  :group 'org-brain
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                (org-brain--load-pins)))))

(defcustom org-brain-link-type "brain"
  "The text used in the url of an org-brain link. This type of
  link is considered by org-brain to be neither a child or parent
  link, but is used much like a regular org link in the body of
  an org file.")

(defcustom org-brain-files-extension "org"
  "The extension for entry files in `org-brain-path'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-batch-separator ";"
  "When adding children and parents, this string allows for batch input."
  :group 'org-brain
  :type '(string))

;;; Utils
(defun org-brain--empty-string-p (string)
  "Return true if the STRING is empty or nil. Expects string type."
  (or (null string)
      (zerop (length (string-trim string)))))

;;; Macros used for serialization (hash-tables don't serialize in a elisp
;;; reaDable so we marshall to and from a list).
(defmacro org-brain--hash-to-list (hash-table)
  "Convert HASH-TABLE to list for storage in a file."
  (let ((nl (gensym)))
    `(let (,nl)
       (maphash (lambda (k v)
                  (push (list k v) ,nl)) ,hash-table)
       ,nl)))

(defmacro org-brain--list-to-hash (list)
  "Convert LIST to a hash-table for use in memory."
  (let ((ht (gensym)))
    `(let ((,ht (make-hash-table :test #'equal)))
       (mapc (lambda (item)
               (puthash (car item) (cadr item) ,ht)) ,list)
       ,ht)))

;;; Logging
(defcustom org-brain-log nil
  "Set to nil to not write to *Messages* buffer."
  :group 'org-brain
  :type 'boolean)

(defun org-brain-log (msg)
  "Write MSG to the *Messages* buffer."
  (when org-brain-log
    (setq inhibit-message t)
    (message msg)
    (setq inhibit-message nil)))

(defvar org-brain--visualizing-entry nil
  "The last entry argument to `org-brain-visualize'.")

;;; See note in code where this is used to understand why this is necessary.
(defvar org-brain--default-relative-path-expansion-directory org-brain-path
  "Directory to use for files with extension
  org-brain-files-extension that have no directory specified.
 Used by `ORG-BRAIN--HANDLE-RELATIVE-PATH'.")

(defvar org-brain--default-link-type-for-attachments "file+emacs"
  "Link type to use for clickable link to attachment.")

(org-link-set-parameters org-brain-link-type
                         :complete 'org-brain-link-complete
                         :follow 'org-brain-open
                         :activate-func 'org-brain-link-activate-func
                         :help-echo 'org-brain-link-tooltip)

;;; Caches
(defvar org-brain-files-cache nil "Cache for org-brain-files")
(defvar org-brain-parents-cache nil "Cache for org-brain-parents")
(defvar org-brain-children-cache nil "Cache for org-brain-children")
(defvar org-brain-pins-cache nil "Cache for org-brain-pins")

(defun org-brain-invalidate-files-cache ()
  "Set the org-brain-files-cache to nil."
  (org-brain-log "Invalidating org-brain file cache...")
  (setq org-brain-files-cache nil))

(defun org-brain-files (&optional relative)
  "Get all org files (recursively) in `org-brain-path'.
If RELATIVE is t, then return relative paths and remove org extension."
  (unless org-brain-files-cache
    (org-brain-log "Updating org-brain-files-cache...")
    (make-directory org-brain-path t)
    (setq org-brain-files-cache
          (directory-files-recursively
           org-brain-path (format "\\.%s$" org-brain-files-extension))))
  (if relative
      (mapcar #'org-brain-path-entry-name org-brain-files-cache)
    org-brain-files-cache))

(defun org-brain-pins ()
  "Get list of org-brain entries that are pinned."
  ;; Create the org-brain-storage-directory if it doesn't already exist.
  (org-brain-log "In org-brain-pins, about to make org-brain-storage-directory if needed.")
  (make-directory org-brain-storage-directory t)
  (org-brain--load-pins))

(defun org-brain-path-entry-name (path)
  "Get PATH as an org-brain entry name."
  (string-remove-suffix (concat "." org-brain-files-extension)
                        (file-relative-name path org-brain-path)))

(defun org-brain-entry-path (entry)
  "Get path of org-brain ENTRY."
  (expand-file-name (org-link-unescape
                     (format "%s.%s" entry org-brain-files-extension))
                    org-brain-path))

;;; Data serialization functions
(defun org-brain--save-data (file data)
  "Save lisp DATA, i.e., sexps, to FILE."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))  ; Allow circular data
      (prin1 data))))

(defun org-brain--load-data (file)
  "Load lisp data, i.e., sexps, from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun org-brain--save-children ()
  "Save the children. Write data into the file specified by
  `org-brain--children-file'."
  (org-brain--save-data org-brain--children-file
             (org-brain--hash-to-list org-brain-children-cache)))

(defun org-brain--save-parents ()
  "Save the parents. Write data into the file specified by
  `org-brain--parents-file'."
  (org-brain--save-data org-brain--parents-file
             (org-brain--hash-to-list org-brain-parents-cache)))

(defun org-brain--save-pins ()
  "Save the pins. Write data into the file specified by
  `org-brain--pins-file'."
  (org-brain--save-data org-brain--pins-file org-brain-pins-cache))

(defun org-brain--load-children ()
  "Load children cache from file."
  (setq org-brain-children-cache
        (org-brain--list-to-hash (org-brain--load-data org-brain--children-file))))

(defun org-brain--load-parents ()
  "Load parents cache from file."
  (setq org-brain-parents-cache
        (org-brain--list-to-hash (org-brain--load-data org-brain--parents-file))))

(defun org-brain--load-pins ()
  "Load pins cache from file."
  (setq org-brain-pins-cache (org-brain--load-data org-brain--pins-file)))

(defun org-brain-parents (entry &optional exclude)
  "Get list of org-brain parent entries linked to ENTRY.
You can choose to EXCLUDE an entry from the list."
  (org-brain--load-parents)
  (when (and org-brain-parents-cache
             (gethash entry org-brain-parents-cache))
    (cl-set-difference (gethash entry org-brain-parents-cache)
                       exclude :test #'equal)))

(defun org-brain-children (entry &optional exclude)
  "Get list of org-brain child entries linked to ENTRY.
You can choose to EXCLUDE an entry from the list."
  (org-brain--load-children)
  (when (and org-brain-children-cache
             (gethash entry org-brain-children-cache))
    (cl-set-difference (gethash entry org-brain-children-cache)
                       exclude :test #'equal)))

(defun org-brain-keywords (entry)
  "Get alist of `org-mode' keywords and their values in org-brain ENTRY."
  (with-temp-buffer
    (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (kw)
        (cons (org-element-property :key kw)
              (org-element-property :value kw))))))

(defun org-brain-title (entry)
  "Get title of ENTRY. Use entry name if no title keyword is provided."
  (or (cdr (assoc "TITLE" (org-brain-keywords entry)))
      (org-link-unescape (file-name-base entry))))

;;;###autoload
(defun org-brain-insert-link ()
  "Insert a link to an org-brain entry and suggest a description."
  (interactive)
  (let* ((file (completing-read "Entry: " (org-brain-files t)))
         (title (org-brain-title file))
         (desc (read-string "Description: " title)))
    (insert (org-make-link-string
             (concat org-brain-link-type ":" file) desc))))

;;;###autoload
(defun org-brain-agenda ()
  "Like `org-agenda', but only for `org-brain-files'."
  (interactive)
  (let ((org-agenda-files (org-brain-files)))
    (org-agenda)))

;;;###autoload
(defun org-brain-open (&optional entry)
  "Open ENTRY file in `org-brain-path'. Prompt for ENTRY if not given."
  (interactive)
  (let ((split-path
         (split-string
          (or entry (completing-read "Entry: " (org-brain-files t)))
                                  "::")))
    (org-open-file (org-brain-entry-path (car split-path))
                   t nil (cadr split-path))))

(defun org-brain-link-activate-func (start end path _bracketp)
  "Links to non-existing org-brain files should have a different face."
  (when (not (member (org-link-unescape (car (split-string path "::")))
                     (org-brain-files t)))
    (put-text-property start end 'face '(org-warning org-link))))

;;; TODO
(defun org-brain-link-complete ()
  "Create an org-brain-link-type link to a file in `org-brain-path'."
  (concat org-brain-child-link-type ":"
          (completing-read "Entry: " (org-brain-files t))))

(defun org-brain-link-tooltip (_window _object position)
  "Org-brain entry links have the entry's title as tooltip."
  (save-excursion
    (goto-char position)
    (org-brain-title
     (car (split-string
           (org-element-property :path (org-element-context)) "::")))))

(defun org-brain-add-child (entry child)
  "Add CHILD as child of ENTRY."
  (let ((entry-path (org-brain-entry-path entry)))
    (unless (file-exists-p entry-path)
      (with-temp-file entry-path
        (make-directory (file-name-directory entry-path) t))))
  ;; Create the cache if it doesn't yet exist, i.e., first run.
  (unless org-brain-children-cache
    (setf org-brain-children-cache (make-hash-table :test #'equal)))
  (org-brain-log (format "org-brain-children-cache: %s"
                         org-brain-children-cache))
  (let ((children (gethash entry org-brain-children-cache)))
    (push child children)
    (org-brain-log (format "entry: %s, children: %s" entry children))
    (setf (gethash entry org-brain-children-cache)
          children))
  (org-brain-log (format "After cache update, org-brain-children-cache: %s"
                         org-brain-children-cache))
  (org-brain--save-children))

(defun org-brain-add-parent (entry parent)
  "Add PARENT as parent of ENTRY."
  (let ((entry-path (org-brain-entry-path entry)))
    (unless (file-exists-p entry-path)
      (with-temp-file entry-path
        (make-directory (file-name-directory entry-path) t))))
  ;; Create the cache if it doesn't yet exist, i.e., first run.
  (unless org-brain-parents-cache
    (setf org-brain-parents-cache (make-hash-table :test #'equal)))
  (org-brain-log (format "org-brain-parents-cache: %s"
                         org-brain-parents-cache))
  (let ((parents (gethash entry org-brain-parents-cache)))
    (push parent parents)
    (org-brain-log (format "entry: %s, parents: %s" entry parents))
    (setf (gethash entry org-brain-parents-cache) parents))
  (org-brain-log (format "After cache update, org-brain-parents-cache: %s"
                         org-brain-parents-cache))
  (org-brain--save-parents))

(defun org-brain-remove-child (entry child)
  "In org-brain ENTRY, remove CHILD link. This doesn't delete the
  file pointed to by the link, just the link."
  (let ((children (gethash entry org-brain-children-cache)))
    (org-brain-log (format "before remove, children: %s" children))
    (setf (gethash entry org-brain-children-cache)
          (cl-remove child children :test #'equal))
    (org-brain-log (format "after remove, children: %s"
                           (gethash entry org-brain-children-cache))))
  (org-brain--save-children))

(defun org-brain-remove-parent (entry parent)
  "In org-brain ENTRY, remove PARENT link. This doesn't delete the
  file pointed to by the link, just the link."
  (let ((parents (gethash entry org-brain-parents-cache)))
    (org-brain-log (format "before remove, parents: %s" parents))
    (setf (gethash entry org-brain-parents-cache)
          (cl-remove parent parents :test #'equal))
    (org-brain-log (format "after remove, parents: %s"
                           (gethash entry org-brain-parents-cache))))
  (org-brain--save-parents))

(defun org-brain--insert-visualize-button (entry)
  "Insert a button, which runs `org-brain-visualize' on ENTRY when clicked."
  (insert-text-button
   (org-brain-title entry)
   'action (lambda (_x) (org-brain-visualize entry))
   'follow-link t))

;;; TODO
;;;###autoload
(defun org-brain-rename-entry (entry newname)
  "Rename org-brain ENTRY to NEWNAME.
If run interactively the user will be prompted for ENTRY and NEWNAME.

All links to ENTRY in `org-brain-path' files will be converted to
NEWENTRY. The ENTRY file will also be renamed. Version control is
also made aware of the change."
  (interactive
   (list (completing-read "Entry to rename: " (org-brain-files t) nil t
                          (when (equal major-mode 'org-brain-visualize-mode)
                            org-brain--visualizing-entry))
         (read-string "New name: ")))
  (let ((oldfile (org-brain-entry-path entry))
        (newfile (org-brain-entry-path newname)))
    (org-brain-invalidate-files-cache)  ; Invalidate cache
    (mapc
     (lambda (brainfile)
       (with-temp-buffer
         (insert-file-contents brainfile)
         (let ((data (org-element-parse-buffer)))
           (when (org-element-map data 'link
                   (lambda (link)
                     (let ((link-parts (split-string
                                        (org-element-property :path link)
                                        "::")))
                       (when (and (string-equal (car link-parts) entry)
                                  (string-equal
                                   (org-element-property :type link)
                                   "brain"))
                         (org-element-put-property
                          link :path (string-join
                                      (cons newname (cdr link-parts))
                                      "::"))))))
             (delete-region (point-min) (point-max))
             (insert (org-element-interpret-data data))
             (write-region nil nil brainfile)
             (when (get-file-buffer brainfile)
               (with-current-buffer (get-file-buffer brainfile)
                 (find-alternate-file brainfile)))))))
     (org-brain-files))
    (when (string-equal org-brain--visualizing-entry entry)
      (setq org-brain--visualizing-entry newname))
    (when (file-exists-p oldfile)
      (make-directory (file-name-directory newfile) t)
      (cond
       ((vc-backend oldfile) (vc-rename-file oldfile newfile))
       (t
        (rename-file oldfile newfile)
        (when (get-file-buffer oldfile)
          (with-current-buffer (get-file-buffer oldfile)
            (set-visited-file-name newfile t t)))))))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defcustom org-brain-ignored-resource-links
  '("fuzzy" "radio")
  "`org-link-types' which shouldn't be shown as resources in
  `org-brain-visualize'."
  :group 'org-brain
  :type '(repeat string))

;;; FIXME This works, but only goes to attachment directory level, not to the
;;; actual attachment files. Shortcoming of org or org-brain? Seems the former.
(defun org-brain--insert-attachment-files (entry headline)
  "Given an org buffer's ENTRY and HEADLINE, insert the list of
  files attached to the HEADLINE."
  (when (member "ATTACH" (org-element-property :tags headline))
    (org-brain-log (format "Entry attachment path: %s"
                   (file-name-directory (org-brain-entry-path entry))))
    (let* ((attach-dir org-attach-directory) ; Issue: non-absolute
                                             ; (org-attach-dir) error.
                                             ; Using org-attach-directory
                                             ; as workaround.
           (absolute-attach-dir (expand-file-name
                                 attach-dir
                                 (file-name-directory
                                  (org-brain-entry-path entry)))))
      (mapcar (lambda (attachment)
                (org-brain-log (format "attach-dir: %s, attachment: %s"
                                       absolute-attach-dir
                                       attachment))
                (org-brain--insert-resource-button
                 (format "%s:%s"
                         org-brain--default-link-type-for-attachments
                         (org-link-escape
                          (expand-file-name
                           attachment absolute-attach-dir)))
                 attachment
                 (1+ (org-element-property :level headline))))
              (org-attach-file-list absolute-attach-dir)))))

(defun org-brain--visualize-get-headline ()
  "Get headline at point in `org-brain--visualizing-entry'."
  (save-excursion
    (end-of-line)
    (re-search-backward "^\*+ *\\(.*\\)" nil t)
    (match-string 1)))

(defun org-brain-visualize-add-resource-link (link &optional description prompt)
  "Insert LINK with DESCRIPTION in `org-brain--visualizing-entry'.
Where to insert LINK is guessed with `org-brain--visualize-get-headline'.
If PROMPT is non nil, use `org-insert-link' even if not being run
interactively."
  (interactive "i")
  (if (not (eq major-mode 'org-brain-visualize-mode))
      (error "Not in org-brain-visualize-mode")
    (let ((heading (org-brain--visualize-get-headline))
          (position (point))
          (entry-path (org-brain-entry-path org-brain--visualizing-entry)))
      (with-temp-file entry-path
        (insert-file-contents entry-path)
        (delay-mode-hooks
          (org-mode)
          (goto-char (org-find-exact-headline-in-buffer heading))
          (when-let (property-block (org-get-property-block))
            (goto-char (cdr property-block)))
          (end-of-line)
          (insert "\n- ")
          (if (and link (not prompt))
              (insert (format "%s" (org-make-link-string link description)))
            (org-insert-link nil link description))))
      (when (get-file-buffer entry-path)
        (kill-buffer (get-file-buffer entry-path)))
      (revert-buffer)
      (goto-char position))))

(defun org-brain-visualize-paste-link ()
  "Add `current-kill' as a resource link."
  (interactive)
  (org-brain-visualize-add-resource-link (current-kill 0) nil t))

(defun org-brain-visualize-add-attachment ()
  "Add an attachment to `org-brain--visualize-get-headline'."
  (interactive)
  (if (not (eq major-mode 'org-brain-visualize-mode))
      (error "Not in org-brain-visualize-mode")
    (let* ((heading (org-brain--visualize-get-headline))
           (position (point))
           (entry-path (org-brain-entry-path org-brain--visualizing-entry))
           (existing-buffer (find-buffer-visiting entry-path)))
      (delay-mode-hooks
        (with-current-buffer (find-file entry-path)
          (goto-char (org-find-exact-headline-in-buffer heading))
          (call-interactively #'org-attach-attach)
          (save-buffer)
          (if existing-buffer
              (switch-to-buffer "*org-brain*")
            (kill-this-buffer)))
        (revert-buffer)
        (goto-char position)))))

(defun org-brain--insert-pinned-entries ()
  "Insert the pinned entries in the visualize interface."
  (insert "PINNED:")
  (mapc (lambda (pin)
          (insert "  ")
          (org-brain--insert-visualize-button pin))
        (org-brain-pins))
  (insert "\n\n\n"))

(defun org-brain--insert-entry-children (entry)
  "Insert ENTRY children into the visualize interface."
  (mapc (lambda (child)
          (let ((child-title (org-brain-title child)))
            (when (> (+ (current-column) (length child-title))
                     fill-column)
              (insert "\n"))
            (org-brain--insert-visualize-button child)
            (insert "  ")))
        (org-brain-children entry)))

(defun org-brain--insert-parent-and-sibling-entries
    (entry &optional ignored-siblings)
  "Insert parent and sibling entries for ENTRY into the visualize
  interface."
  (let ((parent-positions nil)
        (max-width 0))
    (mapc (lambda (parent)
            (let ((children (cl-set-difference
                             (org-brain-children parent)
                             ignored-siblings :test #'equal))
                  (col-start (+ 3 max-width))
                  (parent-title (org-brain-title parent)))
              (goto-line 4)
              (mapc
               (lambda (child)
                 (unless (string-equal entry child)
                   (picture-forward-column col-start)
                   (insert (make-string
                            (1+ (length parent-title)) ?\ ) "/ ")
                   (org-brain--insert-visualize-button child)
                   (setq max-width (max max-width (current-column)))
                   (newline (forward-line 1))))
               children)
              (goto-line 4)
              (forward-line (- (length children) 2))
              (picture-forward-column col-start)
              (push (cons (picture-current-line)
                          (+ (current-column) (/ (length parent-title) 2)))
                    parent-positions)
              (org-brain--insert-visualize-button parent)
              (setq max-width (max max-width (current-column)))
              (when children
                (ignore-errors (delete-char (length parent-title))))))
          (org-brain-parents entry))
    ;; Draw lines
    (when parent-positions
      (let ((maxline (line-number-at-pos (point-max))))
        ;; Bottom line
        (goto-line maxline)
        (picture-forward-column (cdar (last parent-positions)))
        (picture-move-down 1)
        (insert (make-string (1+ (- (cdar parent-positions)
                                    (cdar (last parent-positions))))
                             ?-))
        ;; Lines from parents to bottom
        (mapc (lambda (pos)
                (goto-line (car pos))
                (picture-forward-column (cdr pos))
                (while (< (line-number-at-pos (point))
                          maxline)
                  (picture-move-down 1)
                  (insert "|")
                  (unless (looking-at-p "\n") (delete-char 1)))
                (picture-move-down 1)
                (ignore-errors
                  (delete-char 1))
                (insert "*"))
              parent-positions)
        ;; Line to main entry
        (move-to-column (/ (+ (cdar (last parent-positions))
                              (cdar parent-positions))
                           2))
        (delete-char 1)
        (when (> (length parent-positions) 1)
          (insert "*")
          (backward-char 1)
          (picture-move-down 1)
          (insert "|")
          (picture-move-down 1))
        (insert "V")))))

(defun org-brain--link-description (link-contents)
  "Extract LINK description."
  (or (cadr link-contents) (car link-contents)))

(defun org-brain--insert-resource-button
    (raw-link link-description &optional indent)
  "Insert a new line with a resource button having target RAW-LINK
  and description LINK-DESCRIPTION, indented by INDENT spaces."
  (insert (make-string (or indent 0) ?\ ) "- ")
  (insert-text-button
   link-description
   'action (lambda (_x)
             (org-open-link-from-string raw-link))
   'follow-link t)
  (insert "\n"))

(defun org-brain--parsetree-for-entry (entry)
  "Return a parsetree for the ENTRY."
  (with-temp-buffer
    (ignore-errors
      (insert-file-contents (org-brain-entry-path entry)))
    (delay-mode-hooks
      (org-mode)
      (org-element-parse-buffer))))

(defun org-brain--handle-relative-path (raw-link)
  "Problem: building a link in org using C-c C-l RET file RET and
  choosing a file in the same directory results in a relative
  path that does not include a directory. Subsequently, when the
  *org-brain* interface is drawn, it isn't in the same context as
  the original because it is not associated with a file on disk.
  When a user then clicks on the link in the visualize interface,
  org-brain, really Emacs, treats such files as being in the
  user's home directory.

  Solution: If a RAW-LINK is of the form
  [[file:foo.<org-brain-files-extension>]] expand the RAW-LINK to use
  org-brain--default-relative-path-expansion-directory as the
  directory in the path."
  (if (and (string-equal (file-name-extension raw-link)
                         org-brain-files-extension)
           (not (> (length (split-string raw-link "/")) 1)))
      (concat (car (split-string raw-link ":"))
              ":"
              (expand-file-name
               (second (split-string raw-link ":"))
               org-brain--default-relative-path-expansion-directory))
    raw-link))

(defun org-brain--insert-resources (headline)
  "Draw child links for this particular HEADLINE (do not include
  links that might be children of child headlines of this
  HEADLINE)."
  (org-element-map (org-element-contents headline) 'link
    (lambda (link)
      (let* ((raw-link (org-element-property :raw-link link))
             (link-contents (car (org-element-contents link)))
             (description (org-brain--link-description
                           (list raw-link
                                 link-contents))))
        (if (and description
                 (char-or-string-p description) ; Temp fix: handle org parser
                                                ; bug.
                 (not (string-equal description ","))) ; Temp fix: handle org
                                                       ; parser bug.
            (org-brain--insert-resource-button
             (org-brain--handle-relative-path raw-link)
             description
             (1+ (org-element-property :level headline)))
          (unless (string-equal raw-link ",") ; Temp fix: handle org
                                              ; parser bug.
            (org-brain--insert-resource-button
             (org-brain--handle-relative-path raw-link)
             (org-link-unescape raw-link)
             (1+ (org-element-property :level headline)))))))
    nil nil 'headline))         ; No recursion on headline, i.e., just
                                ; get the links for the current
                                ; headline, but not any of its children
                                ; headline's links.

(defun org-brain--insert-headline (headline headline-title entry-path)
  "Insert text button for HEADLINE with title HEAD-TITLE and
  target ENTRY-PATH."
  (insert (make-string (org-element-property :level headline) ?*) " ")
  (insert-text-button
   headline-title
   'action (lambda (_x)
             (org-open-file entry-path
                            nil nil
                            (concat "*" headline-title)))
   'follow-link t)
  (insert "\n"))

(defun org-brain--insert-headlines-and-resources (entry)
  "Insert a horizontal separator followed by the headlines and
  resources for the ENTRY in the visualize interface."
  (insert "\n\n-----------------------------------------------\n\n")
  (org-element-map (org-brain--parsetree-for-entry entry) 'headline
    (lambda (headline)
      (let* ((head-title (org-element-property :raw-value headline))
             (entry-path (org-brain-entry-path entry)))
        ;; Draw headline
        (org-brain--insert-headline headline head-title entry-path)
        ;; Draw headline's attachments
        (org-brain--insert-attachment-files entry headline)
        ;; Draw headline's links
        (org-brain--insert-resources headline)))
    (insert "\n")))

;;;###autoload
(defun org-brain-visualize (entry &optional ignored-siblings nofocus)
  "View a concept map with ENTRY at the center.
IGNORED-SIBLINGS, a list of org-brain entries, can be provided to
ignore certain sibling links to show. Unless NOFOCUS is non-nil,
the concept map buffer will gain focus."
  (interactive
   (list (completing-read
          "Entry: " (org-brain-files t) nil nil
          (when (and (eq major-mode 'org-mode)
                     (member (buffer-file-name)
                             (org-brain-files)))
            (org-brain-path-entry-name (buffer-file-name))))))
  (with-current-buffer (get-buffer-create "*org-brain*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (org-brain--insert-pinned-entries)
    (org-brain--insert-parent-and-sibling-entries entry ignored-siblings)
    ;; Insert main entry name
    (picture-move-down 1)
    (let ((half-title-length (/ (length (org-brain-title entry)) 2)))
      (if (>= half-title-length (current-column))
          (delete-char (- (current-column)))
        (ignore-errors
          (delete-char (- half-title-length)))))
    (let ((entry-pos (point)))
      (insert (org-brain-title entry) "\n\n")
      (org-brain--insert-entry-children entry)
      (org-brain--insert-headlines-and-resources entry)
      ;; Finishing
      (org-brain-visualize-mode)
      (goto-char entry-pos)
      (unless nofocus (pop-to-buffer "*org-brain*"))))
  (setq org-brain--visualizing-entry entry))

(defun org-brain-visualize-revert (_ignore-auto _noconfirm)
  "Revert function for `org-brain-visualize-mode'."
  (org-brain-visualize org-brain--visualizing-entry nil t))

(defun org-brain-visualize-open ()
  "Open the entry file last visited by `org-brain-visualize'."
  (interactive)
  (org-brain-open org-brain--visualizing-entry))

(defun org-brain-visualize-add-child (child)
  "Add CHILD link to entry last visited by `org-brain-visualize'.
CHILD can hold multiple entries, by using `org-brain-batch-separator'."
  (interactive
   (list (completing-read "Child: " (org-brain-files t))))
  (org-brain-invalidate-files-cache)    ; Invalidate cache
  (dolist (c (split-string child org-brain-batch-separator t " +"))
    (org-brain-add-child org-brain--visualizing-entry c))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-remove-child (child)
  "Prompt user for child(ren) of entry last visited by
  `org-brain-visualize' to remove and then remove it/them. This
  does not delete the file pointed to by the child link."
  (interactive
   (list (completing-read "Child: "
                          (org-brain-children org-brain--visualizing-entry))))
  (dolist (c (split-string child org-brain-batch-separator t " +"))
    (org-brain-remove-child org-brain--visualizing-entry c))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-add-parent (parent)
  "In PARENT add link to entry last visited by `org-brain-visualize'.
PARENT can hold multiple entries, by using `org-brain-batch-separator'."
  (interactive
   (list (completing-read "Parent: " (org-brain-files t))))
  (org-brain-invalidate-files-cache)    ; Invalidate cache
  (dolist (p (split-string parent org-brain-batch-separator t " +"))
    (org-brain-add-parent org-brain--visualizing-entry p))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-remove-parent (parent)
  "In PARENT remove link to entry last visited by `org-brain-visualize'.
PARENT can hold multiple entries, by using `org-brain-batch-separator'."
  (interactive
   (list (completing-read "Parent: "
                          (org-brain-parents org-brain--visualizing-entry))))
  (dolist (p (split-string parent org-brain-batch-separator t " +"))
    (org-brain-remove-parent org-brain--visualizing-entry p))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-add-pin ()
  "Pin `ORG-BRAIN--VISUALIZING-ENTRY.'"
  (interactive)
  (org-brain-log "In org-brain-visualize-add-pin...")
  (org-brain-add-pin org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-add-pin (entry)
  "Pin ENTRY."
  (unless (cl-position entry org-brain-pins-cache :test #'equal)
    (push entry org-brain-pins-cache))
  (org-brain-log (format "After pinning entry: %s, org-brain-pins-cache: %s"
                         entry org-brain-pins-cache))
  (org-brain--save-pins))

(defun org-brain-visualize-remove-pin ()
  "Unpin `ORG-BRAIN--VISUALIZING-ENTRY.'"
  (interactive)
  (org-brain-remove-pin org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-remove-pin (entry)
  "Unpin ENTRY."
  (org-brain-log
   (format "Before unpinning entry: %s, org-brain-pins-cache: %s"
           entry org-brain-pins-cache))
  (setf org-brain-pins-cache
        (cl-remove entry org-brain-pins-cache :test #'equal))
  (format "After unpinning entry: %s, org-brain-pins-cache: %s"
           entry org-brain-pins-cache)
  (org-brain--save-pins))

(defun org-brain-visualize-add-or-change-title ()
  "In current org-brain ENTRY, add \"#+TITLE:\" with title value acquired
  and required from user."
  (interactive)
  (let ((title (read-string "Title: ")))
    (loop while (org-brain--empty-string-p title) do
          (setq title (read-string
                       "Title must have a value, please enter title: ")))
    (org-brain-add-or-change-title title org-brain--visualizing-entry)
    (when (string-equal (buffer-name) "*org-brain*")
      (revert-buffer))))

(defun org-brain-add-or-change-title (title entry)
  "In the ENTRY, add the TITLE. If the ENTRY already has a TITLE,
  first remove it and then add the new TITLE."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (with-current-buffer (find-file-noselect entry-path)
      (if (not (assoc "TITLE" (org-brain-keywords entry)))
          (progn
            (goto-char (point-min))
            (insert (format "#+TITLE: %s\n" title))
            (save-buffer))
        ;; Remove #+TITLE: ... and create new one
          (goto-char (point-min))
          (re-search-forward "^#\\+TITLE: +.*$")
          (beginning-of-line)
          (when (looking-at "^#\\+TITLE: +.*$")
            (kill-line)
            (goto-char (point-min))
            (insert (format "#+TITLE: %s\n" title))
            (save-buffer))))))

(defun org-brain-visualize-remove-title ()
  "Remove \"#+TITLE:\" line from entry last visited by
  `org-brain-visualize' if it exists."
  (interactive)
  (org-brain-remove-title org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-remove-title (entry)
  "In org-brain ENTRY, remove \"#+TITLE:\" if it exists."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (with-current-buffer (find-file-noselect entry-path)
      (when (assoc "TITLE" (org-brain-keywords entry))
        (goto-char (point-min))
        (re-search-forward "^#\\+TITLE:.*$")
        (beginning-of-line)
        (when (looking-at "^#\\+TITLE:.*$")
            (kill-line)
            (save-buffer))))))

(defun org-brain-visualize-search-entries (entry)
  "Search for headline in `ORG-BRAIN--VISUALIZING-ENTRY' and upon
  selection in completing read, open the headline in the
  underlying file."
  (interactive
   (list (completing-read
          "Entry: "
          (org-element-map
              (with-temp-buffer
                (ignore-errors
                  (insert-file-contents
                   (org-brain-entry-path org-brain--visualizing-entry)))
                (delay-mode-hooks
                  (org-mode)
                  (org-element-parse-buffer)))
              'headline
            (lambda (headline)
              (org-element-property :title headline))))))
  (org-brain-log (format "Visualizing entry: %s" org-brain--visualizing-entry))
  (org-brain-log (format "Entry to search for: %s" entry))
  (re-search-forward (format "^\*+ *%s" entry))
  (backward-char 1)
  (push-button))

(defun org-brain-visualize-search-links (link)
  "Search for link in `ORG-BRAIN--VISUALIZING-ENTRY' and upon
  selection in completing read, open the link in the
  underlying file."
  (interactive
   (list (completing-read
          "Link: "
          (org-element-map
              (with-temp-buffer
                (ignore-errors
                  (insert-file-contents
                   (org-brain-entry-path org-brain--visualizing-entry)))
                (delay-mode-hooks
                  (org-mode)
                  (org-element-parse-buffer)))
              'link
            (lambda (link)
              (let* ((raw-link (org-element-property :raw-link link))
                     (link-contents (car (org-element-contents link)))
                     (description (org-brain--link-description
                                   (list raw-link
                                         link-contents))))
                (if (and description
                         (char-or-string-p description) ; Temp fix: handle org
                                                     ; parser bug.
                         (not (string-equal description ","))) ; Temp fix:
                                                           ; handle org
                                                           ; parser bug.
                    description
                  (unless (string-equal raw-link ",") ; Temp fix: handle org
                                                  ; parser bug.
                     raw-link))))))))
  (org-brain-log (format "Visualizing entry: %s" org-brain--visualizing-entry))
  (org-brain-log (format "Link to search for: %s" link))
  (when (re-search-forward (format "^ +- *%s\\|^*+ +\\[\\[.*\\]\\[%s"
                                   link link))
    (backward-char 1)
    (push-button)))

(defun org-brain-visualize-search-string-in-brain ()
  "Prompt user for search string and conduct search in
  `ORG-BRAIN-PATH' using the AG package. Gracefully inform the
  user to install AG if it isn't currently installed."
  (interactive)
  (let ((string (read-string
                 (format "Enter the string to search for in files in your org-brain-path (%s): " org-brain-path))))
    (if (featurep 'ag)
        (ag string org-brain-path)
      (message "Install and configure package ag to use this functionality. E.g., (package-install ag)"))))

(defun org-brain-visualize-org-brain-path-directory ()
  "Open `ORG-BRAIN-PATH' directory using DIRED."
  (interactive)
  (find-file-other-window org-brain-path))

(define-derived-mode org-brain-visualize-mode
  special-mode  "Org-brain Visualize"
  "Major mode for `org-brain-visualize'.
\\{org-brain-visualize-mode-map}"
  (setq revert-buffer-function #'org-brain-visualize-revert))

(define-key org-brain-visualize-mode-map "p" 'org-brain-visualize-add-parent)
(define-key org-brain-visualize-mode-map "P" 'org-brain-visualize-remove-parent)
(define-key org-brain-visualize-mode-map "c" 'org-brain-visualize-add-child)
(define-key org-brain-visualize-mode-map "C" 'org-brain-visualize-remove-child)
(define-key org-brain-visualize-mode-map "n" 'org-brain-visualize-add-pin)
(define-key org-brain-visualize-mode-map "N" 'org-brain-visualize-remove-pin)
(define-key org-brain-visualize-mode-map "t" 'org-brain-visualize-add-or-change-title)
(define-key org-brain-visualize-mode-map "T" 'org-brain-visualize-remove-title)
(define-key org-brain-visualize-mode-map "j" 'forward-button)
(define-key org-brain-visualize-mode-map "k" 'backward-button)
(define-key org-brain-visualize-mode-map [?\t] 'forward-button)
(define-key org-brain-visualize-mode-map [backtab] 'backward-button)
(define-key org-brain-visualize-mode-map "o" 'org-brain-visualize-open)
(define-key org-brain-visualize-mode-map "f" 'org-brain-visualize)
(define-key org-brain-visualize-mode-map "r" 'org-brain-rename-entry)
(define-key org-brain-visualize-mode-map "l" 'org-brain-visualize-add-resource-link)
(define-key org-brain-visualize-mode-map "a" 'org-brain-visualize-add-attachment)
(define-key org-brain-visualize-mode-map "\C-y" 'org-brain-visualize-paste-link)
(define-key org-brain-visualize-mode-map "s" 'org-brain-visualize-search-entries)
(define-key org-brain-visualize-mode-map "S" 'org-brain-visualize-search-links)
(define-key org-brain-visualize-mode-map (kbd "C-c s") 'org-brain-visualize-search-string-in-brain)
(define-key org-brain-visualize-mode-map "d" 'org-brain-visualize-org-brain-path-directory)

(provide 'org-brain)
;;; org-brain.el ends here
