;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; Co-author (caching and extended visualize interface): https://github.com/analyticd
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25") (org "9"))
;; Version: 0.4

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

(defcustom org-brain-children-headline-default-name "Brainchildren"
  "Default name for a headline containing links to org-brain children entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-children-tag-default-name "brainchildren"
  "Default name for a tag on headline containing links to
  org-brain children entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-parents-headline-default-name "Brainparents"
  "Default name for a headline containing links to org-brain parent entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-parents-tag-default-name "brainparents"
  "Default name for a tag on headline containing links to
  org-brain parent entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-simple-link-type "brain"
  "The text used in the url of an org-brain link. This type of
  link is considered by org-brain to be neither a child or parent
  link, but is used much like a regular org link in the body of
  an org file. Used by `org-link-set-parameters'")

(defcustom org-brain-child-link-type "brainchild"
  "The text used in the url of an org-brain child link. Used by
  `org-link-set-parameters'")

(defcustom org-brain-parent-link-type "brainparent"
  "The text used in the url of an org-brain parent link. Used by
  `org-link-set-parameters'")

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

;;; Link types
(org-link-set-parameters org-brain-simple-link-type
                         :complete 'org-brain-child-link-complete
                         :follow 'org-brain-open
                         :activate-func 'org-brain-link-activate-func
                         :help-echo 'org-brain-link-tooltip)

(org-link-set-parameters org-brain-child-link-type
                         :complete 'org-brain-child-link-complete
                         :follow 'org-brain-open
                         :activate-func 'org-brain-link-activate-func
                         :help-echo 'org-brain-link-tooltip)

(org-link-set-parameters org-brain-parent-link-type
                         :complete 'org-brain-parent-link-complete
                         :follow 'org-brain-open
                         :activate-func 'org-brain-link-activate-func
                         :help-echo 'org-brain-link-tooltip)

;;; Caches
(defvar org-brain-files-cache nil "Cache for org-brain-files")
(defvar org-brain-parents-cache nil "Cache for org-brain-parents")
(defvar org-brain-children-cache nil "Cache for org-brain-children")
(defvar org-brain-pins-cache nil "Cache for org-brain-pins")

;;;###autoload
(defun org-brain-invalidate-all-caches ()
  "This is a convenience function for those who (occasionally)
  edit children, parents, or pins manually outside the
  org-brain-visualize interface. In that case, you have to call
  this function manually. It is not needed if children, parents,
  related, and pins are added using the org-brain-visualize
  interface/mode."
  (interactive)
  (setq org-brain-files-cache nil)
  (setq org-brain-parents-cache nil)
  (setq org-brain-children-cache nil)
  (setq org-brain-pins-cache nil))

(defun org-brain-invalidate-files-cache ()
  "Set the org-brain-files-cache to nil."
  (org-brain-log "Invalidating org-brain file cache...")
  (setq org-brain-files-cache nil))

(defun org-brain-invalidate-parent-cache-entry (entry)
  "Set the cache element keyed by ENTRY to nil in the org-brain-parents-cache."
  (org-brain-log
   (format "Invalidating org-brain parent cache entry: %s ..." entry))
  (setq org-brain-parents-cache
        (cl-remove entry org-brain-parents-cache :test #'equal :key #'car)))

(defun org-brain-invalidate-child-cache-entry (entry)
  "Set the cache element keyed by ENTRY to nil in the org-brain-children-cache."
  (org-brain-log
   (format "Invalidating org-brain child cache entry: %s ..." entry))
  (setq org-brain-children-cache
        (cl-remove entry org-brain-children-cache :test #'equal :key #'car)))

(defun org-brain-invalidate-pins-cache ()
  "Set the org-brain-pins-cache to nil."
  (org-brain-log "Invalidating org-brain pin cache...")
  (setq org-brain-pins-cache nil))

;;;###autoload
(defun org-brain-build-caches ()
  "(Optional) It is not necessary to use this function as the
  caches are built lazily, automatically. However, this is just
  here if you want to do some cache building ahead of time, for
  instance during Emacs startup (at the cost of a longer Emacs
  startup) while you grab your coffee."
  (interactive)
  (org-brain-log "Eagerly building some of the org-brain caches..")
  (org-brain-files)
  (org-brain-pins))

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
  "Get list of org-brain entries with \"BRAIN_PIN\" keyword."
  (or org-brain-pins-cache
      (progn
        (org-brain-log "Updating org-brain-pins-cache...")
        (setq org-brain-pins-cache
              (remove nil
                      (mapcar
                       (lambda (entry)
                         (when (assoc "BRAIN_PIN" (org-brain-keywords entry))
                           entry))
                       (org-brain-files t)))))))

(defun org-brain-path-entry-name (path)
  "Get PATH as an org-brain entry name."
  (string-remove-suffix (concat "." org-brain-files-extension)
                        (file-relative-name path org-brain-path)))

(defun org-brain-entry-path (entry)
  "Get path of org-brain ENTRY."
  (expand-file-name (org-link-unescape
                     (format "%s.%s" entry org-brain-files-extension))
                    org-brain-path))

(defun org-brain-parents (entry &optional exclude)
  "Get list of org-brain parent entries linked to ENTRY.
You can choose to EXCLUDE an entry from the list."
  (if (and org-brain-parents-cache
           (assoc entry org-brain-parents-cache))
      (cdr (assoc entry org-brain-parents-cache))
    (org-brain-log (format "Updating org-brain-parents-cache for %s..." entry))
    (let ((parents
           (delete
            exclude
            (car
             (org-element-map (org-brain--parsetree-for-entry entry) 'headline
               (lambda (headline)
                 ;; Only look for parents that are attached to the
                 ;; org-brain-parents-headline-default-name headline.
                 (when (string-equal (org-element-property :raw-value headline)
                                     org-brain-parents-headline-default-name)
                   (org-element-map (org-element-contents headline) 'link
                     (lambda (link)
                       ;; Restrict to only org-brain-parent-link-type links.
                       (when (string-equal (org-element-property :type link)
                                           org-brain-parent-link-type)
                         (car (split-string
                               (org-element-property :path link)
                               "::"))))
                     nil nil 'headline))))))))  ; Only get links for this
                                                ; headline and not for
                                                ; children headlines, i.e.,
                                                ; don't recursive on headlines
                                                ; for links.
      (push (cons entry . (parents)) org-brain-parents-cache)
      (cdr (assoc entry org-brain-parents-cache)))))

(defun org-brain-children (entry &optional exclude)
  "Get list of org-brain child entries linked to ENTRY.
You can choose to EXCLUDE an entry from the list."
  (if (and org-brain-children-cache
           (assoc entry org-brain-children-cache))
      (cdr (assoc entry org-brain-children-cache))
    (org-brain-log (format "Updating org-brain-children-cache for %s..." entry))
    (let ((children
           (delete
            exclude
            (car
              (org-element-map (org-brain--parsetree-for-entry entry) 'headline
                (lambda (headline)
                  ;; Only look for children that are attached to the
                  ;; org-brain-children-headline-default-name headline.
                  (when (string-equal (org-element-property :raw-value headline)
                                      org-brain-children-headline-default-name)
                    (org-element-map (org-element-contents headline) 'link
                      (lambda (link)
                       ;; Restrict to only org-brain-child-link-type links.
                        (when (string-equal (org-element-property :type link)
                                            org-brain-child-link-type)
                          (car (split-string
                                (org-element-property :path link)
                                "::"))))
                      nil nil 'headline))))))))  ; Only get links for this
                                                 ; headline and not for
                                                 ; children headlines, i.e.,
                                                 ; don't recursive on headlines
                                                 ; for links.
      (push (cons entry . (children)) org-brain-children-cache)
      (cdr (assoc entry org-brain-children-cache)))))

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
(defun org-brain-insert-child-link ()
  "Insert a link to an org-brain child entry and suggest a description."
  (interactive)
  (let* ((file (completing-read "Entry: " (org-brain-files t)))
         (title (org-brain-title file))
         (desc (read-string "Description: " title)))
    (insert (org-make-link-string
             (concat org-brain-child-link-type ":" file) desc))))

;;;###autoload
(defun org-brain-insert-parent-link ()
  "Insert a link to an org-brain parent entry and suggest a description."
  (interactive)
  (let* ((file (completing-read "Entry: " (org-brain-files t)))
         (title (org-brain-title file))
         (desc (read-string "Description: " title)))
    (insert (org-make-link-string
             (concat org-brain-parent-link-type ":" file) desc))))

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

(defun org-brain-child-link-complete ()
  "Create an org-brain brainchild org-link target string to a
  file in `org-brain-path'."
  (concat org-brain-child-link-type ":"
          (completing-read "Entry: " (org-brain-files t))))

(defun org-brain-parent-link-complete ()
  "Create an org-brain brainparent org-link target string to a
  file in `org-brain-path'."
  (concat org-brain-parent-link-type ":"
          (completing-read "Entry: " (org-brain-files t))))

(defun org-brain-link-tooltip (_window _object position)
  "Org-brain entry links have the entry's title as tooltip."
  (save-excursion
    (goto-char position)
    (org-brain-title
     (car (split-string
           (org-element-property :path (org-element-context)) "::")))))

(defun org-brain-add-child (entry child)
  "In org-brain ENTRY, add a link to CHILD."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (unless (file-exists-p entry-path)
      (with-temp-file entry-path
        (make-directory (file-name-directory entry-path) t)))
    (with-current-buffer (find-file-noselect entry-path)
      (goto-char (point-min))
      (save-excursion
        (if (re-search-forward
               (format "^\\*.*:%s:.*$"
                       org-brain-children-tag-default-name)
               nil t)
            (progn
              (end-of-line)
              (insert (format "\n- [[%s:%s][%s]]"
                             org-brain-child-link-type
                             child
                             (org-brain-title child)))
              (save-buffer))
          (goto-char (point-max))
          (insert (format "\n\n* %s    :%s:\n- [[%s:%s][%s]]"
                          org-brain-children-headline-default-name
                          org-brain-children-tag-default-name
                          org-brain-child-link-type
                          child
                          (org-brain-title child)))
          (save-buffer))))))

(defun org-brain-add-parent (entry parent)
  "In org-brain ENTRY, add a link to PARENT."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (unless (file-exists-p entry-path)
      (with-temp-file entry-path
        (make-directory (file-name-directory entry-path) t)))
    (with-current-buffer (find-file-noselect entry-path)
      (goto-char (point-min))
      (save-excursion
        (if (re-search-forward
               (format "^\\*.*:%s:.*$"
                       org-brain-parents-tag-default-name)
               nil t)
            (progn
              (end-of-line)
              (insert (format "\n- [[%s:%s][%s]]"
                            org-brain-parent-link-type
                            parent
                            (org-brain-title parent)))
              (save-buffer))
          (goto-char (point-max))
          (insert (format "\n\n* %s    :%s:\n- [[%s:%s][%s]]"
                          org-brain-parents-headline-default-name
                          org-brain-parents-tag-default-name
                          org-brain-parent-link-type
                          parent
                          (org-brain-title parent)))
          (save-buffer))))))

(defun org-brain-remove-child (entry child)
  "In org-brain ENTRY, remove CHILD link. This doesn't delete the
  file pointed to by the link, just the link."
  (let ((entry-path (org-brain-entry-path entry))
        (child-regex (format "^ *- \\[\\[%s:%s.*$"
                             org-brain-child-link-type
                             child)))
    (org-save-all-org-buffers)
    (org-brain-invalidate-child-cache-entry entry)
    (with-current-buffer (find-file-noselect entry-path)
      (goto-char (point-min))
      (save-excursion
        (re-search-forward
         (format "^\\*.*:%s:.*$" org-brain-children-tag-default-name) nil t)
        (beginning-of-line)
        (re-search-forward child-regex nil t)
        (beginning-of-line)
        (looking-at child-regex)
        (kill-line 1)
        (save-buffer)))))

(defun org-brain-remove-parent (entry parent)
  "In org-brain ENTRY, remove PARENT link. This doesn't delete the
  file pointed to by the link, just the link."
  (let ((entry-path (org-brain-entry-path entry))
        (parent-regex (format "^ *- \\[\\[%s:%s.*$"
                              org-brain-parent-link-type
                              parent)))
    (org-save-all-org-buffers)
    (org-brain-invalidate-parent-cache-entry entry)
    (with-current-buffer (find-file-noselect entry-path)
      (goto-char (point-min))
      (save-excursion
        (re-search-forward
         (format "^\\*.*:%s:.*$" org-brain-parents-tag-default-name) nil t)
        (beginning-of-line)
        (re-search-forward parent-regex nil t)
        (beginning-of-line)
        (looking-at parent-regex)
        (kill-line 1)
        (save-buffer)))))

(defun org-brain--insert-visualize-button (entry)
  "Insert a button, which runs `org-brain-visualize' on ENTRY when clicked."
  (insert-text-button
   (org-brain-title entry)
   'action (lambda (_x) (org-brain-visualize entry))
   'follow-link t))

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
  '("brain" "brainchild" "brainparent" "fuzzy" "radio")
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
    (org-brain-log (format "entry path: %s"
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
  "Get a headline at point in `org-brain--visualizing-entry'.
If no headline is found, use `org-brain-children-headline-default-name'."
  (org-brain-log "In org-brain--visualize-get-headline...")
  (save-excursion
    (end-of-line)
    (let ((entry-path (org-brain-entry-path org-brain--visualizing-entry)))
      (if (re-search-backward "^\*+ *\\(.*\\)" nil t)
          (match-string 1)
        (org-save-all-org-buffers)
        (unless (file-exists-p entry-path)
          (with-temp-file entry-path
            (make-directory (file-name-directory entry-path) t)))
        (or (car (org-map-entries (lambda () (org-get-heading t t))
                                  (format "+%s"
                                          org-brain-children-tag-default-name)
                                  (list entry-path)))
            (with-current-buffer (get-file-buffer entry-path)
              (goto-char (point-max))
              (insert (format "\n\n* %s    :%s:"
                              org-brain-children-headline-default-name
                              org-brain-children-tag-default-name))
              (save-buffer)
              (org-get-heading t t)))))))

(defun org-brain-visualize-add-resource-link (link &optional description prompt)
  "Insert LINK with DESCRIPTION in `org-brain--visualizing-entry'.
Where to insert LINK is guessed with `org-brain--visualize-get-headline'.
If PROMPT is non nil, use `org-insert-link' even if not being run interactively."
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
            (let ((children (set-difference
                             (org-brain-children parent)
                             ignored-siblings))
                  (col-start (+ 3 max-width))
                  (parent-title (org-brain-title parent)))
              (goto-line 4)
              (mapc
               (lambda (child)
                 (picture-forward-column col-start)
                 (insert (make-string
                          (1+ (length parent-title)) ?\ ) "/ ")
                 (org-brain--insert-visualize-button child)
                 (setq max-width (max max-width (current-column)))
                 (newline (forward-line 1)))
               children)
              (goto-line 4)
              (forward-line (1- (length children)))
              (picture-forward-column col-start)
              (push (cons (picture-current-line)
                          (+ (current-column) (/ (length parent-title) 2)))
                    parent-positions)
              (org-brain--insert-visualize-button parent)
              (setq max-width (max max-width (current-column)))
              (when children
                (delete-char (length parent-title)))))
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
                 (char-or-string-p description) ; Temp fix for bug in org parser
                 (not (string-equal description ","))) ; Handle another org
                                                       ; parser bug. Parser
                                                       ; thinks "," is a
                                                       ; link when used in e.g.,
                                                       ; or i.e.,
            (org-brain--insert-resource-button
             (org-brain--handle-relative-path raw-link)
             description
             (1+ (org-element-property :level headline)))
          (org-brain-log (format "using raw-link: %s as description"
                                 raw-link))
          (org-brain--insert-resource-button
           (org-brain--handle-relative-path raw-link)
           raw-link
           (1+ (org-element-property :level headline))))))
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
        ;; Don't show Brainchildren or Brainparents headlines
        (unless (member head-title
                        (list org-brain-children-headline-default-name
                              org-brain-parents-headline-default-name))
          ;; Draw headline
          (org-brain--insert-headline headline head-title entry-path)
          ;; Draw headline's attachments
          (org-brain--insert-attachment-files entry headline)
          ;; Draw headline's links
          (org-brain--insert-resources headline))))
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
  (org-brain-invalidate-child-cache-entry
   org-brain--visualizing-entry)        ; Invalidate cache
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
  (org-brain-invalidate-files-cache)    ; Invalidate cache
  (org-brain-invalidate-child-cache-entry
   org-brain--visualizing-entry)        ; Invalidate cache
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
  (org-brain-invalidate-parent-cache-entry
   org-brain--visualizing-entry)        ; Invalidate cache
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
  (org-brain-invalidate-files-cache)    ; Invalidate cache
  (org-brain-invalidate-parent-cache-entry
   org-brain--visualizing-entry)        ; Invalidate cache
  (dolist (p (split-string parent org-brain-batch-separator t " +"))
    (org-brain-remove-parent org-brain--visualizing-entry p))
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-add-pin ()
  "Add \"#+BRAIN_PIN:\" to entry last visited by
  `org-brain-visualize' if it doesn't already exist."
  (interactive)
  (org-brain-invalidate-pins-cache)    ; Invalidate cache
  (org-brain-add-pin org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-add-pin (entry)
  "In org-brain ENTRY, add \"#+BRAIN_PIN:\"."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (with-current-buffer (find-file-noselect entry-path)
      (when (not (assoc "BRAIN_PIN" (org-brain-keywords entry)))
        (goto-char (point-min))
        (insert "#+BRAIN_PIN:\n")
        (save-buffer)))))

(defun org-brain-visualize-remove-pin ()
  "Remove \"#+BRAIN_PIN:\" from entry last visited by
  `org-brain-visualize' if it exists."
  (interactive)
  (org-brain-invalidate-pins-cache)    ; Invalidate cache
  (org-brain-remove-pin org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-remove-pin (entry)
  "In org-brain ENTRY, remove \"#+BRAIN_PIN:\" if it exists."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (with-current-buffer (find-file-noselect entry-path)
      (when (assoc "BRAIN_PIN" (org-brain-keywords entry))
        (goto-char (point-min))
        (re-search-forward "^#\\+BRAIN_PIN:.*$")
        (beginning-of-line)
        (when (looking-at "^#\\+BRAIN_PIN:.*$")
            (kill-line)
            (save-buffer))))))

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

(provide 'org-brain)
;;; org-brain.el ends here
