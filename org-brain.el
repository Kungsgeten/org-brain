;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25") (org "9"))
;; Version: 0.4

;;; Commentary:

;; org-brain implements a variant of concept mapping with org-mode, it is
;; inspired by The Brain software (http://thebrain.com). An org-brain is a
;; network of org-mode entries, where each entry is a file or a headline, and
;; you can get a visual overview of the relationships between the entries:
;; parents, children, siblings and friends. This visual overview can also be
;; used to browse your entries. You can think of entries as nodes in a mind map,
;; or pages in a wiki.

;; All org files put into your `org-brain-path' directory will be considered
;; entries in your org-brain. Headlines with an ID property in your entry file(s)
;; are also considered as entries.

;; Use `org-brain-visualize' to see the relationships between entries, quickly
;; add parents/children/friends/pins to an entry, and open them for editing.

;;; Code:

(require 'org-element)
(require 'org-attach)
(require 'org-id)
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

(defcustom org-brain-files-extension "org"
  "The extension for entry files in `org-brain-path'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-ignored-resource-links '("fuzzy" "radio" "brain")
  "`org-link-types' which shouldn't be shown as resources in `org-brain-visualize'."
  :group 'org-brain
  :type '(repeat string))

(defcustom org-brain-data-file (expand-file-name ".org-brain-data.el" org-brain-path)
  "Where org-brain data is saved."
  :group 'org-brain
  :type '(directory))

(defcustom org-brain-visualize-default-choices 'all
  "Which entries to choose from when using `org-brain-visualize'.
If 'all, choose from all file and headline entries.
If 'files, only choose from file entries.
If 'root, only choose from file entries in `org-brain-path' (non-recursive)."
  :group 'org-brain
  :type '(choice
          (const :tag "All entries" all)
          (const :tag "Only file entries" files)
          (const :tag "Only root file entries" root)))

(defcustom org-brain-show-resources t
  "Should entry resources be shown in `org-brain-visualize'?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-show-text t
  "Should the entry text be shown in `org-brain-visualize'?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-brain-link-adds-child t
  "If brain: links should add the linked entry as a child.
Applicable for `org-insert-link' and `org-brain-insert-link'."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-after-visualize-hook nil
  "Hook run after `org-brain-visualize', but before `org-brain-text'.
Can be used to prettify the buffer output, e.g. `ascii-art-to-unicode'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-after-resource-button-functions nil
  "Hook run during `org-brain-insert-resource-button'.
Insert a bullet, then run hook functions, then insert the actual button.
Each function must take a single argument: the org link to the resource.
Can for instance be used in combination with `all-the-icons'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-exclude-text-tag "notext"
  "`org-mode' tag stopping `org-brain-visualize' from fetching entry text.
Only applies to headline entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-resouces-tag "resourceless"
  "`org-mode' tag stopping `org-brain-visualize' from fetching entry resources.
Only applies to headline entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-children-tag "childless"
  "`org-mode' tag which exclude the headline's children from org-brain's entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-show-children-tag "showchildren"
  "`org-mode' tag which get entire subtree from headline entry during `org-brain-text'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-tree-tag "nobrain"
  "`org-mode' tag which exclude the headline and its children from org-brain's entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-wander-interval 3
  "Seconds between randomized entries, when using `org-brain-visualize-wander'."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-title-max-length 0
  "If a title is longer than this, it'll be capped during `org-brain-visualize'.
If 0 or a negative value, the title won't be capped."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-entry-separator ";"
  "Can be used as a separator when adding children, parents, or friends.
Doing so allows for adding multiple entries at once."
  :group 'org-brain
  :type '(string))

;;;###autoload
(defun org-brain-update-id-locations ()
  "Scan `org-brain-files' using `org-id-update-id-locations'."
  (interactive)
  (org-id-update-id-locations (org-brain-files)))

;;* API

;; An entry is either a string or a list of three strings.
;; If a string, then the entry is a file.
;; If a list, then the entry is a headline:
;; ("file entry" "headline title" "ID")

(defvar org-brain--vis-entry nil
  "The last entry argument to `org-brain-visualize'.")

(defvar org-brain--vis-history nil
  "History previously visualized entries. Newest first.")

(defvar org-brain-resources-start-re "^[ \t]*:RESOURCES:[ \t]*$"
  "Regular expression matching the first line of a resources drawer.")

(defvar org-brain-pins nil "List of pinned org-brain entries.")

(defun org-brain-filep (entry)
  "Return t if the ENTRY is a (potential) brain file."
  (stringp entry))

(defun org-brain-id-exclude-taggedp (id)
  "Return t if ID is tagged as being excluded from org-brain."
  (org-with-point-at (org-id-find id t)
    (let ((tags (org-get-tags-at)))
      (or (member org-brain-exclude-tree-tag tags)
          (and (member org-brain-exclude-children-tag tags)
               (not (member org-brain-exclude-children-tag
                            (org-get-tags-at nil t))))))))

(defun org-brain-save-data ()
  "Save data to `org-brain-data-file'."
  ;; Code adapted from Magnar Sveen's multiple-cursors
  (with-temp-file org-brain-data-file
    (emacs-lisp-mode)
    (dolist (data '(org-brain-pins))
      (insert "(setq " (symbol-name data) "\n"
              "      '(")
      (newline-and-indent)
      (mapc #'(lambda (value)
                (insert (format "%S" value))
                (newline-and-indent))
            (symbol-value data))
      (insert "))")
      (newline))))

(defun org-brain-path-entry-name (path)
  "Get PATH as an org-brain entry name."
  (string-remove-suffix (concat "." org-brain-files-extension)
                        (file-relative-name (expand-file-name path)
                                            (expand-file-name org-brain-path))))

(defun org-brain-entry-path (entry)
  "Get path of org-brain ENTRY."
  (let ((name (if (org-brain-filep entry)
                  entry
                (car entry))))
    (expand-file-name (org-link-unescape (format "%s.%s" name org-brain-files-extension))
                      org-brain-path)))

(defun org-brain-files (&optional relative)
  "Get all org files (recursively) in `org-brain-path'.
If RELATIVE is t, then return relative paths and remove file extension.
Ignores \"dotfiles\"."
  (make-directory org-brain-path t)
  (if relative
      (mapcar #'org-brain-path-entry-name (org-brain-files))
    (directory-files-recursively
     org-brain-path (format "^[^.].*\\.%s$" org-brain-files-extension))))

(defun org-brain-headline-entries ()
  "Get all org-brain headline entries."
  (unless org-id-locations (org-id-locations-load))
  (let (ids)
    (maphash
     (lambda (id file)
       (when (and (string-prefix-p (expand-file-name org-brain-path)
                                   (expand-file-name file))
                  (not (org-brain-id-exclude-taggedp id)))
         (let* ((heading (org-id-find id t))
                (name (org-entry-get heading "ITEM")))
           (push (list (org-brain-path-entry-name file) name id)
                 ids))))
     org-id-locations)
    ids))

(defun org-brain-entry-from-id (id)
  "Get entry from ID."
  (unless org-id-locations (org-id-locations-load))
  (when-let ((path (gethash id org-id-locations)))
    (list
     (org-brain-path-entry-name path)
     (org-entry-get (org-id-find id t) "ITEM")
     id)))

(defun org-brain-entry-identifier (entry)
  "Get identifier of ENTRY.
The identifier is an id if ENTRY is a headline.
If ENTRY is file, then the identifier is the relative file name."
  (if (org-brain-filep entry)
      (org-entry-protect-space entry)
    (nth 2 entry)))

(defun org-brain-entry-at-pt ()
  "Get current org-brain entry.
In `org-mode' this is the current headline, or the file.
In `org-brain-visualize' just return `org-brain--vis-entry'."
  (cond ((eq major-mode 'org-mode)
         (unless (string-prefix-p (expand-file-name org-brain-path)
                                  (expand-file-name (buffer-file-name)))
           (error "Not in a brain file"))
         (if (ignore-errors (org-get-heading))
             (if-let ((id (org-entry-get nil "ID")))
                 (org-brain-entry-from-id id)
               (error "Current headline have no ID"))
           (org-brain-path-entry-name (buffer-file-name))))
        ((eq major-mode 'org-brain-visualize-mode)
         org-brain--vis-entry)
        (t
         (error "Not in org-mode or org-brain-visualize"))))

(defun org-brain-entry-name (entry)
  "Get name string of ENTRY."
  (if (org-brain-filep entry)
      entry
    (concat (car entry) "::" (cadr entry))))

(defun org-brain--choose-entry-helper (title targets)
  "Try to get TITLE as an entry from TARGETS.  Create it if it doesn't exist.
Meant as a helper function for `org-brain-choose-entry' and `org-brain-choose-entries'."
  (let ((id (cdr (assoc title targets))))
    (if id
        (org-brain-entry-from-id id)
      (setq title (split-string title "::" t))
      (let ((entry-path (org-brain-entry-path (car title))))
        (unless (file-exists-p entry-path)
          (write-region "" nil entry-path))
        (if (equal (length title) 2)
            (with-current-buffer (find-file-noselect entry-path)
              (goto-char (point-max))
              (insert (concat "\n* " (cadr title)))
              (list (car title) (cadr title) (org-id-get-create)))
          (car title))))))

(defun org-brain-choose-entry (prompt entries &optional predicate require-match initial-input)
  "PROMPT for an entry from ENTRIES and return it.
For PREDICATE, REQUIRE-MATCH and INITIAL-INPUT, see `completing-read'."
  (unless org-id-locations (org-id-locations-load))
  (let* ((targets (mapcar (lambda (x)
                            (if (org-brain-filep x)
                                (cons x nil)
                              (cons (org-brain-entry-name x)
                                    (nth 2 x))))
                          entries))
         (choice (completing-read prompt targets
                                  predicate require-match initial-input)))
    (org-brain--choose-entry-helper choice targets)))

(defun org-brain-choose-entries (prompt entries)
  "PROMPT for one or more ENTRIES, separated by `org-brain-entry-separator'.
Return the prompted entries in a list.
Very similar to `org-brain-choose-entry', but can return several entries."
  (unless org-id-locations (org-id-locations-load))
  (let* ((targets (mapcar (lambda (x)
                            (if (org-brain-filep x)
                                (cons x nil)
                              (cons (org-brain-entry-name x)
                                    (nth 2 x))))
                          entries))
         (choices (completing-read prompt targets)))
    (mapcar (lambda (title) (org-brain--choose-entry-helper title targets))
            (split-string choices org-brain-entry-separator))))

(defun org-brain-keywords (file)
  "Get alist of `org-mode' keywords and their values in FILE."
  (with-temp-buffer
    (ignore-errors (insert-file-contents file))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (kw)
        (cons (org-element-property :key kw)
              (org-element-property :value kw))))))

(defun org-brain-entry-marker (entry)
  "Get marker of headline ENTRY."
  (unless (org-brain-filep entry)
    (org-id-find (nth 2 entry) t)))

(defun org-brain-title (entry &optional capped)
  "Get title of ENTRY. If CAPPED is t, max length is `org-brain-title-max-length'."
  (let ((title
         (if (org-brain-filep entry)
             (or (cdr (assoc "TITLE" (org-brain-keywords (org-brain-entry-path entry))))
                 (car (last (split-string entry "/" t))))
           (nth 1 entry))))
    (if (and capped (> org-brain-title-max-length 0) (> (length title) org-brain-title-max-length))
        (concat (substring title 0 (1- org-brain-title-max-length)) "…")
      title)))

(defun org-brain-text (entry)
  "Get the text of ENTRY as string, skipping properties drawer."
  (when-let
      ((entry-text
        (if (org-brain-filep entry)
            ;; File entry
            (with-temp-buffer
              (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
              (goto-char (point-min))
              (or (outline-next-heading)
                  (goto-char (point-max)))
              (buffer-substring (or (save-excursion
                                      (when (re-search-backward "^[#:*]" nil t)
                                        (end-of-line)
                                        (point)))
                                    (point-min))
                                (point)))
          ;; Headline entry
          (org-with-point-at (org-brain-entry-marker entry)
            (let ((tags (org-get-tags-at nil t)))
              (unless (member org-brain-exclude-text-tag tags)
                (goto-char (cdr (org-get-property-block)))
                (end-of-line)
                (let (end)
                  (save-excursion
                    (or (and (not (member org-brain-exclude-children-tag tags))
                             (not (member org-brain-show-children-tag tags))
                             (org-goto-first-child))
                        (org-end-of-subtree t))
                    (setq end (point)))
                  (buffer-substring (point) end))))))))
    (remove-text-properties 0 (length entry-text)
                            '(line-prefix nil wrap-prefix nil)
                            entry-text)
    (org-remove-indentation entry-text)
    (with-temp-buffer
      (insert entry-text)
      (goto-char (point-min))
      (when (re-search-forward org-brain-resources-start-re nil t)
        (re-search-forward org-drawer-regexp nil t))
      (buffer-substring (point) (point-max)))))

(defun org-brain-parents (entry)
  "Get parents of ENTRY.
Often you want the siblings too, then use `org-brain-siblings' instead."
  (delete-dups
   (append (org-brain--linked-property-entries entry "BRAIN_PARENTS")
           (org-brain--local-parent entry))))

(defun org-brain-children (entry)
  "Get children of ENTRY."
  (delete-dups
   (append (org-brain--linked-property-entries entry "BRAIN_CHILDREN")
           (org-brain--local-children entry))))

(defun org-brain-siblings (entry)
  "Get siblings of ENTRY.
Return an alist where key = parent, value = siblings from that parent."
  (delete-dups
   (mapcar
    (lambda (parent)
      (cons parent (remove entry (org-brain-children parent))))
    (org-brain-parents entry))))

(defun org-brain-friends (entry)
  "Get friends of ENTRY."
  (delete-dups (org-brain--linked-property-entries entry "BRAIN_FRIENDS")))

(defun org-brain-resources (entry)
  "Get alist of links in ENTRY, excluding `org-brain-ignored-resource-links'.
A link can be either an org link or an org attachment.
The car is the raw-link and the cdr is the description."
  (if (org-brain-filep entry)
      ;; File entry
      (with-temp-buffer
        (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
        (goto-char (point-min))
        (or (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
            (goto-char (point-max)))
        (org-element-map (org-element-parse-secondary-string
                          (buffer-substring-no-properties (point-min) (point)) '(link))
            'link
          (lambda (link)
            (unless (member (org-element-property :type link) org-brain-ignored-resource-links)
              (cons (org-element-property :raw-link link)
                    (car (org-element-contents link)))))))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (unless (member org-brain-exclude-resouces-tag (org-get-tags-at nil t))
        (append
         ;; Links
         (let ((data (let (end)
                       (save-excursion
                         (or (org-goto-first-child)
                             (org-end-of-subtree t))
                         (setq end (point)))
                       (org-element-parse-secondary-string
                        (buffer-substring-no-properties (point) end)
                        '(link)))))
           (delete-dups
            (org-element-map data 'link
              (lambda (link)
                (unless (member (org-element-property :type link)
                                org-brain-ignored-resource-links)
                  (cons (org-element-property :raw-link link)
                        (when-let ((desc (car (org-element-contents link))))
                          (replace-regexp-in-string "[ \t\n\r]+" " " desc)))))
              nil nil t)))
         ;; Attachments
         (when-let ((attach-dir (org-attach-dir)))
           (mapcar (lambda (attachment)
                     (cons (format "file:%s"
                                   (org-link-escape
                                    (expand-file-name attachment attach-dir)))
                           attachment))
                   (org-attach-file-list attach-dir))))))))

(defun org-brain--local-parent (entry)
  "Get file local parent of ENTRY, as a list."
  (if-let ((parent
            (unless (org-brain-filep entry)
              (org-with-point-at (org-brain-entry-marker entry)
                (if (and (org-up-heading-safe)
                         (org-entry-get nil "ID"))
                    (org-brain-entry-from-id (org-entry-get nil "ID"))
                  (car entry))))))
      (list parent)))

(defun org-brain--local-children (entry)
  "Get file local children of ENTRY."
  (remove
   entry
   (if (org-brain-filep entry)
       ;; File entry
       (with-temp-buffer
         (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
         (org-element-map (org-element-parse-buffer 'headline) 'headline
           (lambda (headline)
             (when-let ((id (org-element-property :ID headline)))
               (unless (org-brain-id-exclude-taggedp id)
                 (org-brain-entry-from-id id))))
           nil nil 'headline))
     ;; Headline entry
     (org-with-point-at (org-brain-entry-marker entry)
       (let (children)
         (deactivate-mark)
         (org-mark-subtree)
         (org-goto-first-child)
         (setq children
               (org-map-entries
                (lambda () (org-brain-entry-from-id (org-entry-get nil "ID")))
                t 'region-start-level
                (lambda ()
                  (let ((id (org-entry-get nil "ID")))
                    (when (or (not id)
                              (org-brain-id-exclude-taggedp id))
                      (save-excursion
                        (outline-next-heading)
                        (point)))))))
         (deactivate-mark)
         children)))))

(defun org-brain--linked-property-entries (entry property)
  "Get list of entries linked to in ENTRY by PROPERTY.
PROPERTY could for instance be BRAIN_CHILDREN."
  (let ((propertylist
         (if (org-brain-filep entry)
             ;; File entry
             (mapcar
              (lambda (x) (or (org-brain-entry-from-id x) x))
              (mapcar #'org-entry-restore-space
                      (when-let ((kw-values (cdr (assoc property
                                                        (org-brain-keywords
                                                         (org-brain-entry-path entry))))))
                        (org-split-string kw-values "[ \t]"))))
           ;; Headline entry
           (mapcar
            (lambda (x) (or (org-brain-entry-from-id x) x))
            (org-entry-get-multivalued-property (org-brain-entry-marker entry) property)))))
    (if (equal propertylist '("")) nil propertylist)))

(defun org-brain-add-relationship (parent child)
  "Add external relationship between PARENT and CHILD."
  (unless (member child (org-brain-children parent))

    (org-save-all-org-buffers)
    (if (org-brain-filep parent)
        ;; Parent = File
        (with-current-buffer (find-file-noselect (org-brain-entry-path parent))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+BRAIN_CHILDREN:.*$" nil t)
              (insert (concat " " (org-brain-entry-identifier child)))
            (insert (concat "#+BRAIN_CHILDREN: "
                            (org-brain-entry-identifier child)
                            "\n\n")))
          (save-buffer))
      ;; Parent = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker parent)
                                             "BRAIN_CHILDREN"
                                             (org-brain-entry-identifier child)))
    (if (org-brain-filep child)
        ;; Child = File
        (with-current-buffer (find-file-noselect (org-brain-entry-path child))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+BRAIN_PARENTS:.*$" nil t)
              (insert (concat " " (org-brain-entry-identifier parent)))
            (insert (concat "#+BRAIN_PARENTS: "
                            (org-brain-entry-identifier parent)
                            "\n\n")))
          (save-buffer))
      ;; Child = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker child)
                                             "BRAIN_PARENTS"
                                             (org-brain-entry-identifier parent)))
    (org-save-all-org-buffers)))

(defun org-brain-remove-relationship (parent child)
  "Remove external relationship between PARENT and CHILD."
  (unless (member child (org-brain-children parent))
    (error "Relationship doesn't exist"))
  (org-save-all-org-buffers)
  (if (org-brain-filep parent)
      ;; Parent = File
      (with-current-buffer (find-file-noselect (org-brain-entry-path parent))
        (goto-char (point-min))
        (re-search-forward "^#\\+BRAIN_CHILDREN:.*$")
        (beginning-of-line)
        (re-search-forward (concat " " (org-brain-entry-identifier child)))
        (replace-match "")
        (save-buffer))
    ;; Parent = Headline
    (org-entry-remove-from-multivalued-property (org-brain-entry-marker parent)
                                                "BRAIN_CHILDREN"
                                                (org-brain-entry-identifier child)))
  (if (org-brain-filep child)
      ;; Child = File
      (with-current-buffer (find-file-noselect (org-brain-entry-path child))
        (goto-char (point-min))
        (re-search-forward "^#\\+BRAIN_PARENTS:.*$")
        (beginning-of-line)
        (re-search-forward (concat " " (org-brain-entry-identifier parent)))
        (replace-match "")
        (save-buffer))
    ;; Child = Headline
    (org-entry-remove-from-multivalued-property (org-brain-entry-marker child)
                                                "BRAIN_PARENTS"
                                                (org-brain-entry-identifier parent)))
  (org-save-all-org-buffers))

;;* Buffer commands

;;;###autoload
(defun org-brain-add-child ()
  "Add external child to entry at point.
If chosen child entry doesn't exist, create it as a new file.
Several children can be added, by using `org-brain-entry-separator'."
  (interactive)
  (dolist (child-entry (org-brain-choose-entries
                        "Child: " (append (org-brain-files t)
                                          (org-brain-headline-entries))))
    (org-brain-add-relationship (org-brain-entry-at-pt) child-entry))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-new-child ()
  "Create a new internal child headline to entry at point.
Several children can be created, by using `org-brain-entry-separator'."
  (interactive)
  (let ((entry (org-brain-entry-at-pt))
        (child-name-string (read-string "Child name: ")))
    (dolist (child-name (split-string child-name-string org-brain-entry-separator))
      (when (equal (length child-name) 0)
        (error "Child name must be at least 1 character"))
      (if (org-brain-filep entry)
          ;; File entry
          (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
            (goto-char (point-min))
            (if (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
                (progn
                  (beginning-of-line)
                  (open-line 1))
              (goto-char (point-max)))
            (insert (concat "* " child-name))
            (org-id-get-create)
            (save-buffer))
        ;; Headline entry
        (org-with-point-at (org-brain-entry-marker entry)
          (if (org-goto-first-child)
              (open-line 1)
            (org-end-of-subtree t))
          (org-insert-heading)
          (org-do-demote)
          (insert child-name)
          (org-id-get-create)
          (save-buffer)))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-child ()
  "Remove child from entry at point."
  (interactive)
  (let* ((entry (org-brain-entry-at-pt))
         (child (org-brain-choose-entry "Child: "
                                        (org-brain-children entry)
                                        nil t)))
    (if (member child (org-brain--local-children entry))
        (org-brain-delete-entry child)
      (org-brain-remove-relationship entry child)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-parent ()
  "Add external parent to entry at point.
If chosen parent entry doesn't exist, create it as a new file.
Several parents can be added, by using `org-brain-entry-separator'."
  (interactive)
  (dolist (parent-entry (org-brain-choose-entries
                         "Parent: " (append (org-brain-files t)
                                            (org-brain-headline-entries))))
    (org-brain-add-relationship parent-entry (org-brain-entry-at-pt)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-parent ()
  "Remove external parent from entry at point."
  (interactive)
  (let ((entry (org-brain-entry-at-pt)))
    (org-brain-remove-relationship
     (org-brain-choose-entry "Parent: "
                             (org-brain--linked-property-entries
                              entry "BRAIN_PARENTS")
                             nil t)
     entry))
  (org-brain--revert-if-visualizing))

(defun org-brain--internal-add-friendship (entry1 entry2 &optional oneway)
  "Add friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, add ENTRY2 as friend of ENTRY1, but not the other way around."
  (unless (member entry2 (org-brain-friends entry1))
    (if (org-brain-filep entry1)
        ;; Entry1 = File
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry1))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+BRAIN_FRIENDS:.*$" nil t)
              (insert (concat " " (org-brain-entry-identifier entry2)))
            (insert (concat "#+BRAIN_FRIENDS: "
                            (org-brain-entry-identifier entry2)
                            "\n\n")))
          (save-buffer))
      ;; Entry1 = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker entry1)
                                             "BRAIN_FRIENDS"
                                             (org-brain-entry-identifier entry2))))
  (unless oneway (org-brain--internal-add-friendship entry2 entry1 t))
  (org-save-all-org-buffers))

;;;###autoload
(defun org-brain-add-friendship ()
  "Add a new friend to entry at point.
If chosen friend entry doesn't exist, create it as a new file.
Several friends can be added, by using `org-brain-entry-separator'."
  (interactive)
  (dolist (friend-entry (org-brain-choose-entries
                         "Friend: " (append (org-brain-files t)
                                            (org-brain-headline-entries))))
    (org-brain--internal-add-friendship (org-brain-entry-at-pt) friend-entry))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-friendship (entry1 entry2 &optional oneway)
  "Remove friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, then remove ENTRY2 as a friend of ENTRY1, but not vice versa.

If run interactively, use `org-brain-entry-at-pt' as ENTRY1 and prompt for ENTRY2."
  (interactive
   (let ((entry-at-pt (org-brain-entry-at-pt)))
     (list entry-at-pt
           (org-brain-choose-entry "Remove: " (org-brain-friends entry-at-pt) nil t))))
  (when (member entry2 (org-brain-friends entry1))
    (if (org-brain-filep entry1)
        ;; Entry1 = File
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry1))
          (goto-char (point-min))
          (re-search-forward "^#\\+BRAIN_FRIENDS:.*$")
          (beginning-of-line)
          (re-search-forward (concat " " (org-brain-entry-identifier entry2)))
          (replace-match "")
          (save-buffer))
      ;; Entry2 = Headline
      (org-entry-remove-from-multivalued-property (org-brain-entry-marker entry1)
                                                  "BRAIN_FRIENDS"
                                                  (org-brain-entry-identifier entry2))))
  (if oneway
      (org-brain--revert-if-visualizing)
    (org-brain-remove-friendship entry2 entry1 t))
  (org-save-all-org-buffers))

;;;###autoload
(defun org-brain-goto (&optional entry)
  "Goto buffer and position of org-brain ENTRY.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (org-brain-stop-wandering)
  (unless entry (setq entry (org-brain-choose-entry
                             "Entry: "
                             (append (org-brain-files t)
                                     (org-brain-headline-entries))
                             nil t)))
  (if (org-brain-filep entry)
      (let ((path (org-brain-entry-path entry)))
        (if (file-exists-p path)
            (find-file path)
          ;; If file doesn't exists, it is probably an id
          (org-id-goto entry)
          (org-show-entry)))
    (org-id-goto (nth 2 entry))
    (org-show-entry)))

;;;###autoload
(defun org-brain-goto-end (&optional entry)
  "Like `org-brain-goto', but visits the end of ENTRY.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (org-brain-stop-wandering)
  (unless entry (setq entry (org-brain-choose-entry
                             "Entry: "
                             (append (org-brain-files t)
                                     (org-brain-headline-entries))
                             nil t)))
  (cl-flet ((headline-end
             ()
             (let ((tags (org-get-tags-at nil t)))
               (or (and (not (member org-brain-exclude-children-tag tags))
                        (not (member org-brain-show-children-tag tags))
                        (org-goto-first-child))
                   (org-end-of-subtree t)))))
    (if (org-brain-filep entry)
        (let ((path (org-brain-entry-path entry)))
          (if (file-exists-p path)
              (progn
                (find-file path)
                (goto-char (point-min))
                (or (outline-next-heading)
                    (goto-char (point-max))))
            ;; If file doesn't exists, it is probably an id
            (org-id-goto entry)
            (headline-end)))
      (org-id-goto (nth 2 entry))
      (headline-end))))

;;;###autoload
(defun org-brain-goto-current ()
  "Use `org-brain-goto' on `org-brain-entry-at-pt'."
  (interactive)
  (org-brain-goto (org-brain-entry-at-pt)))

;;;###autoload
(defun org-brain-goto-child (entry &optional all)
  "Goto a child of ENTRY.
If run interactively, get ENTRY from context.
If ALL is nil, choose only between externally linked children."
  (interactive (list (org-brain-entry-at-pt)))
  (org-brain-goto (org-brain-choose-entry
                   "Child: "
                   (if all
                       (org-brain-children entry)
                     (org-brain--linked-property-entries
                      entry "BRAIN_CHILDREN"))
                   nil t)))

;;;###autoload
(defun org-brain-goto-parent (entry &optional all)
  "Goto a parent of ENTRY.
If run interactively, get ENTRY from context.
If ALL is nil, choose only between externally linked parents."
  (interactive (list (org-brain-entry-at-pt)))
  (org-brain-goto (org-brain-choose-entry
                   "Parent: "
                   (if all
                       (org-brain-parents entry)
                     (org-brain--linked-property-entries
                      entry "BRAIN_PARENTS"))
                   nil t)))

;;;###autoload
(defun org-brain-goto-friend (entry)
  "Goto a friend of ENTRY.
If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (org-brain-goto (org-brain-choose-entry
                   "Friend: "
                   (org-brain--linked-property-entries
                    entry "BRAIN_FRIENDS")
                   nil t)))

;;;###autoload
(defun org-brain-delete-entry (entry &optional noconfirm)
  "Delete ENTRY and all of its local children.
If run interactively, ask for the ENTRY.
If NOCONFIRM is nil, ask if we really want to delete."
  (interactive
   (list (org-brain-choose-entry
          "Entry: " (append (org-brain-files t)
                            (org-brain-headline-entries))
          nil t)
         nil))
  (let ((local-children (org-brain--local-children entry)))
    (when (or noconfirm
              (yes-or-no-p
               (format "%s have %d local children. Delete them all?"
                       (org-brain-entry-name entry)
                       (length local-children))))
      (dolist (child local-children)
        (org-brain-delete-entry child t))
      (dolist (child (org-brain--linked-property-entries
                      entry "BRAIN_CHILDREN"))
        (org-brain-remove-relationship entry child))
      (dolist (parent (org-brain--linked-property-entries
                       entry "BRAIN_PARENTS"))
        (org-brain-remove-relationship parent entry))
      (dolist (friend (org-brain-friends entry))
        (org-brain-remove-friendship entry friend))
      (if (org-brain-filep entry)
          (let ((filename (org-brain-entry-path entry)))
            (if (vc-backend filename)
                (vc-delete-file filename)
              (delete-file filename delete-by-moving-to-trash)
              (kill-buffer (get-file-buffer filename))))
        (org-with-point-at (org-brain-entry-marker entry)
          (org-mark-subtree)
          (delete-region (region-beginning) (region-end))))))
  (delete entry org-brain--vis-history)
  (org-save-all-org-buffers)
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-pin (entry &optional status)
  "Change if ENTRY is pinned or not.
If run interactively, get ENTRY from context.

If STATUS is positive, pin the entry. If negative, remove the pin.
If STATUS is omitted, toggle between pinned / not pinned."
  (interactive (list (org-brain-entry-at-pt)))
  (cond ((eq status nil)
         (if (member entry org-brain-pins)
             (org-brain-pin entry -1)
           (org-brain-pin entry 1)))
        ((>= status 1)
         (if (member entry org-brain-pins)
             (error "Entry is already pinned")
           (push entry org-brain-pins)
           (org-brain-save-data)
           (message "Pin added.")))
        ((< status 1)
         (if (member entry org-brain-pins)
             (progn
               (setq org-brain-pins (delete entry org-brain-pins))
               (org-brain-save-data)
               (message "Pin removed."))
           (error "Entry isn't pinned"))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-set-title (entry title)
  "Set the name of ENTRY to TITLE.
If run interactively, get ENTRY from context and prompt for TITLE."
  (interactive
   (let* ((entry-at-pt (org-brain-entry-at-pt))
          (new-title (org-brain-title entry-at-pt)))
     (when (equal (length new-title) 0)
       (error "Title must be at least 1 character"))
     (list entry-at-pt (read-string "Title: " new-title))))
  (if (org-brain-filep entry)
      ;; File entry
      (let ((entry-path (org-brain-entry-path entry)))
        (with-current-buffer (find-file-noselect entry-path)
          (goto-char (point-min))
          (when (assoc "TITLE" (org-brain-keywords entry-path))
            (re-search-forward "^#\\+TITLE:")
            (kill-whole-line))
          (insert (format "#+TITLE: %s\n" title))
          (save-buffer)))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (org-edit-headline title)
      (save-buffer)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-set-tags (entry)
  "Use `org-set-tags' on headline ENTRY.
If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (when (org-brain-filep entry)
    (error "Can only set tags on headline entries"))
  (org-with-point-at (org-brain-entry-marker entry)
    (org-set-tags)
    (save-buffer))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-headline-to-file (entry)
  "Convert headline ENTRY to a file entry.
Prompt for name of the new file.
If interactive, also prompt for ENTRY."
  (interactive (list (org-brain-choose-entry "Entry: "
                                             (org-brain-headline-entries)
                                             nil t)))
  (let* (level
         (title (org-brain-title entry))
         (new-entry (read-string "New file entry: " title))
         (path (org-brain-entry-path new-entry)))
    (when (file-exists-p path)
      (error "That file already exists"))
    (let ((parents (org-brain-parents entry))
          (external-parents (org-brain--linked-property-entries entry "BRAIN_PARENTS"))
          (children (org-brain--linked-property-entries entry "BRAIN_CHILDREN"))
          (friends (org-brain-friends entry))
          (hl-text (org-with-point-at (org-brain-entry-marker entry)
                     (setq level (org-outline-level))
                     (org-get-entry))))
      (dolist (parent external-parents)
        (org-brain-remove-relationship parent entry))
      (dolist (child children)
        (org-brain-remove-relationship entry child))
      (dolist (friend friends)
        (org-brain-remove-friendship entry friend))
      (org-with-point-at (org-brain-entry-marker entry)
        (org-cut-subtree)
        (pop kill-ring)
        (save-buffer))
      (with-temp-file path
        (insert (format "#+TITLE:%s\n\n%s" title hl-text))
        (delay-mode-hooks
          (org-mode)
          (goto-char (point-min))
          (re-search-forward org-property-drawer-re)
          (replace-match "")
          (goto-char (point-max))
          (let ((level-regex "^"))
            (dotimes (_i (1+ level))
              (setq level-regex (concat level-regex "\\*")))
            (setq level-regex (concat level-regex " "))
            (while (re-search-backward level-regex nil t)
              (dotimes (_i level) (org-promote-subtree))))))
      (dolist (parent parents)
        (org-brain-add-relationship parent new-entry))
      (dolist (child children)
        (org-brain-add-relationship new-entry child))
      (dolist (friend friends)
        (org-brain--internal-add-friendship new-entry friend)))))

;;;###autoload
(defun org-brain-insert-link ()
  "Insert a link to an org-brain entry and suggest a description.
Adds the linked entry as a child if `org-brain-brain-link-adds-child' is t."
  (interactive)
  (let* ((entry-at-pt (ignore-errors (org-brain-entry-at-pt)))
         (entry (org-brain-choose-entry "Entry: " (append (org-brain-files t)
                                                          (org-brain-headline-entries))))
         (title (org-brain-title entry))
         (desc (read-string "Description: " title)))
    (when (and entry-at-pt org-brain-brain-link-adds-child)
      (org-brain-add-relationship entry-at-pt entry))
    (insert (org-make-link-string (concat "brain:" (if (org-brain-filep entry)
                                                       entry
                                                     (nth 2 entry)))
                                  desc))))

;;;###autoload
(defun org-brain-agenda ()
  "Like `org-agenda', but only for `org-brain-files'."
  (interactive)
  (let ((org-agenda-files (org-brain-files)))
    (org-agenda)))

;;;###autoload
(defun org-brain-create-relationships-from-links ()
  "Add relationships for brain: links in `org-brain-path'.
Only create relationships to other files, not to headline entries.

This function is meant to be used in order to convert old
org-brain setups to the system introduced in version 0.4. Please
make a backup of your `org-brain-path' before running this
function."
  (interactive)
  (when (y-or-n-p "This function is meant for old configurations. Are uou sure you want to scan for links? ")
    (dolist (file (org-brain-files))
      (with-temp-buffer
        (insert-file-contents file)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (string-equal (org-element-property :type link) "brain")
              (org-brain-add-relationship
               (org-brain-path-entry-name file)
               (car (split-string (org-element-property :path link) "::"))))))))))

;;* Visualize
;;;###autoload
(defun org-brain-visualize (entry &optional nofocus nohistory wander)
  "View a concept map with ENTRY at the center.

When run interactively, prompt for ENTRY and suggest
`org-brain-entry-at-pt'. By default, the choices presented is
determined by `org-brain-visualize-default-choices': 'all will
show all entries, 'files will only show file entries and 'root
will only show files in the root of `org-brain-path'.

You can override `org-brain-visualize-default-choices':
  `\\[universal-argument]' will use 'all.
  `\\[universal-argument] \\[universal-argument]' will use 'files.
  `\\[universal-argument] \\[universal-argument] \\[universal-argument]' will use 'root.

Unless NOFOCUS is non-nil, the `org-brain-visualize' buffer will gain focus.
Unless NOHISTORY is non-nil, add the entry to `org-brain--vis-history'.
Setting NOFOCUS to t implies also having NOHISTORY as t.
Unless WANDER is t, `org-brain-stop-wandering' will be run."
  (interactive
   (let ((choices (cond ((equal current-prefix-arg '(4)) 'all)
                        ((equal current-prefix-arg '(16)) 'files)
                        ((equal current-prefix-arg '(64)) 'root)
                        (t org-brain-visualize-default-choices)))
         (def-choice (unless (eq major-mode 'org-brain-visualize-mode)
                       (ignore-errors (org-brain-entry-name (org-brain-entry-at-pt))))))
     (org-brain-stop-wandering)
     (list
      (org-brain-choose-entry
       "Entry: "
       (cond ((equal choices 'all)
              (append (org-brain-files t) (org-brain-headline-entries)))
             ((equal choices 'files)
              (org-brain-files t))
             ((equal choices 'root)
              (make-directory org-brain-path t)
              (mapcar #'org-brain-path-entry-name
                      (directory-files org-brain-path t (format "\\.%s$" org-brain-files-extension)))))
       nil nil def-choice))))
  (unless wander (org-brain-stop-wandering))
  (setq org-brain--vis-entry entry)
  (with-current-buffer (get-buffer-create "*org-brain*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (org-brain--vis-pinned)
    (org-brain--vis-parents-siblings entry)
    ;; Insert entry title
    (let ((title (org-brain-title entry)))
      (let ((half-title-length (/ (length title) 2)))
        (if (>= half-title-length (current-column))
            (delete-char (- (current-column)))
          (ignore-errors (delete-char (- half-title-length)))))
      (let ((entry-pos (point)))
        (insert title)
        (org-brain--vis-friends entry)
        (org-brain--vis-children entry)
        (when (and org-brain-show-resources)
          (org-brain--vis-resources (org-brain-resources entry)))
        (if org-brain-show-text
            (org-brain--vis-text entry)
          (run-hooks 'org-brain-after-visualize-hook))
        (org-brain-visualize-mode)
        (goto-char entry-pos)
        (unless nofocus
          (pop-to-buffer "*org-brain*")
          (when (and (not nohistory)
                     (not (equal entry (car org-brain--vis-history)))
                     (< (length org-brain--vis-history) 15))
            (push entry org-brain--vis-history)))))))

;;;###autoload
(defun org-brain-visualize-random ()
  "Run `org-brain-visualize' on a random org-brain entry."
  (interactive)
  (let ((entries (append (org-brain-files t)
                         (org-brain-headline-entries))))
    (org-brain-visualize (nth (random (length entries)) entries) nil nil t)))

(defvar org-brain-wander-timer nil
  "A timer running `org-brain-visualize-random' at a set interval.

Can be (de)activated by `org-brain-visualize-wander'.")

(defun org-brain-visualize-wander ()
  (interactive)
  (if (member org-brain-wander-timer timer-list)
      (progn
        (cancel-timer org-brain-wander-timer)
        (message "Wandering stopped."))
    (setq org-brain-wander-timer (run-at-time nil org-brain-wander-interval #'org-brain-visualize-random))
    (message "Wandering started.")))

(defun org-brain-stop-wandering ()
  "Cancels `org-brain-wander-timer', if it is active."
  (when (member org-brain-wander-timer timer-list)
    (cancel-timer org-brain-wander-timer)))

(defun org-brain-visualize-quit ()
  (interactive)
  "Like `quit-window', but also stops `org-brain-visualize-wander'."
  (org-brain-stop-wandering)
  (quit-window))

(defun org-brain-insert-visualize-button (entry)
  "Insert a button, running `org-brain-visualize' on ENTRY when clicked."
  (insert-text-button
   (org-brain-title entry t)
   'action (lambda (_x) (org-brain-visualize entry))
   'follow-link t))

(defun org-brain-insert-resource-button (resource &optional indent)
  "Insert a new line with a RESOURCE button, indented by INDENT spaces."
  (insert (make-string (or indent 0) ?\ ) "\n• ")
  (run-hook-with-args 'org-brain-after-resource-button-functions (car resource))
  (insert-text-button
   (or (cdr resource) (car resource))
   'action (lambda (_x)
             (org-open-link-from-string (car resource)))
   'follow-link t))

(defun org-brain-add-resource (link &optional description prompt entry)
  "Insert LINK with DESCRIPTION in an entry.
If PROMPT is non nil, use `org-insert-link' even if not being run interactively.
If ENTRY is omitted, try to get it from context or prompt for it."
  (interactive "i")
  (unless entry
    (setq entry (or (ignore-errors (org-brain-entry-at-pt))
                    (org-brain-choose-entry "Entry: "
                                            (append (org-brain-files t)
                                                    (org-brain-headline-entries))))))
  (cl-flet ((insert-resource-link
             ()
             (unless (and link (not prompt))
               (setq link (read-string "Link: " link))
               (when (string-match org-bracket-link-regexp link)
                 (let ((linkdesc (match-string 3 link)))
                   (when (and (not description) linkdesc)
                     (setq description linkdesc))
                   (setq link (match-string 1 link))))
               (setq description (read-string "Description: " description)))
             (newline-and-indent)
             (insert (format "- %s" (org-make-link-string link description)))
             (save-buffer)))
    (if (org-brain-filep entry)
        ;; File entry
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
          (goto-char (point-min))
          (or (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
              (goto-char (point-max)))
          (if (re-search-backward org-brain-resources-start-re nil t)
              (end-of-line)
            (goto-char (point-min))
            (insert ":RESOURCES:\n:END:\n")
            (re-search-backward org-brain-resources-start-re nil t)
            (end-of-line))
          (insert-resource-link))
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (goto-char (cdr (org-get-property-block)))
        (forward-line 1)
        (if (looking-at org-brain-resources-start-re)
            (end-of-line)
          (open-line 1)
          (indent-for-tab-command)
          (insert ":RESOURCES:\n")
          (indent-for-tab-command)
          (insert ":END:")
          (re-search-backward org-brain-resources-start-re nil t)
          (end-of-line))
        (insert-resource-link))))
  (org-brain--revert-if-visualizing))

(defalias 'org-brain-visualize-add-resource #'org-brain-add-resource)

(defun org-brain-visualize-attach ()
  "Use `org-attach' on `org-brain--vis-entry'."
  (interactive)
  (unless (eq major-mode 'org-brain-visualize-mode)
    (error "Not in org-brain-visualize-mode"))
  (when (org-brain-filep org-brain--vis-entry)
    (error "Can only attach to headline entries"))
  (let* ((entry-path (org-brain-entry-path org-brain--vis-entry))
         (existing-buffer (find-buffer-visiting entry-path)))
    (with-current-buffer (find-file entry-path)
      (goto-char (cdr (org-id-find (nth 2 org-brain--vis-entry))))
      (call-interactively #'org-attach)
      (save-buffer)
      (if existing-buffer
          (switch-to-buffer "*org-brain*")
        (kill-this-buffer))
      (revert-buffer))))

(defun org-brain-paste-resource ()
  "Add `current-kill' as a resource link.
See `org-brain-add-resource'."
  (interactive)
  (org-brain-add-resource (current-kill 0) nil t))

(defalias 'org-brain-visualize-paste-resource #'org-brain-paste-resource)

(defun org-brain-visualize-back ()
  "Go back to the previously visualized entry."
  (interactive)
  (pop org-brain--vis-history)
  (if org-brain--vis-history
      (org-brain-visualize (car org-brain--vis-history) nil t)
    (error "No further history")))

(defun org-brain-visualize-revert (_ignore-auto _noconfirm)
  "Revert function for `org-brain-visualize-mode'."
  (org-brain-visualize org-brain--vis-entry t))

(defun org-brain--revert-if-visualizing ()
  "Revert buffer if in `org-brain-visualize-mode'."
  (when (eq major-mode 'org-brain-visualize-mode)
    (org-brain-stop-wandering)
    (revert-buffer)))

(define-derived-mode org-brain-visualize-mode
  special-mode  "Org-brain Visualize"
  "Major mode for `org-brain-visualize'.
\\{org-brain-visualize-mode-map}"
  (setq revert-buffer-function #'org-brain-visualize-revert))

;;** Keybindings

(define-key org-brain-visualize-mode-map "p" 'org-brain-add-parent)
(define-key org-brain-visualize-mode-map "P" 'org-brain-remove-parent)
(define-key org-brain-visualize-mode-map "c" 'org-brain-add-child)
(define-key org-brain-visualize-mode-map "C" 'org-brain-remove-child)
(define-key org-brain-visualize-mode-map "*" 'org-brain-new-child)
(define-key org-brain-visualize-mode-map "h" 'org-brain-new-child)
(define-key org-brain-visualize-mode-map "n" 'org-brain-pin)
(define-key org-brain-visualize-mode-map "t" 'org-brain-set-title)
(define-key org-brain-visualize-mode-map "j" 'forward-button)
(define-key org-brain-visualize-mode-map "k" 'backward-button)
(define-key org-brain-visualize-mode-map [?\t] 'forward-button)
(define-key org-brain-visualize-mode-map [backtab] 'backward-button)
(define-key org-brain-visualize-mode-map "o" 'org-brain-goto-current)
(define-key org-brain-visualize-mode-map "O" 'org-brain-goto)
(define-key org-brain-visualize-mode-map "v" 'org-brain-visualize)
(define-key org-brain-visualize-mode-map "f" 'org-brain-add-friendship)
(define-key org-brain-visualize-mode-map "F" 'org-brain-remove-friendship)
(define-key org-brain-visualize-mode-map "d" 'org-brain-delete-entry)
(define-key org-brain-visualize-mode-map "l" 'org-brain-add-resource)
(define-key org-brain-visualize-mode-map "a" 'org-brain-visualize-attach)
(define-key org-brain-visualize-mode-map "b" 'org-brain-visualize-back)
(define-key org-brain-visualize-mode-map "\C-y" 'org-brain-visualize-paste-resource)
(define-key org-brain-visualize-mode-map "T" 'org-brain-set-tags)
(define-key org-brain-visualize-mode-map "q" 'org-brain-visualize-quit)
(define-key org-brain-visualize-mode-map "r" 'org-brain-visualize-random)
(define-key org-brain-visualize-mode-map "R" 'org-brain-visualize-wander)

;;** Drawing helpers

(defun org-brain--vis-pinned ()
  "Insert pinned entries.
Helper function for `org-brain-visualize'."
  (insert "PINNED:")
  (unless org-brain-pins
    (load org-brain-data-file t))
  (dolist (pin org-brain-pins)
    (insert "  ")
    (org-brain-insert-visualize-button pin))
  (insert "\n\n\n"))

(defun org-brain--vis-parents-siblings (entry)
  "Insert parents and siblings of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((siblings (org-brain-siblings entry)))
    (let ((parent-positions nil)
          (max-width 0))
      (dolist (parent siblings)
        (let ((children-links (cdr parent))
              (col-start (+ 3 max-width))
              (parent-title (org-brain-title (car parent))))
          (goto-line 4)
          (mapc
           (lambda (child)
             (picture-forward-column col-start)
             (insert (make-string (1+ (length parent-title)) ?\ ) "+-")
             (org-brain-insert-visualize-button child)
             (setq max-width (max max-width (current-column)))
             (newline (forward-line 1)))
           children-links)
          (goto-line 4)
          (forward-line (1- (length children-links)))
          (picture-forward-column col-start)
          (push (cons (picture-current-line)
                      (+ (current-column) (/ (length parent-title) 2)))
                parent-positions)
          (org-brain-insert-visualize-button (car parent))
          (setq max-width (max max-width (current-column)))
          (when children-links
            (insert "-")
            (delete-char (+ 1 (length parent-title))))))
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
          (dolist (pos parent-positions)
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
            (insert "+"))
          ;; Line to main entry
          (move-to-column (/ (+ (cdar (last parent-positions))
                                (cdar parent-positions))
                             2))
          (delete-char 1)
          (when (> (length parent-positions) 1)
            (insert "+")
            (backward-char 1)
            (picture-move-down 1)
            (insert "|")
            (picture-move-down 1))
          (insert "▽"))))
    (picture-move-down 1)))

(defun org-brain--vis-children (entry)
  "Insert children of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((children (org-brain-children entry)))
    (insert "\n\n")
    (dolist (child children)
      (let ((child-title (org-brain-title child)))
        (when (> (+ (current-column) (length child-title))
                 fill-column)
          (insert "\n"))
        (org-brain-insert-visualize-button child)
        (insert "  ")))))

(defun org-brain--vis-friends (entry)
  "Insert friends of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((friends (org-brain-friends entry)))
    (insert " ←→ ")
    (dolist (friend friends)
      (let ((column (current-column)))
        (org-brain-insert-visualize-button friend)
        (picture-move-down 1)
        (move-to-column column t)))
    (kill-whole-line)
    (backward-char 1)))

(defun org-brain--vis-resources (resources)
  "Insert links to RESOURCES.
Helper function for `org-brain-visualize'."
  (when resources
    (insert "\n\n--- Resources ---------------------------------\n")
    (mapc #'org-brain-insert-resource-button resources)))

(defun org-brain--vis-text (entry)
  "Insert text of ENTRY.
Helper function for `org-brain-visualize'."
  (if-let ((text (org-brain-text entry)))
      (progn
        (setq text (string-trim text))
        (if (> (length text) 0)
            (progn
              (insert "\n\n--- Entry -------------------------------------\n\n")
              (run-hooks 'org-brain-after-visualize-hook)
              (insert text))
          (run-hooks 'org-brain-after-visualize-hook)))
    (run-hooks 'org-brain-after-visualize-hook)))

;;* Brain link
(defun org-brain-link-complete ()
  "Create an org-link target string to a file in `org-brain-path'."
  (let ((entry (ignore-errors (org-brain-entry-at-pt)))
        (choice (org-brain-choose-entry "Entry: " (append (org-brain-files t)
                                                          (org-brain-headline-entries)))))
    (when (and entry org-brain-brain-link-adds-child)
      (org-brain-add-relationship entry choice))
    (concat "brain:" (if (org-brain-filep choice) choice (nth 2 choice)))))

(defun org-brain-link-store ()
  "Store a brain: type link from an `org-brain-visualize-mode' buffer."
  (when (eq major-mode 'org-brain-visualize-mode)
    (org-store-link-props
     :type "brain"
     :link (concat "brain:" (org-brain-entry-identifier org-brain--vis-entry))
     :description (org-brain-title org-brain--vis-entry))))

(org-link-set-parameters "brain"
                         :complete 'org-brain-link-complete
                         :follow 'org-brain-goto
                         :store 'org-brain-link-store)

(provide 'org-brain)
;;; org-brain.el ends here
