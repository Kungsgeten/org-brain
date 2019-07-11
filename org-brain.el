;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017--2018  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25") (org "9"))
;; Version: 0.5

;;; Commentary:

;; org-brain implements a variant of concept mapping with org-mode, it is
;; inspired by The Brain software (http://thebrain.com).  An org-brain is a
;; network of org-mode entries, where each entry is a file or a headline, and
;; you can get a visual overview of the relationships between the entries:
;; parents, children, siblings and friends.  This visual overview can also be
;; used to browse your entries.  You can think of entries as nodes in a mind map,
;; or pages in a wiki.

;; All org files put into your `org-brain-path' directory will be considered
;; entries in your org-brain.  Headlines with an ID property in your entry file(s)
;; are also considered as entries.

;; Use `org-brain-visualize' to see the relationships between entries, quickly
;; add parents/children/friends/pins to an entry, and open them for editing.

;;; Code:

(require 'org-element)
(require 'org-attach)
(require 'org-macs)
(require 'org-id)
(require 'picture)
(require 'subr-x)

(defgroup org-brain nil
  "Org-mode concept mapping"
  :prefix "org-brain-"
  :group 'org)

;; * Custom vars

(defcustom org-brain-path (expand-file-name "brain" org-directory)
  "The root directory of your org-brain.

`org-mode' files placed in this directory, or its subdirectories,
will be considered org-brain entries."
  :group 'org-brain
  :type '(directory))

(defcustom org-brain-scan-directories-recursively t
  "If subdirectories inside `org-brain-path' are considered part of the brain or not."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-files-extension "org"
  "The extension for entry files in `org-brain-path'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-ignored-resource-links '("fuzzy" "radio" "brain" "brain-child" "brain-parent" "brain-friend")
  "`org-link-types' which shouldn't be shown as resources in `org-brain-visualize'."
  :group 'org-brain
  :type '(repeat string))

(defcustom org-brain-suggest-stored-link-as-resource t
  "If `org-brain-add-resource' should suggest the last link saved with `org-store-link'."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-data-file (expand-file-name ".org-brain-data.el" org-brain-path)
  "Where org-brain data is saved."
  :group 'org-brain
  :type '(directory))

(load org-brain-data-file t)

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

(defcustom org-brain-quit-after-goto nil
  "Should the *org-brain* buffer window close itself after executing a goto command?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-headline-links-only-show-visible t
  "Only show visible parts (descriptions) of headline links.

See the docstring for `org-brain-headline-at' for more info
on how this is implemented."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-file-entries-use-title t
  "If file entries should show their title, when choosing entries from a list.
This can potentially be slow.  If set to nil, the relative
filenames will be shown instead, which is faster."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-visualize-text-hook nil
  "Hook runs after inserting `org-brain-text' in `org-brain-visualize'.

Can be used to prettify the entry text, e.g.
`org-display-inline-images'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-after-visualize-hook nil
  "Hook run after `org-brain-visualize', but before `org-brain-text'.
Can be used to prettify the buffer output, e.g. `ascii-art-to-unicode'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-new-entry-hook nil
  "Hook run after a new headline entry has been created."
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

(defcustom org-brain-cap-mind-map-titles nil
  "Whether to cap entries longer than org-brain-title-max-length in mind map visualization mode."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-entry-separator ";"
  "Can be used as a separator when adding children, parents, or friends.
Doing so allows for adding multiple entries at once."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-visualize-one-child-per-line nil
  "If non-nil, each child of the visualized entry is listed on its own line.
If nil (default), children are filled up to the `fill-column'."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-refile-max-level 1
  "The default max-level used by `org-brain-refile'."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-child-link-name "brain-child"
  "The name for `org-mode' links, creating child relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-parent-link-name "brain-parent"
  "The name for `org-mode' links, creating parent relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-friend-link-name "brain-friend"
  "The name for `org-mode' links, creating friend relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

;; ** Faces

(defface org-brain-title
  '((t . (:inherit 'org-level-1)))
  "Face for the currently selected entry.")

(defface org-brain-wires
  `((t . (:inherit 'font-lock-comment-face :italic nil)))
  "Face for the wires connecting entries.")

(defface org-brain-button
  '((t . (:inherit button)))
  "Face for buttons in the org-brain visualize buffer.")

(defface org-brain-parent
  '((t . (:inherit (font-lock-builtin-face org-brain-button))))
  "Face for the entries' parent nodes.")

(defface org-brain-child
  '((t . (:inherit org-brain-button)))
  "Face for the entries' child nodes.")

(defface org-brain-sibling
  '((t . (:inherit org-brain-child)))
  "Face for the entries' sibling nodes.")

(defface org-brain-friend
  '((t . (:inherit org-brain-button)))
  "Face for the entries' friend nodes.")

(defface org-brain-pinned
  '((t . (:inherit org-brain-button)))
  "Face for pinned entries.")


;; * API

;; An entry is either a string or a list of three strings.
;; If a string, then the entry is a file.
;; If a list, then the entry is a headline:
;; ("file entry" "headline title" "ID")

(defvar org-brain--vis-entry nil
  "The last entry argument to `org-brain-visualize'.")

(defvar org-brain--vis-history nil
  "History previously visualized entries.  Newest first.")

(defvar org-brain-resources-start-re "^[ \t]*:RESOURCES:[ \t]*$"
  "Regular expression matching the first line of a resources drawer.")

(defvar org-brain-pins nil "List of pinned org-brain entries.")

;;;###autoload
(defun org-brain-update-id-locations ()
  "Scan `org-brain-files' using `org-id-update-id-locations'."
  (interactive)
  (org-id-update-id-locations (org-brain-files)))

;;;###autoload
(defun org-brain-switch-brain (directory)
  "Choose another DIRECTORY to be your `org-brain-path'."
  (interactive "D")
  (if (file-equal-p directory org-brain-path)
      (message "Current brain already is %s, no switch" directory)
    (progn
      (setq org-brain-path directory)
      (setq org-brain-data-file (expand-file-name ".org-brain-data.el" org-brain-path))
      (unless (file-exists-p org-brain-data-file)
        (org-brain-save-data))
      (setq org-brain-pins nil)
      (load org-brain-data-file t)
      (org-brain-update-id-locations)
      (message "Switched org-brain to %s" directory))))

(defun org-brain-maybe-switch-brain ()
  "Switch brain to `default-directory' if a file named \".org-brain-data.el\" exists there."
  (when (and (not (file-equal-p default-directory org-brain-path))
             (file-exists-p (expand-file-name ".org-brain-data.el" default-directory)))
    (org-brain-switch-brain default-directory)))

(defun org-brain-filep (entry)
  "Return t if the ENTRY is a (potential) brain file."
  (stringp entry))

(defun org-brain-id-exclude-taggedp (id)
  "Return t if ID is tagged as being excluded from org-brain."
  (org-with-point-at (org-id-find id t)
    (org-brain-entry-at-point-excludedp)))

(defun org-brain-entry-at-point-excludedp ()
  "Return t if the entry at point is tagged as being excluded from org-brain."
  (let ((tags (org-get-tags)))
    (or (member org-brain-exclude-tree-tag tags)
        (and (member org-brain-exclude-children-tag tags)
             (not (member org-brain-exclude-children-tag
                          (org-get-tags nil t)))))))

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

(defun org-brain-entry-path (entry &optional check-title)
  "Get path of org-brain ENTRY.
If CHECK-TITLE is non-nil, consider that ENTRY might be a file entry title."
  (let ((name (if (org-brain-filep entry)
                  (or (and check-title
                           org-brain-file-entries-use-title
                           (cdr
                            (assoc entry
                                   (mapcar (lambda (x)
                                             (cons (concat (file-name-directory x)
                                                           (org-brain-title x))
                                                   x))
                                           (org-brain-files t)))))
                      entry)
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
    (if org-brain-scan-directories-recursively
        (directory-files-recursively
         org-brain-path (format "^[^.].*\\.%s$" org-brain-files-extension))
      (directory-files
       org-brain-path t (format "^[^.].*\\.%s$" org-brain-files-extension)))))

(defun org-brain-replace-links-with-visible-parts (raw-str)
  "Get RAW-STR with its links replaced by their descriptions."
  (let ((ret-str "")
        (start 0)
        match-start)
    (while (setq match-start (string-match org-bracket-link-regexp raw-str start))
      (setq ret-str
            (concat ret-str
                    ;; Include everything not part of the string.
                    (substring-no-properties raw-str start match-start)
                    ;; Include either the link description, or the link
                    ;; destination.
                    (or (match-string-no-properties 3 raw-str)
                        (match-string-no-properties 1 raw-str))))
      (setq start (match-end 0)))
    (concat ret-str (substring-no-properties raw-str start nil))))

(defun org-brain-headline-at (&optional pom)
  "Return the full headline of the entry at POM.

If `org-brain-headline-links-only-show-visible' is nil, the links
will be returned raw (all of the bracket syntax visible.)

If `org-brain-headline-links-only-show-visible' is non-nil,
returns only the visible parts of links in the heading.  (For any
links that have descriptions, only the descriptions will be
returned.)

This is done via regex, and does not depend on org-mode's
visibility rendering/formatting in-buffer."
  (let ((pom (or pom (point))))
    (if org-brain-headline-links-only-show-visible
        (org-brain-replace-links-with-visible-parts (org-entry-get pom "ITEM"))
      (org-entry-get pom "ITEM"))))

(defun org-brain--name-and-id-at-point ()
  "Get name and id of headline entry at point.
Respect excluded entries."
  (unless (org-brain-entry-at-point-excludedp)
    (when-let ((id (org-entry-get (point) "ID")))
      (list (org-brain-headline-at (point)) id))))

(defun org-brain-headline-entries ()
  "Get all org-brain headline entries."
  (with-temp-buffer
    (delay-mode-hooks
      (org-mode)
      (remove nil
              (mapcan
               (lambda (file)
                 (insert-file-contents file nil nil nil 'replace)
                 (let ((file-entry (org-brain-path-entry-name file)))
                   (mapcar (lambda (entry)
                             (cons file-entry entry))
                           (remove nil (org-map-entries
                                        #'org-brain--name-and-id-at-point)))))
               (org-brain-files))))))

(defun org-brain-entry-from-id (id)
  "Get entry from ID."
  (unless org-id-locations (org-id-locations-load))
  (when-let ((path (gethash id org-id-locations)))
    (list (org-brain-path-entry-name path)
          (org-brain-headline-at (org-id-find id t))
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
  (if org-brain-file-entries-use-title
      (if (org-brain-filep entry)
          (concat (file-name-directory entry) (org-brain-title entry))
        (concat (org-brain-entry-name (car entry)) "::" (cadr entry)))
    (if (org-brain-filep entry)
        entry
      (concat (car entry) "::" (cadr entry)))))

(defun org-brain-entry-data (entry)
  "Run `org-element-parse-buffer' on ENTRY text.
Isn't recursive, so do not parse local children."
  (with-temp-buffer
    (insert (org-brain-text entry t))
    (org-element-parse-buffer)))

(defun org-brain-description (entry)
  "Get description of ENTRY.
Descriptions are written like this:

#+BEGIN_description
This is a description.
#+END_description"
  (org-element-map (org-brain-entry-data entry) 'special-block
    (lambda (s-block)
      (when (string-equal (org-element-property :type s-block) "description")
        (org-element-interpret-data (org-element-contents s-block))))
    nil t t))

(defun org-brain--file-targets (file)
  "Return alist of (name . entry-id) for all entries (including the file) in FILE."
  (let* ((file-relative (org-brain-path-entry-name file))
         (file-entry-name (org-brain-entry-name file-relative)))
    (append (list (cons file-entry-name file-relative))
            (with-temp-buffer
              (insert-file-contents file)
              (delay-mode-hooks
                (org-mode)
                (mapcar (lambda (entry)
                          (cons (concat file-entry-name "::" (car entry))
                                (cadr entry)))
                        (remove nil (org-map-entries
                                     #'org-brain--name-and-id-at-point))))))))

(defun org-brain-choose-entries (prompt entries &optional predicate require-match initial-input)
  "PROMPT for one or more ENTRIES, separated by `org-brain-entry-separator'.
ENTRIES can be a list, or 'all which lists all headline and file entries.
Return the prompted entries in a list.
Very similar to `org-brain-choose-entry', but can return several entries.

For PREDICATE, REQUIRE-MATCH and INITIAL-INPUT, see `completing-read'."
  (unless org-id-locations (org-id-locations-load))
  (let* ((targets (if (eq entries 'all)
                      (mapcan #'org-brain--file-targets
                              (org-brain-files))
                    (mapcar (lambda (x)
                              (cons (org-brain-entry-name x)
                                    (if (org-brain-filep x)
                                        x
                                      (nth 2 x))))
                            entries)))
         (choices (completing-read prompt targets
                                   predicate require-match initial-input)))
    (mapcar (lambda (title)
              (let ((id (or (cdr (assoc title targets))
                            title)))
                (or
                 ;; Headline entry exists, return it
                 (org-brain-entry-from-id id)
                 ;; File entry
                 (progn
                   (setq id (split-string id "::" t))
                   (let* ((entry-path (org-brain-entry-path (car id) t))
                          (entry-file (org-brain-path-entry-name entry-path)))
                     (unless (file-exists-p entry-path)
                       (make-directory (file-name-directory entry-path) t)
                       (write-region "" nil entry-path))
                     (if (equal (length id) 2)
                         ;; Create new headline entry in file
                         (with-current-buffer (find-file-noselect entry-path)
                           (goto-char (point-max))
                           (insert (concat "\n* " (cadr id)))
                           (let ((new-id (org-id-get-create)))
                             (run-hooks 'org-brain-new-entry-hook)
                             (list entry-file (cadr id) new-id)))
                       entry-file))))))
            (if org-brain-entry-separator
                (split-string choices org-brain-entry-separator)
              (list choices)))))

(defun org-brain-choose-entry (prompt entries &optional predicate require-match initial-input)
  "PROMPT for an entry from ENTRIES and return it.
ENTRIES can be 'all, which lists all headline and file entries.
For PREDICATE, REQUIRE-MATCH and INITIAL-INPUT, see `completing-read'."
  (let ((org-brain-entry-separator nil))
    (car (org-brain-choose-entries prompt entries predicate require-match initial-input))))

(defun org-brain-keywords (entry)
  "Get alist of `org-mode' keywords and their values in file ENTRY."
  (if (org-brain-filep entry)
      (with-temp-buffer
        (ignore-errors (insert (org-brain-text entry t)))
        (org-element-map (org-element-parse-buffer) 'keyword
          (lambda (kw)
            (cons (org-element-property :key kw)
                  (org-element-property :value kw)))))
    (error "Only file entries have keywords")))

(defun org-brain--missing-id-error (entry)
  "Error message to be shown if id of ENTRY isn't found by `org-id-find'."
  (error "Couldn't find entry %s, try running org-brain-update-id-locations. "
         (org-brain-entry-name entry)))

(defun org-brain-entry-marker (entry)
  "Get marker to ENTRY."
  (if (org-brain-filep entry)
      (let ((path (org-brain-entry-path entry)))
        (if (file-exists-p path)
            (set-marker (make-marker) 0 (find-file-noselect path))
          ;; If file doesn't exists, it is probably an id
          (or (org-id-find entry t)
              (org-brain--missing-id-error entry))))
    (or (org-id-find (nth 2 entry) t)
        (org-brain--missing-id-error entry))))

(defun org-brain-title (entry &optional capped)
  "Get title of ENTRY.  If CAPPED is t, max length is `org-brain-title-max-length'."
  (let ((title
         (if (org-brain-filep entry)
             (or (cdr (assoc "TITLE" (org-brain-keywords entry)))
                 (car (last (split-string entry "/" t))))
           (nth 1 entry))))
    (if (and capped (> org-brain-title-max-length 0) (> (length title) org-brain-title-max-length))
        (concat (substring title 0 (1- org-brain-title-max-length)) "…")
      title)))

(defun org-brain-text (entry &optional all-data)
  "Get the text of ENTRY as string.
Only get the body text, unless ALL-DATA is t.  ALL-DATA will
ignore `org-brain-exclude-children-tag' and
`org-brain-show-children-tag' on file entries."
  (when-let
      ((entry-text
        (if (org-brain-filep entry)
            ;; File entry
            (with-temp-buffer
              (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
              (if (and (not all-data)
                       (let ((filetags (ignore-errors
                                         (split-string
                                          (cdr (assoc "FILETAGS"
                                                      (org-brain-keywords entry)))
                                          ":" t))))
                         (or (member org-brain-show-children-tag filetags)
                             (member org-brain-exclude-children-tag filetags))))
                  ;; Get entire buffer
                  (buffer-substring-no-properties
                   (or (save-excursion
                         (when (re-search-backward "^[#:*]" nil t)
                           (end-of-line)
                           (point)))
                       (point-min))
                   (point-max))
                ;; Get text up to first heading
                (goto-char (point-min))
                (or (outline-next-heading)
                    (goto-char (point-max)))
                (buffer-substring-no-properties
                 (or (unless all-data
                       (save-excursion
                         (when (re-search-backward "^[#:*]" nil t)
                           (end-of-line)
                           (point))))
                     (point-min))
                 (point))))
          ;; Headline entry
          (org-with-point-at (org-brain-entry-marker entry)
            (let ((tags (org-get-tags nil t)))
              (unless (and (member org-brain-exclude-text-tag tags)
                           (not all-data))
                (unless all-data
                  (goto-char (cdr (org-get-property-block)))
                  (end-of-line))
                (let (end)
                  (save-excursion
                    (or (and (not (member org-brain-exclude-children-tag tags))
                             (not (member org-brain-show-children-tag tags))
                             (org-goto-first-child))
                        (org-end-of-subtree t))
                    (setq end (point)))
                  (buffer-substring-no-properties (point) end))))))))
    (with-temp-buffer
      (insert (org-remove-indentation entry-text))
      (goto-char (point-min))
      (when (and (not all-data)
                 (re-search-forward org-brain-resources-start-re nil t))
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

(defun org-brain-descendants (entry)
  "Get all entries which descend from ENTRY.
In other words get all the children, grand children, grand-grand children, etc.
The ENTRY itself is also included in the returned list."
  (let ((checked nil))
    (cl-labels ((collect-descendants
                 (e)
                 (unless (member e checked)
                   (push e checked)
                   (mapc #'collect-descendants (org-brain-children e)))))
      (collect-descendants entry)
      checked)))

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
  (let ((links
         (delete-dups
          (org-element-map (org-brain-entry-data entry) 'link
            (lambda (link)
              (unless (member (org-element-property :type link)
                              org-brain-ignored-resource-links)
                (cons (org-element-property :raw-link link)
                      (when-let ((desc (car (org-element-contents link))))
                        (replace-regexp-in-string "[ \t\n\r]+" " " desc)))))
            nil nil t))))
    (if (org-brain-filep entry)
        links
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (unless (member org-brain-exclude-resouces-tag (org-get-tags nil t))
          (append links
                  ;; Attachments
                  (when-let ((attach-dir (org-attach-dir)))
                    (mapcar (lambda (attachment)
                              (cons (format "file:%s"
                                            (org-link-escape
                                             (expand-file-name attachment attach-dir)))
                                    attachment))
                            (org-attach-file-list attach-dir)))))))))

(defun org-brain--choose-resource (entry)
  "Use `completing-read' to get link to a resource from ENTRY."
  (let ((resources (mapcar (lambda (x)
                             (cons (or (cdr x) (car x)) (car x)))
                           (org-brain-resources entry))))
    (if (equal (length resources) 1)
        (cdar resources)
      (cdr (assoc (completing-read "Resource: " resources nil t) resources)))))

;;;###autoload
(defun org-brain-open-resource (entry)
  "Choose and open a resource from ENTRY.
Uses `org-brain-entry-at-pt' for ENTRY, or asks for it if none at point."
  (interactive (list (or (ignore-errors (org-brain-entry-at-pt))
                         (org-brain-choose-entry "Resource from: " 'all))))
  (org-open-link-from-string (org-brain--choose-resource entry)))

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
                                                        (org-brain-keywords entry)))))
                        (org-split-string kw-values "[ \t]"))))
           ;; Headline entry
           (mapcar
            (lambda (x) (or (org-brain-entry-from-id x) x))
            (org-entry-get-multivalued-property (org-brain-entry-marker entry) property)))))
    (if (equal propertylist '("")) nil propertylist)))

(defun org-brain-add-relationship (parent child)
  "Add external relationship between PARENT and CHILD."
  (when (equal parent child)
    (error "An entry can't be a parent/child to itself"))
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

(defun org-brain-remove-line-if-matching (regex)
  "Delete current line, if matching REGEX."
  (when (string-match regex (buffer-substring (line-beginning-position)
                                              (line-end-position)))
    (kill-whole-line)))

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
        (org-brain-remove-line-if-matching "^#\\+BRAIN_CHILDREN:[[:space:]]*$")
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
        (org-brain-remove-line-if-matching "^#\\+BRAIN_PARENTS:[[:space:]]*$")
        (save-buffer))
    ;; Child = Headline
    (org-entry-remove-from-multivalued-property (org-brain-entry-marker child)
                                                "BRAIN_PARENTS"
                                                (org-brain-entry-identifier parent)))
  (org-save-all-org-buffers))

;; * Buffer commands

;;;###autoload
(defun org-brain-add-child (entry children)
  "Add external CHILDREN (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and let user choose entry.
If chosen CHILD entry doesn't exist, create it as a new file.
Several children can be added, by using `org-brain-entry-separator'."
  (interactive (list (org-brain-entry-at-pt)
                     (org-brain-choose-entries "Add child: " 'all)))
  (dolist (child-entry children)
    (org-brain-add-relationship entry child-entry))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-child-headline (entry child-names)
  "Create new internal child headline(s) to ENTRY named CHILD-NAMES.
Several children can be created, by using `org-brain-entry-separator'.
If called interactively use `org-brain-entry-at-pt' and prompt for children."
  (interactive (list (org-brain-entry-at-pt)
                     (read-string "Add child headline: ")))
  (dolist (child-name (split-string child-names org-brain-entry-separator))
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
          (run-hooks 'org-brain-new-entry-hook)
          (save-buffer))
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (if (org-goto-first-child)
            (open-line 1)
          (org-end-of-subtree t))
        (org-insert-heading nil t)
        (org-do-demote)
        (insert child-name)
        (org-id-get-create)
        (run-hooks 'org-brain-new-entry-hook)
        (save-buffer))))
  (org-brain--revert-if-visualizing))

(define-obsolete-function-alias 'org-brain-new-child 'org-brain-add-child-headline "0.5")

;;;###autoload
(defun org-brain-remove-child (entry child)
  "Remove CHILD from ENTRY.
If called interactively use `org-brain-entry-at-point' and prompt for CHILD."
  (interactive (let ((e (org-brain-entry-at-pt)))
                 (list e (org-brain-choose-entry "Remove child: "
                                                 (org-brain-children e)
                                                 nil t))))
  (if (member child (org-brain--local-children entry))
      (org-brain-delete-entry child)
    (org-brain-remove-relationship entry child))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-parent (entry parents)
  "Add external PARENTS (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for PARENT.
If chosen parent entry doesn't exist, create it as a new file.
Several parents can be added, by using `org-brain-entry-separator'."
  (interactive (list (org-brain-entry-at-pt)
                     (org-brain-choose-entries "Add parent: " 'all)))
  (dolist (parent parents)
    (org-brain-add-relationship parent entry))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-parent (entry parent)
  "Remove external PARENT from ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for PARENT."
  (interactive (let ((e (org-brain-entry-at-pt)))
                 (list e (org-brain-choose-entry "Remove parent: "
        	                                 (org-brain--linked-property-entries
                                                  e "BRAIN_PARENTS")
	                                         nil t))))
  (org-brain-remove-relationship parent entry)
  (org-brain--revert-if-visualizing))

(defun org-brain--internal-add-friendship (entry1 entry2 &optional oneway)
  "Add friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, add ENTRY2 as friend of ENTRY1, but not the other way around."
  (when (equal entry1 entry2)
    (error "Can't have an entry as a friend to itself"))
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
(defun org-brain-add-friendship (entry friends)
  "Add a new FRIENDS (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for FRIENDS.
If chosen friend entry doesn't exist, create it as a new file.
Several friends can be added, by using `org-brain-entry-separator'."
  (interactive (list (org-brain-entry-at-pt)
                     (org-brain-choose-entries "Add friend: " 'all)))
  (dolist (friend-entry friends)
    (org-brain--internal-add-friendship entry friend-entry))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-friendship (entry1 entry2 &optional oneway)
  "Remove friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, then remove ENTRY2 as a friend of ENTRY1, but not vice versa.

If run interactively, use `org-brain-entry-at-pt' as ENTRY1 and prompt for ENTRY2."
  (interactive
   (let ((entry-at-pt (org-brain-entry-at-pt)))
     (list entry-at-pt
           (org-brain-choose-entry "Remove friend: " (org-brain-friends entry-at-pt) nil t))))
  (when (member entry2 (org-brain-friends entry1))
    (if (org-brain-filep entry1)
        ;; Entry1 = File
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry1))
          (goto-char (point-min))
          (re-search-forward "^#\\+BRAIN_FRIENDS:.*$")
          (beginning-of-line)
          (re-search-forward (concat " " (org-brain-entry-identifier entry2)))
          (replace-match "")
          (org-brain-remove-line-if-matching "^#\\+BRAIN_FRIENDS:[[:space:]]*$")
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
(defun org-brain-goto (&optional entry goto-file-func)
  "Goto buffer and position of org-brain ENTRY.
If ENTRY isn't specified, ask for the ENTRY.
Unless GOTO-FILE-FUNC is nil, use `pop-to-buffer-same-window' for opening the entry."
  (interactive)
  (org-brain-stop-wandering)
  (unless entry (setq entry (org-brain-choose-entry "Goto entry: " 'all nil t)))
  (when org-brain-quit-after-goto
    (org-brain-visualize-quit))
  (let ((marker (org-brain-entry-marker entry)))
    (apply (or goto-file-func #'pop-to-buffer-same-window)
           (list (marker-buffer marker)))
    (widen)
    (goto-char (marker-position marker))
    (when (org-at-heading-p)
      (org-show-subtree)))
  entry)

(define-obsolete-function-alias 'org-brain-open 'org-brain-goto "0.4")

;;;###autoload
(defun org-brain-goto-other-window (&optional entry)
  "Goto buffer and position of org-brain ENTRY in other window.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (org-brain-goto entry #'pop-to-buffer))

;;;###autoload
(defun org-brain-goto-end (&optional entry)
  "Like `org-brain-goto', but visits the end of ENTRY.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (if (org-brain-filep (org-brain-goto entry))
      (or (outline-next-heading)
          (goto-char (point-max)))
    (let ((tags (org-get-tags nil t)))
      (or (and (not (member org-brain-exclude-children-tag tags))
               (not (member org-brain-show-children-tag tags))
               (org-goto-first-child))
          (org-end-of-subtree t)))))

;;;###autoload
(defun org-brain-goto-current (&optional same-window)
  "Use `org-brain-goto' on `org-brain-entry-at-pt', in other window..
If run with `\\[universal-argument]', or SAME-WINDOW as t, use current window."
  (interactive "P")
  (if same-window
      (org-brain-goto (org-brain-entry-at-pt))
    (org-brain-goto (org-brain-entry-at-pt) #'pop-to-buffer)))

;;;###autoload
(defun org-brain-goto-child (entry &optional all)
  "Goto a child of ENTRY.
If run interactively, get ENTRY from context.
If ALL is nil, choose only between externally linked children."
  (interactive (list (org-brain-entry-at-pt)))
  (org-brain-goto (org-brain-choose-entry
                   "Goto child: "
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
                   "Goto parent: "
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
                   "Goto friend: "
                   (org-brain--linked-property-entries
                    entry "BRAIN_FRIENDS")
                   nil t)))

;;;###autoload
(defun org-brain-refile (max-level)
  "Run `org-refile' to a heading in `org-brain-files', with set MAX-LEVEL.
If MAX-LEVEL isn't given, use `org-brain-refile-max-level'.
After refiling, all headlines will be given an id."
  (interactive "p")
  (unless current-prefix-arg
    (setq max-level org-brain-refile-max-level))
  (let ((org-refile-targets `((org-brain-files . (:maxlevel . ,max-level))))
        (org-after-refile-insert-hook org-after-refile-insert-hook))
    (add-hook 'org-after-refile-insert-hook
              (lambda () (org-map-tree 'org-id-get-create)))
    (org-refile)))

(defun org-brain--remove-relationships (entry &optional recursive)
  "Remove all external relationships from ENTRY.
Also unpin the entry.

If RECURSIVE is t, remove local children's relationships."
  (dolist (child (org-brain--linked-property-entries
                  entry "BRAIN_CHILDREN"))
    (org-brain-remove-relationship entry child))
  (dolist (parent (org-brain--linked-property-entries
                   entry "BRAIN_PARENTS"))
    (org-brain-remove-relationship parent entry))
  (dolist (friend (org-brain-friends entry))
    (org-brain-remove-friendship entry friend))
  (ignore-errors (org-brain-pin entry -1))
  (when recursive
    (dolist (child (org-brain--local-children entry))
      (org-brain--remove-relationships child t))))

;;;###autoload
(defun org-brain-rename-file (file-entry new-name)
  "Rename FILE-ENTRY to NEW-NAME.
Both arguments should be relative to `org-brain-path' and should
not contain `org-brain-files-extension'."
  (interactive (let ((entry (org-brain-choose-entry
                             "Rename file: " (org-brain-files t) nil t)))
                 (list entry (read-string "New filename: " entry))))
  (let ((newpath (org-brain-entry-path new-name))
        (oldpath (org-brain-entry-path file-entry)))
    (if (file-exists-p newpath)
        (error "There's already a file %s" newpath)
      (let ((children (org-brain--linked-property-entries file-entry "BRAIN_CHILDREN"))
            (parents (org-brain--linked-property-entries file-entry "BRAIN_PARENTS"))
            (friends (org-brain-friends file-entry)))
        (org-brain--remove-relationships file-entry)
        (org-save-all-org-buffers)
        (make-directory (file-name-directory newpath) t)
        (with-temp-file newpath (insert-file-contents oldpath))
        (org-brain-delete-entry file-entry t)
        (org-brain-update-id-locations)
        (dolist (child children)
          (org-brain-add-relationship new-name child))
        (dolist (parent parents)
          (org-brain-add-relationship parent new-name))
        (dolist (friend friends)
          (org-brain--internal-add-friendship new-name friend))
        (message "Renamed %s to %s" file-entry new-name)))))

;;;###autoload
(defun org-brain-delete-entry (entry &optional noconfirm)
  "Delete ENTRY and all of its local children.
If run interactively, ask for the ENTRY.
If NOCONFIRM is nil, ask if we really want to delete."
  (interactive
   (list (org-brain-choose-entry "Delete entry: " 'all nil t)
         nil))
  (let ((local-children (org-brain--local-children entry)))
    (when (or noconfirm
              (yes-or-no-p
               (format "%s and its %d local children will be deleted. Are you sure? "
                       (org-brain-entry-name entry)
                       (length local-children))))
      (dolist (child local-children)
        (org-brain-delete-entry child t))
      (org-brain--remove-relationships entry)
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
(defun org-brain-insert-relationships (entry &optional recursive)
  "Insert an `org-mode' list of relationships to ENTRY.
Local children are not included in the list.
If run interactively, get ENTRY from context.

Normally the list is inserted at point, but if RECURSIVE is t
insert at end of ENTRY.  Then recurse in the local (grand)children
of ENTRY and insert there too."
  (interactive (list (org-brain-entry-at-pt)))
  (cl-flet ((list-to-items
             (list)
             (when list
               `(unordered
                 ,@(mapcar (lambda (x)
                             (list (org-make-link-string
                                    (format "brain:%s" (org-brain-entry-identifier x))
                                    (org-brain-title x))))
                           list)))))
    (save-excursion
      (when recursive
        (org-brain-goto-end entry)
        (newline 2))
      (insert
       ":RELATIONSHIPS:\n"
       (org-list-to-org `(unordered
                          ,(remq nil `("Parents"
                                       ,(list-to-items (org-brain-parents entry))))
                          ,(remq nil `("Children"
                                       ,(list-to-items (org-brain--linked-property-entries
                                                        entry "BRAIN_CHILDREN"))))
                          ,(remq nil `("Friends"
                                       ,(list-to-items (org-brain-friends entry))))))
       "\n:END:\n")))
  (when recursive
    (dolist (child (org-brain--local-children entry))
      (org-brain-insert-relationships child t))))

;;;###autoload
(defun org-brain-archive (entry)
  "Use `org-archive-subtree-default' on ENTRY.
If run interactively, get ENTRY from context.
Before archiving, recursively run `org-brain-insert-relationships' on ENTRY.
Remove external relationships from ENTRY, in order to clean up the brain."
  (interactive (list (org-brain-entry-at-pt)))
  (when (org-brain-filep entry)
    (user-error "Only headline entries can be archived"))
  (org-brain-insert-relationships entry t)
  (org-brain--remove-relationships entry t)
  (org-with-point-at (org-brain-entry-marker entry)
    (org-archive-subtree-default))
  (delete entry org-brain--vis-history)
  (org-save-all-org-buffers)
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-pin (entry &optional status)
  "Change if ENTRY is pinned or not.
If run interactively, get ENTRY from context.

If STATUS is positive, pin the entry.  If negative, remove the pin.
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
          (when (assoc "TITLE" (org-brain-keywords entry))
            (re-search-forward "^#\\+TITLE:")
            (kill-whole-line))
          (insert (format "#+TITLE: %s\n" title))
          (save-buffer)))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (org-edit-headline title)
      (save-buffer)
      (setf (nth 1 org-brain--vis-entry) title)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-set-tags (entry)
  "Modify the ENTRY tags.
Use `org-set-tags-command' on headline ENTRY.
Instead sets #+FILETAGS on file ENTRY.
If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (if (org-brain-filep entry)
      (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
        (let ((tag-str (read-string "FILETAGS: "
                                    (mapconcat #'identity org-file-tags ":"))))
          (goto-char (point-min))
          (when (assoc "FILETAGS" (org-brain-keywords entry))
            (re-search-forward "^#\\+FILETAGS:")
            (kill-whole-line))
          (insert (format "#+FILETAGS: %s\n" tag-str)))
        ;; From org.el
        (let ((org-inhibit-startup-visibility-stuff t)
              (org-startup-align-all-tables nil))
          (when (boundp 'org-table-coordinate-overlays)
            (mapc #'delete-overlay org-table-coordinate-overlays)
            (setq org-table-coordinate-overlays nil))
          (org-save-outline-visibility 'use-markers (org-mode-restart)))
        (save-buffer))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-set-tags-command)
      (save-buffer)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-headline-to-file (entry)
  "Convert headline ENTRY to a file entry.
Prompt for name of the new file.
If interactive, also prompt for ENTRY."
  (interactive (list (org-brain-choose-entry "Convert entry: "
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
      (make-directory (file-name-directory path) t)
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
        (org-brain--internal-add-friendship new-entry friend))
      (when (equal entry org-brain--vis-entry)
        (setq org-brain--vis-entry new-entry))
      (when (member entry org-brain-pins)
        (org-brain-pin entry -1)
        (org-brain-pin new-entry 1)))))

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
  (when (y-or-n-p "This function is meant for old configurations.  Are you sure you want to scan for links? ")
    (dolist (file (org-brain-files))
      (with-temp-buffer
        (insert-file-contents file)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (string-equal (org-element-property :type link) "brain")
              (org-brain-add-relationship
               (org-brain-path-entry-name file)
               (car (split-string (org-element-property :path link) "::"))))))))))

;; * Sorting

(defun org-brain-title< (entry1 entry2)
  "Return non-nil if title of ENTRY1 is less than ENTRY2 in lexicographic order.
Case is significant."
  (string< (org-brain-title entry1) (org-brain-title entry2)))

(defvar org-brain-visualize-sort-function 'org-brain-title<
  "How to sort lists of relationships when visualizing.
Should be a function which accepts two entries as arguments.
The function returns t if the first entry is smaller than the second.

If you don't want to sort the relationships, set this to `ignore'.")

;; * Visualize

;;;###autoload
(defun org-brain-visualize (entry &optional nofocus nohistory wander)
  "View a concept map with ENTRY at the center.

When run interactively, prompt for ENTRY and suggest
`org-brain-entry-at-pt'.  By default, the choices presented is
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
   (progn
     (org-brain-maybe-switch-brain)
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
                'all)
               ((equal choices 'files)
                (org-brain-files t))
               ((equal choices 'root)
                (make-directory org-brain-path t)
                (mapcar #'org-brain-path-entry-name
                        (directory-files org-brain-path t (format "\\.%s$" org-brain-files-extension)))))
         nil nil def-choice)))))
  (unless wander (org-brain-stop-wandering))
  (with-current-buffer (get-buffer-create "*org-brain*")
    (read-only-mode 1)
    (setq-local default-directory (file-name-directory (org-brain-entry-path entry)))
    (org-brain-maybe-switch-brain)
    (unless (eq org-brain--vis-entry entry)
      (setq org-brain--vis-entry entry)
      (setq org-brain-mind-map-parent-level (default-value 'org-brain-mind-map-parent-level))
      (setq org-brain-mind-map-child-level (default-value 'org-brain-mind-map-child-level)))
    (let ((inhibit-read-only t)
          (entry-pos))
      (delete-region (point-min) (point-max))
      (org-brain--vis-pinned)
      (if org-brain-visualizing-mind-map
          (setq entry-pos (org-brain-mind-map org-brain--vis-entry org-brain-mind-map-parent-level org-brain-mind-map-child-level))
        (insert "\n\n")
        (org-brain--vis-parents-siblings entry)
        ;; Insert entry title
        (let ((title (org-brain-title entry)))
          (let ((half-title-length (/ (string-width title) 2)))
            (if (>= half-title-length (current-column))
                (delete-char (- (current-column)))
              (ignore-errors (delete-char (- half-title-length)))))
          (setq entry-pos (point))
          (insert (propertize title
                              'face 'org-brain-title
                              'aa2u-text t))
          (org-brain--vis-friends entry)
          (org-brain--vis-children entry)))
      (when (and org-brain-show-resources)
        (org-brain--vis-resources (org-brain-resources entry)))
      (if org-brain-show-text
          (org-brain--vis-text entry)
        (run-hooks 'org-brain-after-visualize-hook))
      (unless (eq major-mode 'org-brain-visualize-mode)
        (org-brain-visualize-mode))
      (goto-char entry-pos))
    (unless nofocus
      (pop-to-buffer "*org-brain*")
      (when (and (not nohistory)
                 (not (equal entry (car org-brain--vis-history)))
                 (< (length org-brain--vis-history) 15))
        (push entry org-brain--vis-history)))))

;;;###autoload
(defun org-brain-visualize-entry-at-pt ()
  "Use `org-brain-visualize' on the `org-brain-entry-at-pt'.
Useful if wanting to visualize the current `org-mode' entry."
  (interactive)
  (org-brain-visualize (org-brain-entry-at-pt)))

;;;###autoload
(defun org-brain-visualize-random (&optional restrict-to)
  "Run `org-brain-visualize' on a random org-brain entry.
If RESTRICT-TO is given, then only choose among those entries.

If called interactively with `\\[universal-argument]' then
restrict to descendants of the visualized entry."
  (interactive (when (equal current-prefix-arg '(4))
                 (list (org-brain-descendants org-brain--vis-entry))))
  (let ((entries (or restrict-to
                     (append (org-brain-files t)
                             (org-brain-headline-entries)))))
    (org-brain-visualize (nth (random (length entries)) entries) nil nil t)))

(defvar org-brain-wander-timer nil
  "A timer running `org-brain-visualize-random' at a set interval.

Can be (de)activated by `org-brain-visualize-wander'.")

(defun org-brain-stop-wandering ()
  "Cancels `org-brain-wander-timer', if it is active."
  (when (member org-brain-wander-timer timer-list)
    (cancel-timer org-brain-wander-timer)
    t))

(defun org-brain-visualize-wander (&optional restrict-to)
  "Run `org-brain-visualize-random' every `org-brain-wander-interval'.
If RESTRICT-TO is given, then only wander among those entries.

If called interactively with `\\[universal-argument]' then
restrict to descendants of the visualized entry starting the wandering session.

Wandering is cancelled by many org-brain commands, but can also be
cancelled manually with `org-brain-stop-wandering'."
  (interactive (when (equal current-prefix-arg '(4))
                 (list (org-brain-descendants org-brain--vis-entry))))
  (if (org-brain-stop-wandering)
      (message "Wandering stopped.")
    (setq org-brain-wander-timer (run-at-time nil org-brain-wander-interval #'org-brain-visualize-random restrict-to))
    (message "Wandering started.")))

(defun org-brain-visualize-quit ()
  "Like `quit-window', but also stops `org-brain-visualize-wander'."
  (interactive)
  (org-brain-stop-wandering)
  (quit-window))

(defun org-brain-insert-visualize-button (entry &optional face)
  "Insert a button, running `org-brain-visualize' on ENTRY when clicked."
  (insert-text-button
   (org-brain-title entry (or (not org-brain-visualizing-mind-map)
                              org-brain-cap-mind-map-titles))
   'action (lambda (_x) (org-brain-visualize entry))
   'follow-link t
   'help-echo (org-brain-description entry)
   'aa2u-text t
   'face (or face 'org-brain-button)))

(defun org-brain-insert-resource-button (resource &optional indent)
  "Insert a new line with a RESOURCE button, indented by INDENT spaces."
  (insert (make-string (or indent 0) ?\ ) "\n- ")
  (run-hook-with-args 'org-brain-after-resource-button-functions (car resource))
  (insert-text-button
   (or (cdr resource) (car resource))
   'action (lambda (_x)
             (org-open-link-from-string (car resource)))
   'follow-link t
   'aa2u-text t))

(defun org-brain-add-resource (link &optional description prompt entry)
  "Insert LINK with DESCRIPTION in an entry.
If PROMPT is non nil, use `org-insert-link' even if not being run interactively.
If ENTRY is omitted, try to get it from context or prompt for it."
  (interactive (or (and org-brain-suggest-stored-link-as-resource
                        (when-let ((last-stored-link (car org-stored-links)))
                          (list (substring-no-properties (car last-stored-link))
                                (cadr last-stored-link)
                                t)))
                   '(nil)))
  (unless entry
    (setq entry (or (ignore-errors (org-brain-entry-at-pt))
                    (org-brain-choose-entry "Insert link in entry: " 'all))))
  (cl-flet ((insert-resource-link
             ()
             (unless (and link (not prompt))
               (setq link (read-string "Insert link: " link))
               (when (string-match org-bracket-link-regexp link)
                 (let ((linkdesc (match-string 3 link)))
                   (when (and (not description) linkdesc)
                     (setq description linkdesc))
                   (setq link (match-string 1 link))))
               (setq description (read-string "Link description: " description)))
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

(defun org-brain-visualize-eldoc-function ()
  "Return description of org-brain entry button at point."
  (ignore-errors
    (plist-get (text-properties-at (button-at (point)))
               'help-echo)))

(define-derived-mode org-brain-visualize-mode
  special-mode  "Org-brain Visualize"
  "Major mode for `org-brain-visualize'.
\\{org-brain-visualize-mode-map}"
  (setq-local revert-buffer-function #'org-brain-visualize-revert)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'org-brain-visualize-eldoc-function))

;; ** Keybindings

(define-key org-brain-visualize-mode-map "p" 'org-brain-add-parent)
(define-key org-brain-visualize-mode-map "P" 'org-brain-remove-parent)
(define-key org-brain-visualize-mode-map "c" 'org-brain-add-child)
(define-key org-brain-visualize-mode-map "C" 'org-brain-remove-child)
(define-key org-brain-visualize-mode-map "*" 'org-brain-add-child-headline)
(define-key org-brain-visualize-mode-map "h" 'org-brain-add-child-headline)
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
(define-key org-brain-visualize-mode-map "r" 'org-brain-open-resource)
(define-key org-brain-visualize-mode-map "a" 'org-brain-visualize-attach)
(define-key org-brain-visualize-mode-map "A" 'org-brain-archive)
(define-key org-brain-visualize-mode-map "b" 'org-brain-visualize-back)
(define-key org-brain-visualize-mode-map "\C-y" 'org-brain-visualize-paste-resource)
(define-key org-brain-visualize-mode-map "T" 'org-brain-set-tags)
(define-key org-brain-visualize-mode-map "q" 'org-brain-visualize-quit)
(define-key org-brain-visualize-mode-map "w" 'org-brain-visualize-random)
(define-key org-brain-visualize-mode-map "W" 'org-brain-visualize-wander)
(define-key org-brain-visualize-mode-map "m" 'org-brain-visualize-mind-map)
(define-key org-brain-visualize-mode-map "+" 'org-brain-show-descendant-level)
(define-key org-brain-visualize-mode-map "-" 'org-brain-hide-descendant-level)
(define-key org-brain-visualize-mode-map "z" 'org-brain-show-ancestor-level)
(define-key org-brain-visualize-mode-map "Z" 'org-brain-hide-ancestor-level)

;; ** Drawing helpers

(defun org-brain--vis-pinned ()
  "Insert pinned entries.
Helper function for `org-brain-visualize'."
  (insert "PINNED:")
  (dolist (pin (sort (copy-sequence org-brain-pins) org-brain-visualize-sort-function))
    (insert "  ")
    (org-brain-insert-visualize-button pin 'org-brain-pinned))
  (insert "\n"))

(defun org-brain--insert-wire (&rest strings)
  "Helper function for drawing fontified wires in the org-brain visualization buffer."
  (insert (propertize (apply 'concat strings) 'face 'org-brain-wires)))

(defun org-brain--vis-parents-siblings (entry)
  "Insert parents and siblings of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((siblings (org-brain-siblings entry)))
    (let ((parent-positions nil)
          (max-width 0))
      (dolist (parent (sort siblings (lambda (x y)
                                       (funcall org-brain-visualize-sort-function
                                                (car x) (car y)))))
        (let ((children-links (cdr parent))
              (col-start (+ 3 max-width))
              (parent-title (org-brain-title (car parent))))
          (org-goto-line 4)
          (mapc
           (lambda (child)
             (picture-forward-column col-start)
             (org-brain--insert-wire (make-string (1+ (string-width parent-title)) ?\ ) "+-")
             (org-brain-insert-visualize-button child 'org-brain-sibling)
             (setq max-width (max max-width (current-column)))
             (newline (forward-line 1)))
           (sort children-links org-brain-visualize-sort-function))
          (org-goto-line 4)
          (forward-line (1- (length children-links)))
          (picture-forward-column col-start)
          (push (cons (picture-current-line)
                      (+ (current-column) (/ (string-width parent-title) 2)))
                parent-positions)
          (org-brain-insert-visualize-button (car parent) 'org-brain-parent)
          (setq max-width (max max-width (current-column)))
          (when children-links
            (org-brain--insert-wire "-")
            (delete-char (+ 1 (string-width parent-title))))))
      ;; Draw lines
      (when parent-positions
        (let ((maxline (line-number-at-pos (point-max))))
          ;; Bottom line
          (org-goto-line maxline)
          (picture-forward-column (cdar (last parent-positions)))
          (picture-move-down 1)
          (org-brain--insert-wire (make-string (1+ (- (cdar parent-positions)
                                                      (cdar (last parent-positions))))
                                               ?-))
          ;; Lines from parents to bottom
          (dolist (pos parent-positions)
            (org-goto-line (car pos))
            (picture-forward-column (cdr pos))
            (while (< (line-number-at-pos (point))
                      maxline)
              (picture-move-down 1)
              (org-brain--insert-wire "|")
              (unless (looking-at-p "\n") (delete-char 1)))
            (picture-move-down 1)
            (ignore-errors
              (delete-char 1))
            (org-brain--insert-wire "+"))
          ;; Line to main entry
          (move-to-column (/ (+ (cdar (last parent-positions))
                                (cdar parent-positions))
                             2))
          (delete-char 1)
          (when (> (length parent-positions) 1)
            (org-brain--insert-wire "+")
            (backward-char 1)
            (picture-move-down 1)
            (org-brain--insert-wire "|")
            (picture-move-down 1))
          (org-brain--insert-wire "V"))))
    (picture-move-down 1)))

(defun org-brain--vis-children (entry)
  "Insert children of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((children (org-brain-children entry)))
    (insert "\n\n")
    (dolist (child (sort children org-brain-visualize-sort-function))
      (let ((child-title (org-brain-title child)))
        (when (or org-brain-visualize-one-child-per-line
                  (> (+ (current-column) (length child-title))
                     fill-column))
          (insert "\n"))
        (org-brain-insert-visualize-button child 'org-brain-child)
        (insert "  ")))))

(defun org-brain--vis-friends (entry)
  "Insert friends of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((friends (org-brain-friends entry)))
    (org-brain--insert-wire " <-> ")
    (dolist (friend (sort friends org-brain-visualize-sort-function))
      (let ((column (current-column)))
        (org-brain-insert-visualize-button friend 'org-brain-friend)
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
              (insert (with-temp-buffer
                        (insert text)
                        (delay-mode-hooks
                          (org-mode)
                          (font-lock-ensure (point-min) (point-max))
                          (buffer-string))))
              (run-hooks 'org-brain-visualize-text-hook))
          (run-hooks 'org-brain-after-visualize-hook)))
    (run-hooks 'org-brain-after-visualize-hook)))

;; * Mind-map

(defun org-brain-map-create-indentation (level)
  "Return a string of spaces, length determined by indentation LEVEL."
  (make-string (* level 2) ? ))

(defun org-brain-insert-recursive-child-buttons (entry max-level indent)
  "Use `org-brain-insert-visualize-button' on ENTRY and its children.
Also insert buttons for grand-children, up to MAX-LEVEL.
Each button is indented, starting at level determined by INDENT."
  (insert (org-brain-map-create-indentation indent))
  (org-brain-insert-visualize-button entry 'org-brain-child)
  (insert "\n")
  (dolist (child (and (> max-level 0) (sort (org-brain-children entry) org-brain-visualize-sort-function)))
    (org-brain-insert-recursive-child-buttons child (1- max-level) (1+ indent))))

(defun org-brain-tree-depth (tree)
  "Return depth of nested TREE."
  (if (atom tree)
      0
    (1+ (cl-reduce #'max (mapcar #'org-brain-tree-depth tree)))))

(defun org-brain-recursive-parents (entry max-level)
  "Return a tree of ENTRY and its (grand)parents, up to MAX-LEVEL."
  (cons (org-brain-title entry)
        (when (> max-level 0)
          (mapcar (lambda (x) (org-brain-recursive-parents x (1- max-level)))
                  (org-brain-parents entry)))))

(defun org-brain-insert-recursive-parent-buttons (entry max-level indent)
  "Use `org-brain-insert-visualize-button' on ENTRY and its parents.
Also insert buttons for grand-parents, up to MAX-LEVEL.
Each button is indented, starting at level determined by INDENT."
  (dolist (parent (and (> max-level 0)
                       (sort (org-brain-parents entry) org-brain-visualize-sort-function)))
    (org-brain-insert-recursive-parent-buttons parent (1- max-level) (1- indent)))
  (insert (org-brain-map-create-indentation indent))
  (org-brain-insert-visualize-button entry 'org-brain-parent)
  (insert "\n"))

(defun org-brain-mind-map (entry parent-max-level children-max-level)
  "Insert a tree of buttons for the parents and children of ENTRY.
Insert friends to ENTRY in a row above the tree.
Will also insert grand-parents up to PARENT-MAX-LEVEL, and
children up to CHILDREN-MAX-LEVEL.
Return the position of ENTRY in the buffer."
  (insert "FRIENDS:")
  (dolist (friend (sort (org-brain-friends entry) org-brain-visualize-sort-function))
    (insert "  ")
    (org-brain-insert-visualize-button friend 'org-brain-friend))
  (insert "\n\n")
  (let ((indent (1- (org-brain-tree-depth (org-brain-recursive-parents entry parent-max-level))))
        (entry-pos))
    (dolist (parent (sort (org-brain-siblings entry) (lambda (x y)
                                                       (funcall org-brain-visualize-sort-function
                                                                (car x) (car y)))))
      (org-brain-insert-recursive-parent-buttons (car parent) (1- parent-max-level) (1- indent))
      (dolist (sibling (sort (cdr parent) org-brain-visualize-sort-function))
        (insert (org-brain-map-create-indentation indent))
        (org-brain-insert-visualize-button sibling 'org-brain-sibling)
        (insert "\n")))
    (insert (org-brain-map-create-indentation indent))
    (setq entry-pos (point))
    (insert (propertize (org-brain-title entry)
                        'face 'org-brain-title
                        'aa2u-text t) "\n")
    (dolist (child (sort (org-brain-children entry) org-brain-visualize-sort-function))
      (org-brain-insert-recursive-child-buttons child (1- children-max-level) (1+ indent)))
    entry-pos))

(defvar org-brain-visualizing-mind-map nil)
(defvar-local org-brain-mind-map-child-level 1)
(defvar-local org-brain-mind-map-parent-level 1)

(defun org-brain-visualize-mind-map ()
  "Toggle mind-map view of `org-brain-visualize'."
  (interactive)
  (when (eq major-mode 'org-brain-visualize-mode)
    (setq org-brain-visualizing-mind-map (not org-brain-visualizing-mind-map))
    (org-brain-visualize org-brain--vis-entry)))

;; ** Show/hide nested levels
(defun org-brain-show-descendant-level ()
  "Show one more level of descendant entries to the right in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (cl-incf org-brain-mind-map-child-level)
  (org-brain--revert-if-visualizing))

(defun org-brain-hide-descendant-level ()
  "Hide the rightmost level of descendant entries in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (when (> org-brain-mind-map-child-level 1)
    (cl-decf org-brain-mind-map-child-level))
  (org-brain--revert-if-visualizing))

(defun org-brain-show-ancestor-level ()
  "Show one more level of ancestor entries to the left in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (cl-incf org-brain-mind-map-parent-level)
  (org-brain--revert-if-visualizing))

(defun org-brain-hide-ancestor-level ()
  "Hide the leftmost level of ancestor entries in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (when (> org-brain-mind-map-parent-level 1)
    (cl-decf org-brain-mind-map-parent-level))
  (org-brain--revert-if-visualizing))

(define-obsolete-function-alias
  'org-brain-visualize-add-grandchild 'org-brain-show-descendant-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-remove-grandchild 'org-brain-hide-descendant-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-add-grandparent 'org-brain-show-ancestor-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-remove-grandparent 'org-brain-hide-ancestor-level "0.5")

;; * Brain link

(defun org-brain-link-complete (&optional link-type)
  "Create an org-link target string to a file in `org-brain-path'.
LINK-TYPE will be \"brain\" by default."
  (setq link-type (or link-type "brain"))
  (let ((entry (ignore-errors (org-brain-entry-at-pt)))
        (choice (org-brain-choose-entry "Entry: " 'all)))
    (cond ((string-equal link-type org-brain-child-link-name)
           (org-brain-add-relationship entry choice))
          ((string-equal link-type org-brain-parent-link-name)
           (org-brain-add-relationship choice entry))
          ((string-equal link-type org-brain-friend-link-name)
           (org-brain--internal-add-friendship entry choice)))
    (concat link-type ":" (if (org-brain-filep choice) choice (nth 2 choice)))))

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

(org-link-set-parameters org-brain-child-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-child-link-name))
                         :follow 'org-brain-goto)

(org-link-set-parameters org-brain-parent-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-parent-link-name))
                         :follow 'org-brain-goto)

(org-link-set-parameters org-brain-friend-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-friend-link-name))
                         :follow 'org-brain-goto)

;; * Brain switch link

(defun org-brain--switch-link-complete ()
  "Create an org-link target string to an org-brain and one of its entries."
  (let* ((org-brain-path (read-directory-name "Brain dir: " org-brain-path))
         (entry (org-brain-choose-entry "Entry: " (append (org-brain-files t)
                                                          (org-brain-headline-entries)))))
    (concat "brainswitch:" org-brain-path
            "::"
            (if (org-brain-filep entry)
                entry
              (nth 2 entry)))))

(defun org-brain--switch-and-visualize (directory entry)
  "Switch brain to DIRECTORY and visualize ENTRY.
ENTRY should be a string; an id in the case of an headline entry."
  (org-brain-switch-brain directory)
  (org-brain-visualize (or (org-brain-entry-from-id entry) entry)))

(defun org-brain--switch-link-follow (link)
  "Follow function for brainswitch links."
  (let ((link-parts (split-string link "::")))
    (org-brain--switch-and-visualize (car link-parts)
                                     (cadr link-parts))))

(org-link-set-parameters "brainswitch"
                         :complete 'org-brain--switch-link-complete
                         :follow 'org-brain--switch-link-follow)

;; * Helm integration

(with-eval-after-load "helm"
  (defun helm-brain--add-children (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-parents (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (or (org-brain-entry-from-id candidate) candidate) (org-brain-entry-at-pt)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-friends (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain--internal-add-friendship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--delete-entries (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-delete-entry (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain--archive (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-archive (or (org-brain-entry-from-id candidate) candidate))))

  (defvar helm-brain--actions
    (helm-make-actions
     "Visualize" (lambda (x)
                   (org-brain-visualize (or (org-brain-entry-from-id x) x)))
     "Add children" 'helm-brain--add-children
     "Add parents" 'helm-brain--add-parents
     "Add friends" 'helm-brain--add-friends
     "Delete" 'helm-brain--delete-entries
     "Archive" 'helm-brain--archive))

  (defun helm-brain--source ()
    (helm-build-sync-source "Brain"
                            :candidates (mapcan #'org-brain--file-targets
                                                (org-brain-files))
                            :action 'helm-brain--actions))

  (defun helm-brain ()
    "Use `helm' to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc.
Supports selecting multiple entries at once."
    (interactive)
    (helm :sources (helm-brain--source))))

;; * Ivy integration

(with-eval-after-load "ivy"
  (defun counsel-brain ()
    "Use Ivy to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc."
    (interactive)
    (let ((targets (mapcan #'org-brain--file-targets
                           (org-brain-files))))
      (ivy-read "Org-brain: "
                targets
                :require-match t
                :action (lambda (x)
                          (org-brain-visualize (or (org-brain-entry-from-id (cdr x))
                                                   (cdr x))))
                :preselect (ignore-errors
                             (org-brain-entry-name
                              (org-brain-entry-at-pt)))
                :caller 'counsel-brain)))

  (defun counsel-brain--add-child (child)
    (org-brain-add-relationship (org-brain-entry-at-pt)
                                (or (org-brain-entry-from-id (cdr child))
                                    (cdr child)))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--add-parent (parent)
    (org-brain-add-relationship (or (org-brain-entry-from-id (cdr parent))
                                    (cdr parent))
                                (org-brain-entry-at-pt))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--add-friend (friend)
    (org-brain--internal-add-friendship (org-brain-entry-at-pt)
                                        (or (org-brain-entry-from-id (cdr friend))
                                            (cdr friend)))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--delete (x)
    (org-brain-delete-entry (or (org-brain-entry-from-id (cdr x)) (cdr x))))

  (defun counsel-brain--archive (x)
    (org-brain-archive (or (org-brain-entry-from-id (cdr x)) (cdr x))))

  (ivy-set-actions
   'counsel-brain
   '(("c" counsel-brain--add-child "add as child")
     ("p" counsel-brain--add-parent "add as parent")
     ("f" counsel-brain--add-friend "add as friend")
     ("d" counsel-brain--delete "delete")
     ("a" counsel-brain--archive "archive"))))

(provide 'org-brain)
;;; org-brain.el ends here
