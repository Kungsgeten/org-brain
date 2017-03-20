;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25") (org "9") (dash "2.12"))
;; Version: 0.1

;;; Commentary:

;; org-brain implements a variant of concept mapping with org-mode, it is
;; inspired by The Brain software (http://thebrain.com). An org-brain is a
;; network of org-mode files, and you can get a visual overview of the
;; relationships between the files (kind of like a mind map): a file's parents
;; (other files which link to the file) and its children (files linked to from
;; the file). Files can also be browsed between via this overview.

;; All org files put into your `org-brain-path' directory will be considered as
;; "entries" (nodes, thoughts, pages, whatever you like to call them) in your
;; org-brain. An entry can link to other entries via an `org-mode' brain link,
;; for instance [[brain:index]]. You can also include search patterns in the
;; link, just like the `org-mode' file-link: [[brain:index::*Programming]].

;; You can use `org-brain-visualize' to see the relationships between entries,
;; quickly add parents/children to an entry, and open them for editing. In this
;; view you may also have pinned entries, which will be shown at all times. To
;; pin an entry, add #+BRAIN_PIN: on a line in the beginning of the entry file.

;;; Code:

(require 'org-element)
(require 'dash)
(require 'picture)

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
  "Default name for a headline containing links to org-brain entries.

This will be used by `org-brain-new-child'."
  :group 'org-brain
  :type '(string))

(defun org-brain-files (&optional relative)
  "Get all org files (recursively) in `org-brain-path'.
If RELATIVE is t, then return relative paths and remove org extension."
  (let ((files (directory-files-recursively org-brain-path "\\.org$")))
    (if relative
        (mapcar #'org-brain-path-entry-name files)
      files)))

(defun org-brain-pins ()
  "Get list of org-brain entries with \"BRAIN_PIN\" keyword."
  (-non-nil
   (mapcar
    (lambda (entry)
      (when (assoc "BRAIN_PIN" (org-brain-keywords entry))
        entry))
    (org-brain-files t))))

(defun org-brain-path-entry-name (path)
  "Get PATH as an org-brain entry name."
  (file-name-sans-extension (file-relative-name path org-brain-path)))

(defun org-brain-entry-path (entry)
  "Get path of org-brain ENTRY."
  (expand-file-name (org-link-unescape (concat entry ".org")) org-brain-path))

(defun org-brain-parents (entry)
  "Get list of org-brain entries which links to ENTRY."
  (-non-nil
   (mapcar
    (lambda (brainfile)
      (let ((brainfile-entry (org-brain-path-entry-name brainfile)))
        (unless (string-equal brainfile-entry entry)
          (org-element-map
              (with-temp-buffer
                (insert-file-contents brainfile)
                (org-element-parse-buffer))
              'link
            (lambda (link)
              (when (and (string-equal (org-element-property :type link) "brain")
                         (string-equal (car (split-string (org-element-property :path link) "::"))
                                       entry))
                brainfile-entry))
            nil t))))
    (org-brain-files))))

(defun org-brain-children (entry &optional exclude)
  "Get list of org-brain entries linked to from ENTRY.
You can choose to EXCLUDE an entry from the list."
  (delete
   exclude
   (delete-dups
    (org-element-map
        (with-temp-buffer
          (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
          (org-element-parse-buffer))
        'link
      (lambda (link)
        (when (string-equal (org-element-property :type link) "brain")
          (let ((link-entry (car (split-string (org-element-property :path link) "::"))))
            (unless (string-equal link-entry entry) link-entry))))))))

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

(defun org-brain-insert-link ()
  "Insert a link to an org-brain entry and suggest a description."
  (interactive)
  (let* ((file (completing-read "Entry: " (org-brain-files t)))
         (title (org-brain-title file))
         (desc (read-string "Description: " title)))
    (insert (org-make-link-string (concat "brain:" file) desc))))

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
  (let ((split-path (split-string (or entry (completing-read "Entry: " (org-brain-files t)))
                                  "::")))
    (org-open-file (org-brain-entry-path (car split-path))
                   t nil (cadr split-path))))

(defun org-brain-link-activate-func (start end path bracketp)
  "Links to non-existing org-brain files should have a different face."
  (when (not (member (org-link-unescape (car (split-string path "::")))
                     (org-brain-files t)))
    (put-text-property start end 'face '(org-warning org-link))))

(defun org-brain-link-complete ()
  "Create an org-link target string to a file in `org-brain-path'."
  (concat "brain:" (completing-read "Entry: " (org-brain-files t))))

(defun org-brain-link-tooltip (window object position)
  "Org-brain entry links have the entry's title as tooltip."
  (save-excursion
    (goto-char position)
    (org-brain-title
     (car (split-string (org-element-property :path (org-element-context)) "::")))))

(org-link-set-parameters "brain"
                         :complete 'org-brain-link-complete
                         :follow 'org-brain-open
                         :activate-func 'org-brain-link-activate-func
                         :help-echo 'org-brain-link-tooltip)

(defun org-brain-new-child (entry child)
  "In org-brain ENTRY, add a link to CHILD."
  (let ((entry-path (org-brain-entry-path entry)))
    (org-save-all-org-buffers)
    (unless (file-exists-p entry-path)
      (with-temp-file entry-path
        (make-directory (file-name-directory entry-path) t)))
    (or (org-map-entries (lambda ()
                           (end-of-line)
                           (insert (format "\n- [[brain:%s]]" child))
                           (save-buffer))
                         "+brainchildren"
                         (list entry-path))
        (with-current-buffer (get-file-buffer entry-path)
          (goto-char (point-max))
          (insert (format "\n\n* %s    :brainchildren:\n- [[brain:%s]]"
                          org-brain-children-headline-default-name child))
          (save-buffer)))))

(defun org-brain-insert-visualize-button (entry)
  "Insert a button, which runs `org-brain-visualize' on ENTRY when clicked."
  (insert-text-button
   (org-brain-title entry)
   'action (lambda (x) (org-brain-visualize entry))))

(defvar org-brain--visualizing-entry nil
  "The last entry argument to `org-brain-visualize'.")

;;;###autoload
(defun org-brain-visualize (entry &optional ignored-siblings)
  "View a concept map with ENTRY at the center.
IGNORED-SIBLINGS, a list of org-brain entries, can be provided to
ignore certain sibling links to show."
  (interactive
   (list (completing-read "Entry: " (org-brain-files t))))
  (with-current-buffer (get-buffer-create "*org-brain*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    ;; Pinned entries
    (insert "PINNED:")
    (mapc (lambda (pin)
            (insert "  ")
            (org-brain-insert-visualize-button pin))
          (org-brain-pins))
    (insert "\n\n\n")
    ;; Draw parent entries and siblings
    (let ((parent-positions nil)
          (max-width 0))
      (mapc (lambda (parent)
              (push parent ignored-siblings)
              (let ((children-links (-difference (org-brain-children parent entry)
                                                 ignored-siblings))
                    (col-start (+ 3 max-width))
                    (parent-title (org-brain-title parent)))
                (goto-line 4)
                (mapc
                 (lambda (child)
                   (picture-forward-column col-start)
                   (insert (make-string (1+ (length parent-title)) ?\ ) "/ ")
                   (org-brain-insert-visualize-button child)
                   (setq max-width (max max-width (current-column)))
                   (newline (forward-line 1))
                   (push child ignored-siblings))
                 children-links)
                (goto-line 4)
                (forward-line (1- (length children-links)))
                (picture-forward-column col-start)
                (push (cons (picture-current-line)
                            (+ (current-column) (/ (length parent-title) 2)))
                      parent-positions)
                (org-brain-insert-visualize-button parent)
                (setq max-width (max max-width (current-column)))
                (when children-links
                  (delete-char (length parent-title)))))
            (org-brain-parents entry))
      ;; Draw lines
      (when parent-positions
        (let ((maxline (line-number-at-pos (point-max))))
          ;; Bottom line
          (goto-line maxline)
          (picture-forward-column (cdr (-last-item parent-positions)))
          (picture-move-down 1)
          (insert (make-string (1+ (- (cdar parent-positions)
                                      (cdr (-last-item parent-positions))))
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
          (move-to-column (/ (+ (cdr (-last-item parent-positions))
                                (cdar parent-positions))
                             2))
          (delete-char 1)
          (when (> (length parent-positions) 1)
            (insert "*")
            (backward-char 1)
            (picture-move-down 1)
            (insert "|")
            (picture-move-down 1))
          (insert "V"))))
    ;; Insert main entry name
    (picture-move-down 1)
    (let ((half-title-length (/ (length (org-brain-title entry)) 2)))
      (if (>= half-title-length (current-column))
          (delete-char (- (current-column)))
        (ignore-errors
          (delete-char (- half-title-length)))))
    (let ((entry-pos (point)))
      (insert (org-brain-title entry) "\n\n")
      ;; Insert entry children
      (mapc (lambda (child)
              (let ((child-title (org-brain-title child)))
                (when (> (+ (current-column) (length child-title))
                         fill-column)
                  (insert "\n"))
                (org-brain-insert-visualize-button child)
                (insert "  ")))
            (org-brain-children entry))
      ;; Insert headlines in entry file
      (insert "\n\n-----------------------------------------------\n\n")
      (org-element-map (with-temp-buffer
                         (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
                         (org-element-parse-buffer))
          'headline
        (lambda (headline)
          (insert (make-string (org-element-property :level headline) ?*) " ")
          (insert-text-button
           (org-element-property :raw-value headline)
           'action (lambda (x)
                     (org-open-file (org-brain-entry-path entry)
                                    nil nil
                                    (concat "*" (org-element-property
                                                 :raw-value headline)))))
          (insert "\n")))
      ;; Finishing
      (display-buffer "*org-brain*")
      (org-brain-visualize-mode)
      (goto-char entry-pos)))
  (setq org-brain--visualizing-entry entry))

(defun org-brain-visualize-revert (ignore-auto noconfirm)
  "Revert function for `org-brain-visualize-mode'."
  (org-brain-visualize org-brain--visualizing-entry))

(defun org-brain-visualize-open ()
  "Open the entry file last visited by `org-brain-visualize'."
  (interactive)
  (org-brain-open org-brain--visualizing-entry))

(defun org-brain-visualize-add-child (child)
  "Add CHILD link to entry last visited by `org-brain-visualize'."
  (interactive
   (list (completing-read "Child: " (org-brain-files t))))
  (org-brain-new-child org-brain--visualizing-entry child)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(defun org-brain-visualize-add-parent (parent)
  "In PARENT add link to entry last visited by `org-brain-visualize'."
  (interactive
   (list (completing-read "Parent: " (org-brain-files t))))
  (org-brain-new-child parent org-brain--visualizing-entry)
  (when (string-equal (buffer-name) "*org-brain*")
    (revert-buffer)))

(define-derived-mode org-brain-visualize-mode
  special-mode  "Org-brain Visualize"
  "Major mode for `org-brain-visualize'.
\\{org-brain-visualize-mode-map}"
  (setq revert-buffer-function #'org-brain-visualize-revert))

(define-key org-brain-visualize-mode-map "p" 'org-brain-visualize-add-parent)
(define-key org-brain-visualize-mode-map "c" 'org-brain-visualize-add-child)
(define-key org-brain-visualize-mode-map "j" 'forward-button)
(define-key org-brain-visualize-mode-map "k" 'backward-button)
(define-key org-brain-visualize-mode-map [?\t] 'forward-button)
(define-key org-brain-visualize-mode-map [backtab] 'backward-button)
(define-key org-brain-visualize-mode-map "o" 'org-brain-visualize-open)
(define-key org-brain-visualize-mode-map "f" 'org-brain-visualize)

(provide 'org-brain)
;;; org-brain.el ends here
