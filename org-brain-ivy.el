;;; org-brain-ivy.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017--2020 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25.1") (org "9.2") (ivy "0.13.4"))
;; Version: 0.94

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

(require 'ivy)
(require 'org-brain)

;;;; Ivy integration

(defun counsel-brain ()
  "Use Ivy to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc."
  (interactive)
  (let ((targets (org-brain--all-targets)))
    (ivy-read "Org-brain: "
              targets
              :action (lambda (x)
                        (org-brain-visualize
                         (if (stringp x)
                             (org-brain-get-entry-from-title x)
                           (or (org-brain-entry-from-id (cdr x))
                               (cdr x)))))
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

(defun counsel-brain--select (x)
  (org-brain-select (or (org-brain-entry-from-id (cdr x)) (cdr x)) 1))

(defun counsel-brain--unselect (x)
  (org-brain-select (or (org-brain-entry-from-id (cdr x)) (cdr x)) -1))

(ivy-set-actions
 'counsel-brain
 '(("c" counsel-brain--add-child "add as child")
   ("p" counsel-brain--add-parent "add as parent")
   ("f" counsel-brain--add-friend "add as friend")
   ("d" counsel-brain--delete "delete")
   ("a" counsel-brain--archive "archive")
   ("s" counsel-brain--select "select")
   ("S" counsel-brain--unselect "unselect")))

(provide 'org-brain-ivy)
;;; org-brain-ivy.el ends here
