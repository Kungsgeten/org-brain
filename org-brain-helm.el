;;; org-brain-helm.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017--2020 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25.1") (org "9.2"))
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

(require 'org-element)
(require 'org-attach)
(require 'org-agenda)
(require 'org-macs)
(require 'org-id)
(require 'picture)
(require 'subr-x)
(require 'seq)
(require 'helm)
(require 'org-brain)

;;;; Helm integration

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

(defun helm-brain--select (_c)
  (dolist (candidate (helm-marked-candidates))
    (org-brain-select (or (org-brain-entry-from-id candidate) candidate) 1)))

(defun helm-brain--unselect (_c)
  (dolist (candidate (helm-marked-candidates))
    (org-brain-select (or (org-brain-entry-from-id candidate) candidate) -1)))

(defvar helm-brain--actions
  (helm-make-actions
   "Visualize" (lambda (x)
                 (org-brain-visualize (or (org-brain-entry-from-id x) x)))
   "Add children" 'helm-brain--add-children
   "Add parents" 'helm-brain--add-parents
   "Add friends" 'helm-brain--add-friends
   "Delete" 'helm-brain--delete-entries
   "Archive" 'helm-brain--archive
   "Select" 'helm-brain--select
   "Unselect" 'helm-brain--unselect))

(defvar helm-brain--source
  (helm-make-source "Brain" 'helm-source-sync
    :candidates #'org-brain--all-targets
    :action 'helm-brain--actions))

(defvar helm-brain--fallback-source
  (helm-make-source "New entry" 'helm-source-dummy
    :action (helm-make-actions
             "Visualize" (lambda (x)
                           (org-brain-visualize (org-brain-get-entry-from-title x)))
             "Add children" 'helm-brain--add-children
             "Add parents" 'helm-brain--add-parents
             "Add friends" 'helm-brain--add-friends)))

(defun helm-brain ()
  "Use `helm' to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc.
Supports selecting multiple entries at once."
  (interactive)
  (helm :sources '(helm-brain--source helm-brain--fallback-source)))

(provide 'org-brain-helm)
;;; org-brain-helm.el ends here
