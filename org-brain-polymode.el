;;; org-brain-polymode.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

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

(require 'polymode)
(require 'org-brain)

;;;; Polymode integration

;; This code has been adapted from Dustin Lacewell's project polybrain
;; Have a look at: https://github.com/dustinlacewell/polybrain.el/

(define-hostmode org-brain-poly-hostmode
  :mode 'org-brain-visualize-mode)

(define-innermode org-brain-poly-innermode
  :mode 'org-mode
  :head-matcher "^[─-]\\{3\\} Entry [─-]+\n"
  :tail-matcher "\\'"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode org-brain-polymode
  :hostmode 'org-brain-poly-hostmode
  :innermodes '(org-brain-poly-innermode)
  (setq-local polymode-move-these-vars-from-old-buffer
              (delq 'buffer-read-only polymode-move-these-vars-from-old-buffer)))

(defun org-brain-polymode-save ()
  "Save entry text to the entry's file."
  (interactive)
  (when (buffer-modified-p)
    (let ((text (save-excursion
                  (goto-char org-brain--vis-entry-text-marker)
                  (end-of-line)
                  (buffer-substring (point) (point-max)))))
      (find-file (org-brain-entry-path org-brain--vis-entry))
      (seq-let (entry-min entry-max) (org-brain-text-positions org-brain--vis-entry)
        (goto-char entry-min)
        (delete-region entry-min entry-max)
        (insert text)
        (unless (looking-at-p "\n")
          (insert "\n\n"))
        (save-buffer)
        (switch-to-buffer (other-buffer (current-buffer) 1))
        (set-buffer-modified-p nil)))))

(define-key org-brain-polymode-map "\C-x\C-s" 'org-brain-polymode-save)

(provide 'org-brain-polymode)
;;; org-brain-polymode.el ends here
