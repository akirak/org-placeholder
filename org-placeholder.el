;;; org-placeholder.el --- Placeholders for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (org "9.6") (org-ql "0.6"))
;; Keywords: hypermedia, outlines
;; URL: https://github.com/akirak/org-placeholder

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'bookmark)
(require 'org)

(defgroup org-placeholder nil
  ""
  :group 'org)

(defcustom org-placeholder-sort-function
  #'org-placeholder-default-sort
  "Function used to sort entries in `org-placeholder-view'.

It takes a function that takes two Org headline elements as
arguments."
  :type 'function)

;;;; Common

(defun org-placeholder-read-bookmark-name (prompt)
  (completing-read prompt (org-placeholder--bookmarks) nil t))

(defun org-placeholder--bookmarks ()
  (bookmark-maybe-load-default-file)
  (seq-filter (pcase-lambda (`(,_ . ,alist))
                (assq 'org-placeholder alist))
              bookmark-alist))

(defun org-placeholder--regexp-for-level (level)
  (rx-to-string `(and bol ,(make-string level ?\*) " ")))

;;;; Find

;;;###autoload
(defun org-placeholder-find-or-create (&optional bookmark-name initial-input)
  (interactive (list (when current-prefix-arg
                       (org-placeholder-read-bookmark-name "Placeholder: "))))
  (let (candidates
        (node-parent-table (make-hash-table :test #'equal))
        (node-group-table (make-hash-table :test #'equal))
        (marker-table (make-hash-table :test #'equal))
        (root-names (if bookmark-name
                        (list bookmark-name)
                      (mapcar #'car (org-placeholder--bookmarks)))))
    (cl-labels
        ((annotator (candidate)
           (if-let (s (gethash candidate node-parent-table))
               (concat " " s)
             ""))
         (group (candidate transform)
           (if transform
               candidate
             (gethash candidate node-group-table)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-placeholder-item)
                           (cons 'annotation-function #'annotator)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred))))
      (dolist (root-name root-names)
        (pcase-exhaustive (org-placeholder--view-args root-name)
          (`(,root ,type)
           (save-current-buffer
             (org-with-point-at root
               (org-with-wide-buffer
                (let* ((root-level (org-outline-level))
                       (end-of-root (save-excursion (org-end-of-subtree)))
                       (regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
                  (pcase-exhaustive type
                    (`nested
                     (while (re-search-forward regexp1 end-of-root t)
                       (let ((group-heading (org-no-properties (org-get-heading t t t t)))
                             (bound (save-excursion (org-end-of-subtree)))
                             (target-level (+ root-level
                                              2
                                              (if-let (str (org-entry-get nil "PLACEHOLDER_LEVEL"))
                                                  (string-to-number str)
                                                0)))
                             (olp-string nil))
                         (while (re-search-forward org-complex-heading-regexp bound t)
                           (unless (save-match-data (org-in-archived-heading-p))
                             (let* ((level (- (match-end 1)
                                              (match-beginning 1)))
                                    (marker (copy-marker (match-beginning 0)))
                                    (heading (org-link-display-format
                                              (match-string-no-properties 4))))
                               (cond
                                ((< level target-level)
                                 (let ((olp (org-get-outline-path t t)))
                                   (setq olp-string
                                         (org-no-properties
                                          (org-format-outline-path
                                           (seq-drop olp (1+ root-level)))))))
                                ((= level target-level)
                                 (push heading candidates)
                                 (puthash heading olp-string node-parent-table)
                                 (puthash heading
                                          (if bookmark-name
                                              group-heading
                                            (format "%s: %s" root-name group-heading))
                                          node-group-table)
                                 (puthash heading marker marker-table)))))))))))))))))
      (let ((input (or (and initial-input
                            (car (member-ignore-case initial-input candidates)))
                       (completing-read "Find a node: " #'completions nil nil
                                        initial-input))))
        (if-let (marker (gethash input marker-table))
            (with-current-buffer (marker-buffer marker)
              (pop-to-buffer (current-buffer))
              (goto-char marker)
              (run-hooks 'org-ql-find-goto-hook))
          (org-placeholder-capture-input input bookmark-name))))))

(defun org-placeholder-capture-input (input &optional bookmark-names)
  (let ((root-name-map (make-hash-table :test #'equal))
        (marker-map (make-hash-table :test #'equal))
        candidates)
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (gethash candidate root-name-map)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'category)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred)))
         (add-parent (root-name root-level)
           (let ((candidate (org-no-properties
                             (org-format-outline-path
                              (seq-drop (org-get-outline-path t t)
                                        root-level)))))
             (push candidate candidates)
             (puthash candidate root-name root-name-map)
             (puthash candidate (point-marker) marker-map))))
      (dolist (bookmark (or (if (stringp bookmark-names)
                                (list bookmark-names)
                              bookmark-names)
                            (mapcar #'car (org-placeholder--bookmarks))))
        (org-placeholder-map-parents bookmark
          (apply-partially #'add-parent bookmark)))
      (let* ((parent (completing-read (format "Add \"%s\": " input)
                                      #'completions))
             (marker (gethash parent marker-map))
             (org-capture-initial input)
             (root-name (gethash parent root-name-map))
             (template (cdr (assq 'org-placeholder-capture-template
                                  (bookmark-get-bookmark-record root-name))))
             (org-capture-entry `("" ""
                                  entry
                                  (function
                                   (lambda ()
                                     (org-goto-marker-or-bmk ,marker)))
                                  ,(or template "* %i\n%?"))))
        (org-capture)))))

(defun org-placeholder-map-parents (bookmark-name fn)
  "Call a function at each parent heading of the items."
  (declare (indent 1))
  (pcase-exhaustive (org-placeholder--view-args bookmark-name)
    (`(,root ,type)
     (save-current-buffer
       (org-with-point-at root
         (org-with-wide-buffer
          (let* ((root-level (org-outline-level))
                 (end-of-root (save-excursion (org-end-of-subtree)))
                 (regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
            (pcase-exhaustive type
              (`nested
               (while (re-search-forward regexp1 end-of-root t)
                 (let ((group-heading (org-no-properties (org-get-heading t t t t)))
                       (bound (save-excursion (org-end-of-subtree))))
                   (if-let (str (org-entry-get nil "PLACEHOLDER_LEVEL"))
                       (let ((target-level (+ root-level
                                              2 (string-to-number str))))
                         (while (re-search-forward org-complex-heading-regexp bound t)
                           (when (and (= (1- target-level)
                                         (- (match-end 1) (match-beginning 1)))
                                      (not (org-in-archived-heading-p)))
                             (save-excursion
                               (beginning-of-line)
                               (funcall fn root-level)))))
                     (save-excursion
                       (beginning-of-line)
                       (funcall fn root-level))))))))))))))

;;;; Views like what org-ql-view provides

(defvar org-placeholder-view-name nil)

(defun org-placeholder-view (bookmark-name)
  (interactive (list (org-placeholder-read-bookmark-name "View: ")))
  (cl-assert (org-placeholder--view-args bookmark-name))
  (with-current-buffer (get-buffer-create (format "*View<%s>*" bookmark-name))
    (setq-local org-placeholder-view-name bookmark-name)
    (read-only-mode t)
    (org-placeholder-revert-view)))

(defun org-placeholder-revert-view (&rest _args)
  (let ((inhibit-read-only t)
        (args (org-placeholder--view-args org-placeholder-view-name)))
    (erase-buffer)
    (org-agenda-mode)
    (apply #'org-placeholder--insert-view args)
    (pop-to-buffer (current-buffer))
    (org-agenda-finalize)
    (goto-char (point-min))))

(defun org-placeholder--view-args (bookmark-name)
  (if-let* ((record (bookmark-get-bookmark-record bookmark-name))
            (type (cdr (assq 'org-placeholder record)))
            (id (cdr (assq 'id record)))
            (marker (org-id-find id 'marker)))
      (list marker type)
    (error "Either a record, type, id, or marker is missing")))

(defun org-placeholder--insert-view (root type)
  (cl-check-type root marker)
  (require 'org-ql-view)
  (let (strings)
    (save-current-buffer
      (org-with-point-at root
        (org-with-wide-buffer
         (let* ((root-level (org-outline-level))
                (end-of-root (save-excursion (org-end-of-subtree)))
                (regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
           (pcase-exhaustive type
             (`nested
              (while (re-search-forward regexp1 end-of-root t)
                (push (thread-first
                        (org-get-heading t t t t)
                        (propertize 'org-marker (point-marker)))
                      strings)
                (let ((bound (save-excursion (org-end-of-subtree)))
                      (target-level (+ root-level
                                       2
                                       (if-let (str (org-entry-get nil "PLACEHOLDER_LEVEL"))
                                           (string-to-number str)
                                         0)))
                      (first-section t)
                      items)
                  (cl-flet
                      ((emit (&optional no-empty-line)
                         (setq strings (append (thread-last
                                                 items
                                                 (seq-sort (or org-placeholder-sort-function
                                                               #'ignore))
                                                 (mapcar #'org-ql-view--format-element))
                                               strings))
                         (if first-section
                             (setq first-section nil)
                           (unless no-empty-line
                             (push "" strings)))
                         (setq items nil)))
                    (while (re-search-forward org-complex-heading-regexp bound t)
                      (unless (org-in-archived-heading-p)
                        (let ((level (org-outline-level)))
                          (cond
                           ((< level target-level)
                            (emit)
                            (when-let (olp (seq-drop (org-get-outline-path t t)
                                                     (1+ root-level)))
                              (push (thread-first
                                      (format " (%s)" (org-no-properties
                                                       (org-format-outline-path olp)))
                                      (propertize 'face 'font-lock-doc-face
                                                  'org-marker (point-marker)))
                                    strings)))
                           ((= level target-level)
                            (beginning-of-line)
                            (push (org-ql--add-markers (org-element-headline-parser))
                                  items)
                            (end-of-line))))))
                    (emit t)))
                (push "" strings))))))))
    (insert (string-join (nreverse strings) "\n"))))

;;;; Default sorting function

(defvar org-placeholder-keyword-order nil)

(defun org-placeholder-keyword-order (element)
  (when-let (kwd (org-element-property :todo-keyword element))
    (or (cdr (assoc kwd org-placeholder-keyword-order))
        (let ((order (with-current-buffer
                         (marker-buffer (or (org-element-property :org-marker element)
                                            (org-element-property :org-hd-marker element)))
                       (seq-position org-todo-keywords-1 kwd #'equal))))
          (push (cons kwd order)
                org-placeholder-keyword-order)
          order))))

(defun org-placeholder-default-sort (a b)
  (or (let ((ta (org-placeholder-keyword-order a))
            (tb (org-placeholder-keyword-order b)))
        (if (and ta tb)
            (< ta tb)
          tb))
      (let ((sa (org-element-property :scheduled a))
            (sb (org-element-property :scheduled b)))
        (if (and sa sb)
            (time-less-p (org-timestamp-to-time sb)
                         (org-timestamp-to-time sa))
          sb))
      (let ((pa (org-element-property :priority a))
            (pb (org-element-property :priority b)))
        (if (and pa pb)
            (> pa pb)
          pb))))

(provide 'org-placeholder)
;;; org-placeholder.el ends here
