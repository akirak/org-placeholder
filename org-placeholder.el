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
(require 'org-agenda)

(declare-function org-ql-view--format-element "ext:org-ql")

(defvar org-capture-entry)
(defvar org-capture-initial)

(defgroup org-placeholder nil
  ""
  :group 'org)

(defcustom org-placeholder-sort-function
  #'org-placeholder-default-sort
  "Function used to sort entries in `org-placeholder-view'.

It takes a function that takes two Org headline elements as
arguments."
  :type 'function)

(defcustom org-placeholder-default-capture-template
  "* %i\n%?"
  "Template for `org-capture' used in the package."
  :type 'string)

(defvar org-placeholder-marker-table nil)

;;;; Common

(defun org-placeholder-read-bookmark-name (prompt)
  (let ((completion-ignore-case t))
    (completing-read prompt (org-placeholder--bookmarks) nil t)))

(defun org-placeholder--bookmarks ()
  (bookmark-maybe-load-default-file)
  (seq-filter (lambda (record)
                (eq (bookmark-get-handler record)
                    'org-placeholder-view))
              bookmark-alist))

(defun org-placeholder--regexp-for-level (level)
  (rx-to-string `(and bol ,(make-string level ?\*) " ")))

;;;; Creating a placeholder bookmark

;;;###autoload
(defun org-placeholder-store-bookmark ()
  "Store a bookmark to a view of the current subtree."
  (interactive)
  (let* ((filename (thread-last
                     (org-base-buffer (current-buffer))
                     (buffer-file-name)
                     (abbreviate-file-name)))
         (heading (unless (org-before-first-heading-p)
                    (org-get-heading t t t t)))
         (bookmark-name (read-from-minibuffer
                         "Bookmark name: "
                         (if heading
                             (format "%s:%s" filename heading)
                           (or (org-placeholder--find-keyword "title")
                               filename))))
         (point (point-marker))
         (record (with-current-buffer (org-base-buffer (current-buffer))
                   (org-with-wide-buffer
                    (goto-char point)
                    (bookmark-make-record-default 'no-file 'no-context)))))
    (if heading
        (bookmark-prop-set record 'id (org-id-get-create))
      (bookmark-prop-set record 'filename filename))
    (bookmark-prop-set record 'handler #'org-placeholder-view)
    (bookmark-store bookmark-name record t)
    (bookmark-save)))

(defun org-placeholder--find-keyword (keyword &optional pred)
  (declare (indent 1))
  (save-excursion
    (goto-char (point-min))
    (let ((bound (save-excursion
                   (re-search-forward org-heading-regexp)))
          (case-fold-search t))
      (catch 'prop
        (while (re-search-forward org-keyword-regexp bound t)
          (when (org-string-equal-ignore-case keyword (match-string 1))
            (let ((value (match-string-no-properties 2)))
              (when-let (ret (if (not pred)
                                 value
                               (funcall pred value)))
                (throw 'prop ret)))))))))

;;;; Find

(defcustom org-placeholder-prefix-todo t
  "Whether to prefix each entry with the todo state.

If this option is non-nil, each item in
`org-placeholder-find-or-create' will be prefixed with its todo
state."
  :type 'boolean)

(defcustom org-placeholder-suffix-tags t
  "Whether to suffix each entry with tags.

If this option is non-nil, each item in
`org-placeholder-find-or-create' will be prefixed with its todo
state."
  :type 'boolean)

;;;###autoload
(defun org-placeholder-find-or-create (&optional bookmark-name initial-input)
  (interactive (list (when current-prefix-arg
                       (org-placeholder-read-bookmark-name "Placeholder: "))))
  (unless org-placeholder-marker-table
    (setq org-placeholder-marker-table (make-hash-table :test #'equal)))
  (let (candidates
        (node-parent-table (make-hash-table :test #'equal))
        (node-group-table (make-hash-table :test #'equal))
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
             (complete-with-action action candidates string pred)))
         (run (type root-name root-level end-of-root)
           (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
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
                               (todo (when org-placeholder-prefix-todo
                                       (match-string 2)))
                               (tags (when org-placeholder-suffix-tags
                                       (org-make-tag-string (org-get-tags))))
                               (heading (concat (if todo
                                                    (concat todo " ")
                                                  "")
                                                (org-link-display-format
                                                 (match-string-no-properties 4))
                                                (if tags
                                                    (concat " " tags)
                                                  ""))))
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
                            (puthash heading marker org-placeholder-marker-table)))))))))))))
      (dolist (root-name root-names)
        (cl-etypecase (org-placeholder-bookmark-root root-name)
          (marker (save-current-buffer
                    (org-with-point-at root
                      (org-with-wide-buffer
                       (org-show-subtree)
                       (run (org-placeholder--subtree-type)
                            root-name
                            (org-outline-level)
                            (save-excursion (org-end-of-subtree)))))))
          (buffer (with-current-buffer root
                    (org-with-wide-buffer
                     (org-show-all)
                     (run (org-placeholder--buffer-type)
                          root-name
                          0
                          nil))))))
      (unwind-protect
          (let ((input (or (and initial-input
                                (car (member-ignore-case initial-input candidates)))
                           (completing-read "Find a node: " #'completions nil nil
                                            initial-input))))
            (if-let (marker (gethash input org-placeholder-marker-table))
                (with-current-buffer (marker-buffer marker)
                  (pop-to-buffer (current-buffer))
                  (goto-char marker)
                  (run-hooks 'org-ql-find-goto-hook))
              (org-placeholder-capture-input input bookmark-name)))
        (clrhash org-placeholder-marker-table)))))

;;;###autoload
(defun org-placeholder-find-or-create-1 (name)
  "Find or create NAME from all placeholders.

This is an alternative API to `org-placeholder-find-or-create'
which is suitable for integration with embark package."
  (interactive "s")
  (org-placeholder-find-or-create nil name))

;;;###autoload
(defun org-placeholder-capture-input (input &optional bookmark-names)
  "Create a heading into a placeholder."
  (interactive "s")
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
      (thread-first
        (completing-read (format "Add \"%s\": " input)
                         #'completions)
        (gethash marker-map)
        (org-placeholder--capture input)))))

(defun org-placeholder--capture (marker initial)
  (pcase-let*
      ((`(,template ,pre-capture ,post-capture)
        (org-with-point-at marker
          (list (org-entry-get marker "PLACEHOLDER_CAPTURE_TEMPLATE" t)
                (org-entry-get marker "PLACEHOLDER_PRE_CAPTURE" t)
                (org-entry-get marker "PLACEHOLDER_POST_CAPTURE" t))))
       (org-capture-entry `("" ""
                            entry
                            (function
                             (lambda ()
                               (org-goto-marker-or-bmk ,marker)))
                            ,(if template
                                 (read template)
                               org-placeholder-default-capture-template)))
       (org-capture-initial initial))

    (when pre-capture
      (let ((func (read pre-capture)))
        (condition-case-unless-debug _
            (if (fboundp func)
                (funcall func)
              (error "Unbound function: %s" func)))))
    (org-capture)
    (when post-capture
      (let ((func (read post-capture)))
        (condition-case-unless-debug _
            (if (fboundp func)
                (funcall func)
              (error "Unbound function: %s" func)))))))

(defun org-placeholder-map-parents (bookmark-name fn)
  "Call a function at each parent heading of the items."
  (declare (indent 1))
  (cl-flet
      ((f (type root-level bound)
         (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
           (pcase-exhaustive type
             (`nested
              (while (re-search-forward regexp1 bound t)
                (let ((bound (save-excursion (org-end-of-subtree))))
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
                      (funcall fn root-level))))))))))
    (cl-etypecase (org-placeholder-bookmark-root bookmark-name)
      (marker (save-current-buffer
                (org-with-point-at root
                  (org-with-wide-buffer
                   (org-show-subtree)
                   (f (org-placeholder--subtree-type)
                      (org-outline-level)
                      (save-excursion (org-end-of-subtree)))))))
      (buffer (with-current-buffer root
                (org-with-wide-buffer
                 (org-show-all)
                 (goto-char (point-min))
                 (f (org-placeholder--buffer-type)
                    0
                    nil)))))))

;;;; Views like what org-ql-view provides

(defvar org-placeholder-view-name nil)

(defvar org-placeholder-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'org-placeholder-revert-view)
    map))

(define-derived-mode org-placeholder-view-mode org-agenda-mode
  "Org Placeholder View")

;;;###autoload
(defun org-placeholder-view (bookmark)
  (interactive (list (org-placeholder-read-bookmark-name "View: ")))
  (cl-assert (bookmark-get-bookmark-record bookmark))
  (let* ((bookmark-name (if (stringp bookmark)
                            bookmark
                          (car bookmark)))
         (buffer (get-buffer-create (format "*View<%s>*" bookmark-name))))
    (with-current-buffer buffer
      (read-only-mode t)
      (org-placeholder-view-mode)
      (setq-local org-placeholder-view-name bookmark-name)
      (org-placeholder-revert-view))
    (funcall (if (eq this-command 'org-placeholder-view)
                 #'pop-to-buffer
               #'pop-to-buffer-same-window)
             buffer)))

(defun org-placeholder-revert-view (&rest _args)
  (interactive)
  (let ((inhibit-read-only t)
        (root (org-placeholder-bookmark-root
               (or org-placeholder-view-name
                   (error "org-placeholder-view-name is not set")))))
    (erase-buffer)
    (org-placeholder--insert-view root)
    (org-agenda-finalize)
    (goto-char (point-min))))

(defun org-placeholder-bookmark-root (bookmark-name)
  (pcase-exhaustive (bookmark-get-bookmark-record bookmark-name)
    (`nil
     (error "Bookmark record %s does not exist" bookmark-name))
    ((and (app (alist-get 'id) id)
          (guard id))
     (or (org-id-find id 'marker)
         (error "Entry for ID %s is not found" id)))
    ((and (app (alist-get 'filename) filename)
          (guard filename))
     (or (find-buffer-visiting filename)
         (find-file-noselect filename)))))

(defun org-placeholder--subtree-type ()
  (org-placeholder--parse-type
   (org-entry-get nil "PLACEHOLDER_TYPE")))

(defun org-placeholder--buffer-type ()
  (org-placeholder--parse-type
   (save-excursion
     (goto-char (point-min))
     (org-placeholder--find-keyword "PROPERTY"
       (lambda (value)
         (when (string-match (rx bol "PLACEHOLDER_TYPE="
                                 (group (+ (not (any space)))))
                             value)
           (match-string 1 value)))))))

(defun org-placeholder--parse-type (string)
  (pcase-exhaustive string
    ("nested" 'nested)))

(defun org-placeholder--insert-view (root)
  (require 'org-ql-view)
  (let ((type (cl-etypecase root
                (marker (org-with-point-at root
                          (org-placeholder--subtree-type)))
                (buffer (with-current-buffer root
                          (org-with-wide-buffer
                           (org-placeholder--buffer-type))))))
        root-heading
        strings)
    (cl-flet
        ((run (type root-level end-of-root)
           (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
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
                  (push "" strings)))))))
      ;; FIXME: save outline visibility
      (cl-etypecase root
        (marker (save-current-buffer
                  (org-with-point-at root
                    (org-with-wide-buffer
                     (org-show-subtree)
                     (setq root-heading (propertize (org-get-heading t t t t)
                                                    'org-marker root))
                     (run type
                          (org-outline-level)
                          (save-excursion (org-end-of-subtree)))))))
        (buffer (with-current-buffer root
                  (org-with-wide-buffer
                   (org-show-all)
                   (setq root-heading (propertize (buffer-name)
                                                  'org-marker (copy-marker (point-min))))
                   (run type
                        0
                        nil))))))
    (insert root-heading "\n\n")
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

;;;; Embark integration

(defun org-placeholder-item-marker (item)
  "Return the marker to an item.

This is intended for use in `embark-transformer-alist' for
writing your own transformer.

This should be matched against \\='org-placeholder-item category."
  (gethash item org-placeholder-marker-table))

(provide 'org-placeholder)
;;; org-placeholder.el ends here
