;;; org-placeholder.el --- Placeholders for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.6"))
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

;; This library help the user search and create headings in bookmarked
;; locations.

;;; Code:

(require 'bookmark)
(require 'org)
(require 'org-agenda)
(require 'eieio)

(declare-function org-element-property "org-element")
(declare-function org-element-headline-parser "org-element")
(declare-function org-ql--add-markers "ext:org-ql")
(declare-function org-ql-view--format-element "ext:org-ql-view")
(declare-function org-string-equal-ignore-case "org-compat")
(declare-function org-ql--add-markers "ext:org-ql")
(declare-function org-ql-view--format-element "ext:org-ql-view")

(defvar imenu-use-markers)
(defvar org-capture-last-stored-marker)
(defvar org-capture-entry)
(defvar org-capture-initial)

;;;; Group and variables

(defgroup org-placeholder nil
  ""
  :group 'org)

(defconst org-placeholder-bookmark-type-property "PLACEHOLDER_TYPE"
  "Org property that specifies the type of the placeholder at the root entry.")

(defconst org-placeholder-allowed-type-values
  '("nested"
    "simple")
  "Possible values for `org-placeholder-bookmark-type-property'.")

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

(defcustom org-placeholder-default-capture-options nil
  "Plist of options for `org-capture' used in the package.

See `org-capture-templates'."
  :type 'sexp)

(defcustom org-placeholder-search-subtrees t
  "Provide completion candidates from subtrees.

If this value is non-nil, `org-placeholder-find-or-create'"
  :type 'boolean)

(defcustom org-placeholder-ignored-group-heading-regexp nil
  "Regular expression for ignored heading groups."
  :type '(choice string null))

(defcustom org-placeholder-show-archived-entries-in-view nil
  "Whether to show archived entries in views.

The default value is nil, and archived entries are not visible in
`org-placeholder-view', as in other commands in this package such
as `org-placeholder-find-or-create'.

If this variable is non-nil, archived entries will be made visible in views."
  :type 'boolean)

(defcustom org-placeholder-highlight-line t
  "Highlight the current line on certain events.

If the value of this variable is non-nil, the current line will
be highlighted when jumping to an item after certain events like
reverting the buffer.

At present, `pulse-momentary-highlight-one-line' is used to
highlight the current line. If the function is unavailable, this
feature doesn't work anyway."
  :type 'boolean)

(defface org-placeholder-subgroup-face
  '((t (:inherit font-lock-doc-face)))
  "Face for subgroups")

(defface org-placeholder-archived-subgroup-face
  '((t (:inherit org-placeholder-subgroup-face :strike-through t)))
  "Face for archived subgroups.")

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

(add-to-list 'org-property-allowed-value-functions
             (defun org-placeholder-allowed-value-functions (property)
               (when (equal property org-placeholder-bookmark-type-property)
                 org-placeholder-allowed-type-values)))

;;;###autoload
(defun org-placeholder-store-bookmark ()
  "Store a bookmark to a view of the current subtree."
  (interactive nil org-mode)
  (unless (org-entry-get nil org-placeholder-bookmark-type-property)
    (org-read-property-value org-placeholder-bookmark-type-property))
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
              (when-let* ((ret (if (not pred)
                                   value
                                 (funcall pred value))))
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

(defcustom org-placeholder-find-hook nil
  "Hook run after visiting a node.

This hook is run after `org-placeholder-find-or-create' visits an
existing node."
  :type 'hook)

;;;###autoload
(defun org-placeholder-find-or-create (&optional bookmark-name initial-input)
  (interactive (list (when current-prefix-arg
                       (org-placeholder-read-bookmark-name "Placeholder: "))))
  (pcase (org-placeholder--read-entry bookmark-name initial-input)
    (`(,input . ,marker)
     (if marker
         (with-current-buffer (marker-buffer marker)
           (pop-to-buffer (current-buffer))
           (goto-char marker)
           (run-hooks 'org-placeholder-find-hook))
       (org-placeholder-capture-input input bookmark-name)))))

(defun org-placeholder--read-entry (&optional bookmark-name initial-input)
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
           (if-let* ((s (gethash candidate node-parent-table)))
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
         (format-heading-from-match ()
           (let ((todo (when org-placeholder-prefix-todo
                         (match-string 2)))
                 (tags (when org-placeholder-suffix-tags
                         (org-get-tags))))
             (concat (if todo
                         (concat todo " ")
                       "")
                     (org-link-display-format
                      (match-string-no-properties 4))
                     (if tags
                         (concat " " (org-make-tag-string tags))
                       ""))))
         (scan-subgroups (root-name root-level target-level bound)
           (let ((group-heading (org-no-properties (org-get-heading t t t t)))
                 (olp-string nil))
             (while (re-search-forward org-complex-heading-regexp bound t)
               (let ((heading-text (match-string 4)))
                 (if (and org-placeholder-ignored-group-heading-regexp
                          heading-text
                          (string-match-p org-placeholder-ignored-group-heading-regexp heading-text))
                     (org-end-of-subtree)
                   (unless (save-match-data (org-in-archived-heading-p))
                     (let ((level (- (match-end 1)
                                     (match-beginning 1)))
                           (marker (copy-marker (match-beginning 0)))
                           (heading (format-heading-from-match)))
                       (cond
                        ((< level target-level)
                         (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                             (scan-subgroups root-name root-level
                                             (+ level 1 (string-to-number str))
                                             (save-excursion (org-end-of-subtree)))
                           (let ((olp (org-get-outline-path t t)))
                             (setq olp-string
                                   (org-no-properties
                                    (org-format-outline-path
                                     (seq-drop olp (1+ root-level))))))))
                        ((= level target-level)
                         (push heading candidates)
                         (puthash heading olp-string node-parent-table)
                         (puthash heading
                                  (if bookmark-name
                                      group-heading
                                    (format "%s: %s" root-name group-heading))
                                  node-group-table)
                         (puthash heading marker org-placeholder-marker-table))
                        (org-placeholder-search-subtrees
                         (let ((this-olp-string (thread-first
                                                  (org-get-outline-path t t)
                                                  (seq-drop (1- target-level))
                                                  (org-format-outline-path)
                                                  (org-no-properties))))
                           (push this-olp-string candidates)
                           (puthash this-olp-string olp-string node-parent-table)
                           (puthash this-olp-string
                                    (if bookmark-name
                                        group-heading
                                      (format "%s: %s" root-name group-heading))
                                    node-group-table)
                           (puthash this-olp-string marker org-placeholder-marker-table)))))))))))
         (run (type root-name root-level end-of-root)
           (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
             (pcase-exhaustive type
               (`nested
                (while (re-search-forward regexp1 end-of-root t)
                  (scan-subgroups root-name root-level
                                  (+ root-level
                                     2
                                     (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                                         (string-to-number str)
                                       0))
                                  (save-excursion (org-end-of-subtree)))))
               (`simple
                (while (re-search-forward regexp1 end-of-root t)
                  (unless (save-match-data (org-in-archived-heading-p))
                    (beginning-of-line)
                    (looking-at org-complex-heading-regexp)
                    (goto-char (match-end 0))
                    (let ((marker (copy-marker (match-beginning 0)))
                          (heading (format-heading-from-match)))
                      (push heading candidates)
                      (puthash heading root-name node-group-table)
                      (puthash heading marker org-placeholder-marker-table)))))))))
      (dolist (root-name root-names)
        (let ((root (org-placeholder-bookmark-root root-name)))
          (cl-etypecase root
            (marker (save-current-buffer
                      (org-with-point-at root
                        (org-with-wide-buffer
                         (org-fold-show-subtree)
                         (run (org-placeholder--subtree-type)
                              root-name
                              (org-outline-level)
                              (save-excursion (org-end-of-subtree)))))))
            (buffer (with-current-buffer root
                      (org-with-wide-buffer
                       (goto-char (point-min))
                       (org-fold-show-all)
                       (run (org-placeholder--buffer-type)
                            root-name
                            0
                            nil)))))))
      (unwind-protect
          (let ((input (or (and initial-input
                                (car (member-ignore-case initial-input candidates)))
                           (completing-read "Find a node: " #'completions nil nil
                                            initial-input))))
            (cons input (gethash input org-placeholder-marker-table)))
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
  (org-placeholder--capture
      (org-placeholder--read-parent (format "Add \"%s\": " input)
                                    bookmark-names)
      input))

(defun org-placeholder--read-parent (prompt &optional bookmark-names)
  (let ((group-map (make-hash-table :test #'equal))
        (marker-map (make-hash-table :test #'equal))
        candidates)
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (gethash candidate group-map)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'category)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred)))
         (add-parent (root-name root-level)
           (let* ((non-heading (and (= root-level 0)
                                    (org-before-first-heading-p)))
                  (candidate (if non-heading
                                 root-name
                               (org-no-properties
                                (org-format-outline-path
                                 (seq-drop (org-get-outline-path t t)
                                           root-level))))))
             (push candidate candidates)
             (if non-heading
                 (puthash candidate "Files" group-map)
               (puthash candidate root-name group-map))
             (puthash candidate (point-marker) marker-map))))
      (dolist (bookmark (or (if (stringp bookmark-names)
                                (list bookmark-names)
                              bookmark-names)
                            (mapcar #'car (org-placeholder--bookmarks))))
        (org-placeholder-map-parents bookmark
          (apply-partially #'add-parent bookmark)))
      (thread-first
        (completing-read prompt
                         #'completions)
        (gethash marker-map)))))

(cl-defun org-placeholder--capture (marker initial &key after-finalize)
  (declare (indent 2))
  (pcase-let*
      ((`(,template ,pre-capture ,post-capture)
        (org-with-point-at marker
          (list (org-entry-get marker "PLACEHOLDER_CAPTURE_TEMPLATE" t)
                (org-entry-get marker "PLACEHOLDER_PRE_CAPTURE" t)
                (org-entry-get marker "PLACEHOLDER_POST_CAPTURE" t))))
       (file (org-with-point-at marker
               (when (org-before-first-heading-p)
                 (buffer-file-name))))
       (org-capture-entry `("" ""
                            entry
                            ,(if file
                                 `(file ,file)
                               `(function
                                 (lambda ()
                                   (org-goto-marker-or-bmk ,marker))))
                            ,(if template
                                 (read template)
                               org-placeholder-default-capture-template)
                            :after-finalize ,after-finalize
                            ,@org-placeholder-default-capture-options))
       (org-capture-initial initial))

    (when pre-capture
      (let ((func (read pre-capture)))
        (condition-case-unless-debug err
            (if (fboundp func)
                (funcall func)
              (error "Unbound function: %s" func))
          (error (message "PLACEHOLDER_PRE_CAPTURE is set to an invalid value: %s" err)))))
    (org-capture)
    (when post-capture
      (let ((func (read post-capture)))
        (condition-case-unless-debug err
            (if (fboundp func)
                (funcall func)
              (error "Unbound function: %s" func))
          (error (message "PLACEHOLDER_POST_CAPTURE is set to an invalid value: %s" err)))))))

(defun org-placeholder-map-parents (bookmark-name fn)
  "Call a function at each parent heading of the items."
  (declare (indent 1))
  (cl-labels
      ((scan-subgroups (root-level target-level bound)
         (while (re-search-forward org-complex-heading-regexp bound t)
           (let ((level (- (match-end 1) (match-beginning 1)))
                 (heading (match-string 4)))
             (if (and org-placeholder-ignored-group-heading-regexp
                      heading
                      (string-match-p org-placeholder-ignored-group-heading-regexp heading))
                 (org-end-of-subtree)
               (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                   (scan-subgroups root-level
                                   (+ level
                                      1
                                      (string-to-number str))
                                   (save-excursion (org-end-of-subtree)))
                 (when (and (= (1- target-level)
                               level)
                            (not (org-in-archived-heading-p)))
                   (save-excursion
                     (beginning-of-line)
                     (funcall fn root-level))))))))
       (f (type root-level bound)
         (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
           (pcase-exhaustive type
             (`nested
              (while (re-search-forward regexp1 bound t)
                (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                    (scan-subgroups root-level
                                    (+ root-level
                                       2 (string-to-number str))
                                    (save-excursion (org-end-of-subtree)))
                  (save-excursion
                    (beginning-of-line)
                    (funcall fn root-level)))))
             (`simple
              (funcall fn root-level))))))
    (let ((root (org-placeholder-bookmark-root bookmark-name)))
      (cl-etypecase root
        (marker (save-current-buffer
                  (org-with-point-at root
                    (org-with-wide-buffer
                     (org-fold-show-subtree)
                     (f (org-placeholder--subtree-type)
                        (org-outline-level)
                        (save-excursion (org-end-of-subtree)))))))
        (buffer (with-current-buffer root
                  (org-with-wide-buffer
                   (org-fold-show-all)
                   (goto-char (point-min))
                   (f (org-placeholder--buffer-type)
                      0
                      nil))))))))

;;;; Sink class for generalized views

(defclass org-placeholder-sink-class nil
  ((source-type :initform nil :type symbol
                :documentation "Type of the source subtree (simple or nested).")
   (items :initform nil :type list
          :documentation "Items in the current section.")))

(cl-defmethod org-placeholder-sink-initialize ((sink org-placeholder-sink-class)
                                               _root)
  "Initialize the sink before feeding data into it.

This method should be called in the output buffer."
  (oset sink items nil))

(cl-defgeneric org-placeholder-sink-start-section (sink
                                                   heading
                                                   &key marker level indirect)
  (error "abstract method"))

(cl-defgeneric org-placeholder-sink-start-subsection (sink
                                                      olp
                                                      &key marker level indirect
                                                      archivedp)
  (error "abstract method"))

(cl-defmethod org-placeholder-sink-flush (sink
                                          &optional archivedp)
  (let ((items (thread-last
                 (oref sink items)
                 (seq-sort (or org-placeholder-sort-function
                               #'ignore)))))
    (org-placeholder-sink-emit-items sink items archivedp)
    (oset sink items nil)))

(cl-defgeneric org-placeholder-sink-emit-items (sink
                                                items
                                                &optional archivedp)
  (error "abstract method"))

(cl-defgeneric org-placeholder-sink-finalize (sink)
  "Finalize the sink and flash the result into the current buffer."
  (error "abstract method"))

(defun org-placeholder-write-to-sink (root sink)
  ;; For `org-ql--add-markers'.
  (require 'org-ql)
  (let ((type (cl-etypecase root
                (marker (org-with-point-at root
                          (org-placeholder--subtree-type)))
                (buffer (with-current-buffer root
                          (org-with-wide-buffer
                           (org-placeholder--buffer-type))))))
        subgroup-archivedp
        first-section)
    (oset sink source-type type)
    (cl-labels
        ((emit (&optional no-empty-line)
           (org-placeholder-sink-flush sink subgroup-archivedp)
           (if first-section
               (setq first-section nil)
             (unless no-empty-line
               (org-placeholder-sink-separator sink))))
         (scan-subgroups (root-level target-level bound)
           (while (re-search-forward org-complex-heading-regexp bound t)
             (when (or org-placeholder-show-archived-entries-in-view
                       (not (org-in-archived-heading-p)))
               (let ((level (org-outline-level)))
                 (cond
                  ((< level target-level)
                   (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                       (scan-subgroups root-level
                                       (+ level 1 (string-to-number str))
                                       (save-excursion (org-end-of-subtree)))
                     ;; Serialize items in the last subgroup.
                     (emit)
                     (setq subgroup-archivedp (and (member org-archive-tag (org-get-tags))
                                                   t))
                     (when-let* ((olp (seq-drop (org-get-outline-path t t)
                                                (1+ root-level))))
                       (org-placeholder-sink-start-subsection
                        sink olp
                        :marker (point-marker)
                        :level level
                        :indirect (not (= level (1- target-level)))
                        :archivedp subgroup-archivedp))))
                  ((= level target-level)
                   (beginning-of-line)
                   (oset sink items
                         (cons (org-ql--add-markers (org-element-at-point))
                               (oref sink items)))
                   (end-of-line))))))
           (emit t))
         (run (root-level end-of-root)
           (let ((regexp1 (org-placeholder--regexp-for-level (1+ root-level))))
             (pcase-exhaustive type
               (`nested
                (while (re-search-forward regexp1 end-of-root t)
                  (catch 'heading
                    (let ((bound (save-excursion (org-end-of-subtree)))
                          (target-level (+ root-level
                                           2
                                           (if-let* ((str (org-entry-get nil "PLACEHOLDER_LEVEL")))
                                               (string-to-number str)
                                             0))))
                      (setq first-section t)
                      (font-lock-ensure (point) (pos-eol))
                      (let ((heading (org-get-heading t t t t)))
                        (when (and org-placeholder-ignored-group-heading-regexp
                                   heading
                                   (string-match-p org-placeholder-ignored-group-heading-regexp
                                                   heading))
                          (org-end-of-subtree)
                          (throw 'heading t))
                        (org-placeholder-sink-start-section
                         sink heading
                         :marker (point-marker)
                         :level (1+ root-level)
                         :indirect (not (= (1+ root-level)
                                           (1- target-level)))))
                      (scan-subgroups root-level target-level bound))
                    (org-placeholder-sink-separator sink))))
               (`simple
                (while (re-search-forward regexp1 end-of-root t)
                  (catch 'heading
                    (beginning-of-line)
                    (when (and org-placeholder-ignored-group-heading-regexp
                               (looking-at org-complex-heading-regexp)
                               (string-match-p org-placeholder-ignored-group-heading-regexp
                                               (match-string 4)))
                      (org-end-of-subtree)
                      (throw 'heading t))
                    (oset sink items
                          (cons (org-ql--add-markers (org-element-at-point))
                                (oref sink items)))
                    (end-of-line)))
                (org-placeholder-sink-flush sink nil))))))
      ;; FIXME: save outline visibility
      (unwind-protect
          (cl-etypecase root
            (marker (save-current-buffer
                      (org-with-point-at root
                        (org-with-wide-buffer
                         (org-fold-show-subtree)
                         (org-placeholder-sink-initialize sink root)
                         (run (org-outline-level)
                              (save-excursion (org-end-of-subtree)))))))
            (buffer (with-current-buffer root
                      (org-with-wide-buffer
                       (goto-char (point-min))
                       (org-fold-show-all)
                       (org-placeholder-sink-initialize sink root)
                       (run 0
                            nil)))))
        (org-placeholder-sink-finalize sink)))))

;;;; Agenda-based view implementation

(defclass org-placeholder-agenda-view (org-placeholder-sink-class)
  ((strings :initform nil :type list)
   (buffer :initarg :buffer :type buffer)))

(cl-defmethod org-placeholder-sink-initialize ((sink org-placeholder-agenda-view) root)
  (oset sink strings nil)
  (cl-etypecase root
    (marker (progn
              (org-fold-show-subtree)
              (font-lock-ensure (point) (pos-eol))
              (let ((title (propertize (org-get-heading t t t t)
                                       'org-marker root)))
                (with-current-buffer (oref sink buffer)
                  (insert title "\n\n")))))
    (buffer (progn
              (goto-char (point-min))
              (org-fold-show-all)
              (let ((title (propertize (or (org-placeholder--find-keyword "title")
                                           (buffer-name))
                                       'org-marker (copy-marker (point-min)))))
                (with-current-buffer (oref sink buffer)
                  (insert title "\n\n")))))))

(cl-defmethod org-placeholder-sink-start-section ((sink org-placeholder-agenda-view)
                                                  heading
                                                  &key marker level indirect)
  (oset sink strings
        (cons (propertize heading
                          'org-marker marker
                          'org-agenda-structural-header t
                          'org-placeholder-outline-level level
                          'org-placeholder-container (if indirect
                                                         'indirect
                                                       t))
              (oref sink strings))))

(cl-defmethod org-placeholder-sink-start-subsection ((sink org-placeholder-agenda-view)
                                                     olp
                                                     &key marker level indirect
                                                     archivedp)
  (oset sink strings
        (cons (thread-first
                (concat
                 " " (propertize (format "(%s)" (org-no-properties
                                                 (org-format-outline-path olp)))
                                 'face
                                 (if archivedp
                                     'org-placeholder-archived-subgroup-face
                                   'org-placeholder-subgroup-face)))
                (propertize 'org-marker marker
                            'org-agenda-structural-header t
                            'org-placeholder-formatted-olp
                            (org-no-properties (org-format-outline-path olp))
                            'org-placeholder-outline-level level
                            'org-placeholder-container
                            (if indirect
                                'indirect
                              t)))
              (oref sink strings))))

(cl-defmethod org-placeholder-sink-emit-items ((sink org-placeholder-agenda-view)
                                               items
                                               &optional archivedp)
  (require 'org-ql-view)
  (with-current-buffer (oref sink buffer)
    (let ((strings (nreverse (oref sink strings))))
      (insert (thread-first
                (append strings
                        (pcase (oref sink source-type)
                          (`simple
                           (mapcar #'org-ql-view--format-element items))
                          (`nested
                           (if archivedp
                               (list (propertize "  (Items in this archived group are not shown)"
                                                 'face 'font-lock-comment-face))
                             (mapcar #'org-ql-view--format-element items)))))
                (string-join "\n"))
              (if (or items strings)
                  "\n"
                ;; Avoid continuous empty lines.
                ""))))
  (oset sink strings nil))

(cl-defmethod org-placeholder-sink-separator ((sink org-placeholder-agenda-view))
  (oset sink strings
        (cons "" (oref sink strings))))

(cl-defmethod org-placeholder-sink-finalize ((_sink org-placeholder-agenda-view)))

(defvar org-placeholder-view-name nil)

(defvar org-placeholder-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'org-placeholder-revert-view)
    (define-key map "c" #'org-placeholder-view-capture)
    (define-key map [remap org-agenda-refile] #'org-placeholder-refile-from-view)
    map))

(define-derived-mode org-placeholder-view-mode org-agenda-mode
  "Org Placeholder View"
  (setq-local imenu-create-index-function #'org-placeholder-view-imenu-function))

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
  (interactive nil org-placeholder-view-mode)
  (org-placeholder--revert-view
   :marker (get-text-property (point) 'org-marker)))

(cl-defun org-placeholder--revert-view (&key marker highlight parent index)
  (let ((inhibit-read-only t)
        (root (org-placeholder-bookmark-root
               (or org-placeholder-view-name
                   (error "org-placeholder-view-name is not set"))))
        (sink (make-instance 'org-placeholder-agenda-view
                             :buffer (current-buffer))))
    (erase-buffer)
    (org-placeholder-write-to-sink root sink)
    (org-agenda-finalize)
    (goto-char (point-min))
    (cond
     (marker
      (when (text-property-search-forward 'org-marker marker #'equal)
        (beginning-of-line)))
     (parent
      (when (text-property-search-forward 'org-marker parent #'equal)
        (when index
          (forward-line index)))))
    (pcase highlight
      (`t
       (org-placeholder--highlight-line))
      (`(id ,id)
       (save-excursion
         (goto-char (point-min))
         (when (text-property-search-forward 'ID id #'equal)
           (org-placeholder--highlight-line)))))
    (message "Refreshed the view")))

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
   (or (org-entry-get nil org-placeholder-bookmark-type-property)
       (user-error "The subtree is missing a required property %s"
                   org-placeholder-bookmark-type-property))))

(defun org-placeholder--buffer-type ()
  (org-placeholder--parse-type
   (save-excursion
     (goto-char (point-min))
     (or (if (looking-at-p org-property-drawer-re)
             (org-entry-get nil org-placeholder-bookmark-type-property)
           (org-placeholder--find-keyword org-placeholder-bookmark-type-property))
         (user-error "The subtree is missing a required property or keyword %s"
                     org-placeholder-bookmark-type-property)))))

(defun org-placeholder--parse-type (string)
  (pcase-exhaustive string
    ("nested" 'nested)
    ("simple" 'simple)))

(defun org-placeholder--highlight-line ()
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line)))

(defun org-placeholder-view-capture ()
  "Add an item to the group at point, or add a subgroup."
  (interactive nil org-placeholder-view-mode)
  (pcase (or (get-char-property (pos-bol) 'org-placeholder-container)
             (save-excursion
               (when-let* ((pos (previous-single-property-change
                                 (point) 'org-placeholder-container)))
                 (goto-char pos)
                 (get-char-property (pos-bol) 'org-placeholder-container))))
    (`t
     (let* ((marker (or (get-char-property (pos-bol) 'org-hd-marker)
                        (get-char-property (pos-bol) 'org-marker)))
            (group-title (org-with-point-at marker
                           (org-no-properties (org-get-heading t t t t))))
            (title (read-from-minibuffer (format "Title of the new entry (in \"%s\"): "
                                                 group-title)
                                         nil
                                         nil nil nil nil 'inherit)))
       (org-placeholder--capture marker title
         :after-finalize `(lambda ()
                            (org-placeholder--maybe-refresh-view ,(buffer-name))
                            (org-placeholder--goto-captured-in-view)))))
    (`indirect
     (let* ((marker (or (get-char-property (pos-bol) 'org-hd-marker)
                        (get-char-property (pos-bol) 'org-marker)))
            (parent-title (when marker
                            (org-with-point-at marker
                              (org-no-properties (org-get-heading t t t t)))))
            (org-capture-initial (read-from-minibuffer
                                  (if parent-title
                                      (format "Title of the new group (in \"%s\"): "
                                              parent-title)
                                    "Title of the new group: ")
                                  nil
                                  nil nil nil nil 'inherit))
            (target (if (and marker
                             (org-with-point-at marker
                               (not (org-before-first-heading-p))))
                        `(function
                          (lambda ()
                            (org-goto-marker-or-bmk ,marker)))
                      `(file ,(buffer-file-name (marker-buffer marker)))))
            (org-capture-entry `("" "" entry ,target
                                 "* %i"
                                 :immediate-finish t
                                 :after-finalize
                                 (lambda ()
                                   (org-placeholder--maybe-refresh-view ,(buffer-name))
                                   (org-placeholder--goto-captured-in-view))
                                 ,@org-placeholder-default-capture-options)))
       (org-capture)))
    ;; If none of the above applies, add a top-level section.
    (`nil
     (let* ((marker (save-restriction
                      (get-char-property (point-min) 'org-marker)))
            (title (read-from-minibuffer "Title of the new group: "
                                         nil
                                         nil nil nil nil 'inherit)))
       (org-placeholder--capture marker title
         :after-finalize `(lambda ()
                            (org-placeholder--maybe-refresh-view ,(buffer-name))
                            (org-placeholder--goto-captured-in-view)))))))

(defun org-placeholder--goto-captured-in-view ()
  (let ((marker (org-with-point-at org-capture-last-stored-marker
                  (when (org-match-line org-complex-heading-regexp)
                    (copy-marker (match-beginning 0))))))
    (goto-char (point-min))
    (when (text-property-search-forward 'org-marker marker #'equal)
      (beginning-of-line)
      (when org-placeholder-highlight-line
        (org-placeholder--highlight-line)))))

(defun org-placeholder--maybe-refresh-view (buffer-name)
  (when-let* ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (org-placeholder-revert-view))))

(defun org-placeholder-refile-from-view (&optional arg)
  (interactive "P" org-placeholder-view-mode)
  (unless org-placeholder-view-name
    (user-error "Run this command from inside `org-placeholder-view-mode'"))
  (when (org-get-at-bol 'org-agenda-structural-header)
    (user-error "Don't run this command on a group"))
  (unless (or (org-get-at-bol 'org-hd-marker)
              (org-get-at-bol 'org-marker))
    (user-error "Not on an entry"))
  (let* ((id (org-get-at-bol 'ID))
         (parent (org-placeholder--read-parent (format "Refile target of \"%s\": "
                                                       (org-link-display-format
                                                        (org-get-at-bol 'raw-value)))
                                               (unless arg
                                                 (list org-placeholder-view-name))))
         (orig-parent (save-excursion
                        (when (text-property-search-backward 'org-placeholder-container)
                          (cons (org-get-at-bol 'org-marker)
                                (cdr (posn-actual-col-row (posn-at-point (point))))))))
         (index (when orig-parent
                  (- (cdr (posn-actual-col-row (posn-at-point (point))))
                     (cdr orig-parent)))))
    (org-agenda-refile nil (org-with-point-at parent
                             (list (org-get-heading t t t t)
                                   (buffer-file-name)
                                   nil
                                   (marker-position parent)))
                       'no-update)
    (org-placeholder--revert-view :highlight (when id `(id ,id))
                                  :parent (car orig-parent)
                                  :index index)))

;;;###autoload
(defun org-placeholder-all-views ()
  "Dispatch all views (experimental).

This command turns on `tab-bar-mode' and display each view in a tab."
  (interactive)
  (tab-bar-mode 1)
  (dolist (bmk (mapcar #'car (org-placeholder--bookmarks)))
    (tab-bar-new-tab)
    (org-placeholder-view bmk)))

(defun org-placeholder-view-imenu-function ()
  (save-excursion
    (let (results
          prev-level
          olp)
      (while (text-property-search-forward 'org-agenda-structural-header)
        (let ((level (get-text-property (pos-bol) 'org-placeholder-outline-level))
              (headline (or (get-text-property (pos-bol) 'org-placeholder-formatted-olp)
                            (org-link-display-format
                             (string-trim (buffer-substring-no-properties (pos-bol) (pos-eol)))))))
          (if (or (not prev-level)
                  (> level prev-level))
              (setq olp (cons headline olp))
            (setq olp (list headline)))
          (push (cons (string-join (reverse olp) "/")
                      (if imenu-use-markers
                          (copy-marker (pos-bol))
                        (pos-bol)))
                results)
          (setq prev-level level)))
      results)))

;;;; Default sorting function

(defvar org-placeholder-keyword-order nil)

(defun org-placeholder-keyword-order (element)
  (when-let* ((kwd (org-element-property :todo-keyword element)))
    (or (cdr (assoc kwd org-placeholder-keyword-order))
        (let ((order (with-current-buffer
                         (marker-buffer (or (org-element-property :org-marker element)
                                            (org-element-property :org-hd-marker element)))
                       (seq-position org-todo-keywords-1 kwd #'equal))))
          (push (cons kwd order)
                org-placeholder-keyword-order)
          order))))

(defun org-placeholder-default-sort (a b)
  (let ((ta (org-placeholder-keyword-order a))
        (tb (org-placeholder-keyword-order b)))
    (or (if (and ta tb)
            (< ta tb)
          tb)
        (unless ta
          (let ((sa (org-element-property :scheduled a))
                (sb (org-element-property :scheduled b)))
            (or (if (and sa sb)
                    (time-less-p (org-timestamp-to-time sb)
                                 (org-timestamp-to-time sa))
                  sb)
                (unless sa
                  (let ((pa (org-element-property :priority a))
                        (pb (org-element-property :priority b)))
                    (if (and pa pb)
                        (> pa pb)
                      pb)))))))))

;;;; Folding support

(defvar-keymap org-placeholder-view-fold-mode-map
  "TAB" #'org-placeholder-toggle-fold)

;;;###autoload
(define-minor-mode org-placeholder-view-fold-mode
  "Minor mode for `org-placeholder-view-mode' adding folding support."
  :lighter " PF"
  (when org-placeholder-view-fold-mode
    (add-to-list 'text-property-default-nonsticky
                 '(org-placeholder-folded . t))))

(defun org-placeholder-toggle-fold (&optional arg)
  "Toggle folding of the current section."
  (interactive "P")
  (pcase arg
    ('(16)
     (org-placeholder-close-all-folds))
    (_ (if (or (get-char-property (point) 'org-placeholder-folded)
               (save-excursion (org-placeholder--folding-start)))
           (org-placeholder-open-fold)
         (org-placeholder-close-fold)))))

(defun org-placeholder--foldable-p (&optional pos)
  "Return non-nil if the section at POS is foldable."
  (get-char-property (or pos (point)) 'org-placeholder-container))

(defun org-placeholder--folding-start ()
  "Return a point where folding starts and move the cursor to the
containing line."
  (when (org-placeholder--foldable-p)
    (beginning-of-line 2))
  (let ((begin (next-single-property-change (point) 'org-placeholder-folded
                                            nil (line-end-position))))
    (if (and begin
             (get-char-property begin 'org-placeholder-folded))
        begin
      (if-let* ((end (previous-single-property-change (point) 'org-placeholder-folded
                                                      nil (line-beginning-position))))
          (and (get-char-property end 'org-placeholder-folded)
               (previous-single-property-change end 'org-placeholder-folded))))))

(defun org-placeholder--foldable-region (&optional pos)
  "Return a bound to which the folding is applied."
  (let* ((pos (or pos (point)))
         (current-level (get-char-property (point) 'org-placeholder-outline-level))
         (change (next-single-property-change pos 'org-placeholder-outline-level)))
    (when change
      (save-excursion
        (goto-char (1+ change))
        (let ((begin (if (looking-at (rx (+ blank)))
                         (match-end 0)
                       (point)))
              (match (text-property-search-forward 'org-placeholder-outline-level
                                                   current-level #'org-placeholder--compare-level)))
          (goto-char (if match
                         (prop-match-beginning match)
                       (point-max)))
          (end-of-line 0)
          (while (bolp)
            (end-of-line 0))
          (when (> (point) change)
            (cons begin (point))))))))

(defun org-placeholder--compare-level (current-level value)
  (and value (<= value current-level)))

(defun org-placeholder-close-fold (&optional state)
  "Fold the current section.

If STATE is nested, the section is closed as nested. When the nested
section is opened, its children will remain closed.

When called non-interactively, this function returns the bounds of the
folded region."
  (interactive nil org-placeholder-view-mode)
  (pcase (org-placeholder--foldable-region)
    (`(,begin . ,end)
     (let ((inhibit-read-only t))
       (add-text-properties begin end (list 'org-placeholder-folded (or state t)
                                            'display "..."))
       (cons begin end)))
    (_ (user-error "Not foldable"))))

(defun org-placeholder-open-fold ()
  "Unfold the current section."
  (interactive nil org-placeholder-view-mode)
  (let ((inhibit-read-only t))
    (if-let* ((state (get-char-property (point) 'org-placeholder-folded)))
        (let ((begin (or (previous-single-property-change (point) 'org-placeholder-folded)
                         (point)))
              (end (next-single-property-change (point) 'org-placeholder-folded)))
          (remove-text-properties begin end '(org-placeholder-folded t display nil))
          (when (eq state 'nested)
            (org-placeholder-close-all-folds begin end)))
      (save-excursion
        (if-let* ((begin (org-placeholder--folding-start)))
            (let ((state (get-char-property begin 'org-placeholder-folded))
                  (end (next-single-property-change begin 'org-placeholder-folded)))
              (remove-text-properties begin end '(org-placeholder-folded t display nil))
              (when (eq state 'nested)
                (org-placeholder-close-all-folds begin end)))
          (user-error "Not folded or foldable"))))))

(defun org-placeholder-close-all-folds (&optional begin end)
  "Close all top-level folds in the current buffer.

If BEGIN and END are points, close folds inside the region.
"
  (interactive nil org-placeholder-view-mode)
  (save-excursion
    (goto-char (or begin (point-min)))
    (catch 'fold-search-abort
      (while-let ((match (text-property-search-forward 'org-placeholder-outline-level)))
        (when (and end (> (prop-match-beginning match) end))
          (throw 'fold-search-abort t))
        (goto-char (prop-match-beginning match))
        ;; There can be non-foldable sections, i.e. empty sections, and scanning
        ;; should continue even with such items.
        (pcase (ignore-errors (org-placeholder-close-fold 'nested))
          (`(,_ . ,end)
           (goto-char end))
          (_
           (end-of-line)))))))

;;;; Embark integration

(defun org-placeholder-item-marker (item)
  "Return the marker to an item.

This is intended for use in `embark-transformer-alist' for
writing your own transformer.

This should be matched against \\='org-placeholder-item category."
  (gethash item org-placeholder-marker-table))

;;;; Exporting

(defclass org-placeholder-json-exporter (org-placeholder-sink-class)
  ((root-filename
    :initarg :root-filename :initform "root.json" :type string)
   (entries-filename
    :initarg :entries-filename :initform "entries.jsonl" :type string)
   (sections-filename
    :initarg :sections-filename :initform "sections.jsonl" :type string)
   (output-filename
    :initarg :output-filename :type string)
   (temporary-directory
    :initform nil :type (or string null))
   (root-olp :initform nil :type list)
   (content-format
    :initarg :content-format :initform gfm :type symbol
    :documentation
    "The format of the content. It is a symbol indicating the format or nil. gfm and org are supported.")))

(defun org-placeholder--file-in-temp-dir (sink slot)
  (file-name-concat (or (oref sink temporary-directory)
                        (error "The temporary directory must not be null"))
                    (slot-value sink slot)))

(defun org-placeholder--append-jsonl-entry (sink slot object)
  (declare (indent 2))
  (with-temp-buffer
    (insert (json-serialize (cl-remove-if #'null object :key #'cdr))
            "\n")
    (let ((filename (org-placeholder--file-in-temp-dir sink slot))
          (coding-system-for-write 'utf-8-unix)
          message-log-max)
      (write-region (point-min) (point-max) filename t))))

(cl-defmethod org-placeholder-sink-initialize ((sink org-placeholder-json-exporter)
                                               root)
  (let ((temp-dir (thread-first
                    (format "mktemp -d -t org-placeholder-export-XXX")
                    (shell-command-to-string)
                    (string-trim))))
    (condition-case _
        (let ((root-data (append (cl-etypecase root
                                   (marker
                                    `((title . ,(org-entry-get nil "ITEM"))
                                      (olp . ,(seq-into (org-get-outline-path t)
                                                        'vector))
                                      (tags . ,(seq-into (org-get-tags)
                                                         'vector))
                                      (states . ,(seq-into org-todo-keywords-1
                                                           'vector))))
                                   (buffer
                                    `((title . ,(or (org-placeholder--find-keyword "title")
                                                    (buffer-name)))
                                      (tags . ,(seq-into org-file-tags
                                                         'vector))
                                      (filename . ,(buffer-file-name))
                                      (states . ,(seq-into org-todo-keywords-1
                                                           'vector)))))
                                 (org-placeholder--json-encode-properties
                                  (org-entry-properties nil 'standard)))))
          (oset sink temporary-directory temp-dir)
          (oset sink root-olp (alist-get 'olp root-data))
          (with-temp-buffer
            (insert (json-serialize root-data))
            (let ((filename (org-placeholder--file-in-temp-dir sink 'root-filename))
                  (coding-system-for-write 'utf-8-unix)
                  message-log-max)
              (write-region (point-min) (point-max) filename))))
      (error (delete-directory temp-dir t)))))

(cl-defmethod org-placeholder-sink-start-section ((sink org-placeholder-json-exporter)
                                                  heading
                                                  &key marker level indirect)
  (org-placeholder--append-jsonl-entry sink 'sections-filename
    (append `((olp . ,(vector heading))
              (indirect . ,(or indirect :false))
              (level . ,level))
            (org-placeholder--exported-section-properties marker))))

(cl-defmethod org-placeholder-sink-start-subsection ((sink org-placeholder-json-exporter)
                                                     olp
                                                     &key marker level indirect
                                                     archivedp)
  (org-placeholder--append-jsonl-entry sink 'sections-filename
    (append `((olp . ,(apply #'vector olp))
              (indirect . ,(or indirect :false))
              (level . ,level)
              (archived . ,(or archivedp :false)))
            (org-placeholder--exported-section-properties marker))))

(defun org-placeholder--exported-section-properties (marker))

(cl-defmethod org-placeholder-sink-separator ((_ org-placeholder-json-exporter))
  ;; noop, as there is no notion of separator in jsonl
  )

(cl-defmethod org-placeholder-sink-emit-items ((sink org-placeholder-json-exporter)
                                               items
                                               &optional archivedp)
  (dolist (element items)
    (let ((raw-title (org-element-property :title element)))
      (org-placeholder--append-jsonl-entry sink 'entries-filename
        (append (save-match-data
                  (if (string-match org-link-bracket-re raw-title)
                      `((title . ,(or (match-string 2 raw-title)
                                      (match-string 1 raw-title)))
                        (href . ,(match-string 1 raw-title)))
                    `((title . ,raw-title))))
                `((olp . ,(thread-first
                            (org-with-point-at (org-element-property :org-hd-marker element)
                              (org-get-outline-path nil t))
                            (seq-drop (length (oref sink root-olp)))
                            (seq-into 'vector)))))))))

(cl-defmethod org-placeholder-sink-finalize ((sink org-placeholder-json-exporter))
  ;; Write to the file
  (unwind-protect
      (let* ((filename (convert-standard-filename
                        (expand-file-name (oref sink output-filename))))
             (buffer-name "*org-placeholder-tar*"))
        (when-let* ((buffer (get-buffer buffer-name)))
          (kill-buffer buffer))
        (with-current-buffer (generate-new-buffer buffer-name)
          (unless (zerop (call-process "tar" nil t nil
                                       "cvzf"
                                       filename
                                       "-C" (oref sink temporary-directory)
                                       (oref sink root-filename)
                                       (oref sink entries-filename)
                                       (oref sink sections-filename)))
            (display-buffer (current-buffer))
            (error "Failed to create the tarball"))))
    (delete-directory (oref sink temporary-directory) t)))

(defun org-placeholder--json-encode-properties (alist)
  (mapcar (lambda (cell)
            (cons (intern (downcase (car cell)))
                  (cdr cell)))
          alist))

;;;###autoload
(defun org-placeholder-export (bookmark filename &optional force)
  "Export BOOKMARK to a tarball of JSON-L files."
  (interactive (list (org-placeholder-read-bookmark-name "Export a view: ")
                     (read-file-name "Export to (*.tar.gz): ")
                     current-prefix-arg))
  (when (and (file-exists-p filename)
             (not force))
    (user-error "File %s already exists" filename))
  (let* ((bookmark-name (if (stringp bookmark)
                            bookmark
                          (car bookmark)))
         (exporter (make-instance 'org-placeholder-json-exporter
                                  :output-filename filename)))
    (org-placeholder-write-to-sink (or (org-placeholder-bookmark-root bookmark-name)
                                       (error "bookmark-name is nil"))
                                   exporter)
    (message "Successfully exported to %s" filename)))

(provide 'org-placeholder)
;;; org-placeholder.el ends here
