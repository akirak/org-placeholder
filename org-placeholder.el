;;; org-placeholder.el --- Placeholders for Org -*- lexical-binding: t -*-

(require 'bookmark)
(require 'org)

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
  (require 'org-super-agenda)
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
                  (cl-flet*
                      ((sort-element-pred (a b)
                         t)
                       (emit (&optional no-empty-line)
                         (setq strings (append (thread-last
                                                 items
                                                 (seq-sort #'sort-element-pred)
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
                                                  'marker (point-marker)))
                                    strings)))
                           ((= level target-level)
                            (beginning-of-line)
                            (push (org-ql--add-markers (org-element-headline-parser))
                                  items)
                            (end-of-line))))))
                    (emit t)))
                (push "" strings))))))))
    (insert (string-join (nreverse strings) "\n"))))

(provide 'org-placeholder)
;;; org-placeholder.el ends here
