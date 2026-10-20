;;; r4-columns.el --- Columns of the Radio4000 lists  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Internet4000

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A column of a tabulated list is described by an `r4-column': its
;; KEY function returns the value of an object for that column, used
;; to sort the list, and its FORMAT function displays that value.  So
;; numbers and dates sort by value, whatever their display.
;;
;; Each list chooses its visible columns with a user option, such as
;; `r4-channels-columns' and `r4-tracks-columns'.

;;; Code:

(require 'cl-lib)
(require 'r4-models)

(cl-defstruct (r4-column (:constructor r4-column-create)
                         (:copier nil))
  "A column of a list, named NAME, of WIDTH characters.
KEY returns the value of an object for this column, FORMAT returns
the string displaying that value, and RIGHT-ALIGN aligns it right."
  name width key (format #'r4-columns-format-value) right-align)

(defconst r4-columns-max-length 200
  "Maximum length of the text of a cell.
Longer texts, such as descriptions, are cut: a list shows one line.")

(defun r4-columns-format-value (value)
  "Return VALUE as a string on a single line."
  (let ((text (cond ((null value) "")
                    ((stringp value) value)
                    ((listp value) (string-join value " "))
                    (t (format "%s" value)))))
    (r4-one-line (if (> (length text) r4-columns-max-length)
                     (substring text 0 r4-columns-max-length)
                   text))))

(defun r4-columns-less-p (a b)
  "Return non-nil if the column value A sorts before B.
Numbers sort by value, strings ignoring case.  Missing values sort
first."
  (cond ((null b) nil)
        ((null a) t)
        ((and (numberp a) (numberp b)) (< a b))
        ((and (stringp a) (stringp b))
         (let ((order (compare-strings a nil nil b nil nil t)))
           (and (integerp order) (< order 0))))
        (t (r4-columns-less-p (r4-columns-format-value a)
                              (r4-columns-format-value b)))))

(defun r4-columns-valid (columns specs)
  "Return the COLUMNS, a list of symbols, that are defined in SPECS."
  (or (cl-remove-if-not (lambda (column) (assq column specs)) columns)
      (user-error "No valid column in %S" columns)))

(defun r4-columns-list-format (columns specs)
  "Return the `tabulated-list-format' of COLUMNS defined in SPECS.
SPECS is an alist of column symbols and `r4-column'."
  (vconcat
   (cl-loop for (symbol . rest) on columns
            for column = (alist-get symbol specs)
            for key = (r4-column-key column)
            collect `(,(r4-column-name column)
                      ,(if rest (r4-column-width column) 0)
                      ,(lambda (a b)
                         (r4-columns-less-p (funcall key (car a)) (funcall key (car b))))
                      ,@(when (r4-column-right-align column) '(:right-align t))))))

(defun r4-columns-cell (object column specs)
  "Return the text of the COLUMN defined in SPECS for OBJECT."
  (let ((spec (alist-get column specs)))
    (funcall (r4-column-format spec) (funcall (r4-column-key spec) object))))

(defun r4-columns-row (object columns specs)
  "Return the `tabulated-list-entries' row vector of OBJECT.
COLUMNS are the column symbols, defined in SPECS."
  (vconcat (mapcar (lambda (column) (r4-columns-cell object column specs)) columns)))

(defun r4-columns-custom-type (specs)
  "Return the `defcustom' type choosing columns among SPECS."
  `(repeat (choice ,@(mapcar (lambda (spec)
                               (let ((tag (string-trim (r4-column-name (cdr spec)))))
                                 `(const :tag ,(if (string-empty-p tag)
                                                   (capitalize (symbol-name (car spec)))
                                                 tag)
                                         ,(car spec))))
                             specs))))

(provide 'r4-columns)

;;; r4-columns.el ends here
