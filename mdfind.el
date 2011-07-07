;;; mdfind.el --- A basic interface for Apple Spotlight

;; Copyright (c) 2011 Mike Spindel <deactivated@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'mdfind-dired nil t)

(defvar md-vars
  '((cdate . "kMDItemContentCreationDate")
    (mdate . "kMDItemContentModificationDate")
    (title . "kMDItemTitle")
    (name  . "kMDItemFSName")
    (type  . "kMDItemContentType")
    (gid   . "kMDItemFSOwnerGroupID")
    (uid   . "kMDItemFSOwnerUserID"))
  "A list of abbreviations for Spotlight metadata fields.")

(defvar md-ops
  '((or  . "||")
    (and . "&&")
    (eq  . "=="))
  "A list of abbreviations for Spotlight operators.")

(defun md-queryp (s)
  (and (listp s) (eq (car s) 'q)))

(defun md-val (s)
  (cdr s))

(defun md-ret (s)
  (cons 'q s))

(defun md-compiled-val (s)
  (md-val (md-compile-query s)))

(defun md-compile-query (s)
  "Compile an expression S in the mdfind.el query language to a
Spotlight query."
  (cond
   ((md-queryp s) s)
   ((null s)
    (md-ret ""))
   ((stringp s)
    (md-ret (format "%S" s)))
   ((numberp s)
    (md-ret (format "%s" s)))
   ((symbolp s)
    (md-ret (or (cdr (assq s md-vars))
                (symbol-name s))))
   ((and (consp s) (not (listp (cdr s))))
    (md-ret (format "%S%s" (car s) (cdr s))))

   ((listp s)
    (case (first s)
      (range
       (md-ret (apply 'format "InRange(%s, %s, %s)"
                      (mapcar 'md-compiled-val (cdr s)))))
      (in
       (md-compile-query
        (cons 'or
              (mapcar (lambda (x)
                        (list 'eq (second s) x))
                      (cddr s)))))
      (t
       (let ((op (concat " "
                         (or (cdr (assq (car s) md-ops))
                             (symbol-name (car s)))
                         " ")))
         (md-ret (concat "("
                         (mapconcat 'md-compiled-val (cdr s) op)
                         ")"))))))))

(defun md-find (query &rest dirs)
  "Run a Spotlight search for QUERY and return a list of
resulting files."
  (let ((dir-flag
         (if dirs
             (concat "-onlyin "
                     (mapconcat 'shell-quote-argument dirs " -onlyin ")) ""))
        (query-str (md-compiled-val query)))
    (message "%s" query-str)
    (with-temp-buffer
      (let ((coding-system-for-read (or file-name-coding-system 'utf-8))
            (coding-system-for-write 'utf-8))
        (shell-command
         (concat
          "mdfind "
          dir-flag " "
          (shell-quote-argument query-str))
         (current-buffer)))
      (split-string
       (buffer-string) "\n" t))))

(defun md-dired (query &rest dirs)
  "Run QUERY and open results in a dired buffer.  Requires the
package mdfind-dired."
  (apply 'mdfind-dired
   (format "%S" (md-compiled-val query))
   dirs))

(provide 'mdfind)
;;; mdfind.el ends here.
