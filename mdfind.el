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

(defun md-compile-query (s)
  (cond
   ((null s) "")
   ((stringp s) s)
   ((numberp s) (format "%s" s))
   ((symbolp s)
    (or (cdr (assq s md-vars))
        (symbol-name s)))
   ((listp s)
    (case (first s)
      (range
       (apply 'format "InRange(%s, %s, %s)"
              (mapcar 'md-compile-query (cdr s))))
      (in
       (md-compile-query
        (cons 'or
              (mapcar (lambda (x)
                        (list 'eq (second s) x))
                      (cddr s)))))
      ((s str string)
       (format "%S%s"
               (second s)
               (or (third s) "")))
      
      (t
       (let ((op (concat " "
                         (or (cdr (assq (car s) md-ops))
                             (symbol-name (car s)))
                         " ")))
         (concat "("
                 (mapconcat 'md-compile-query (cdr s) op)
                 ")")))))))


(defun md-find (query &rest dirs)
  "Run a Spotlight search for QUERY and return a list of
resulting files."
  (let ((dir-flag
         (if dirs
             (concat "-onlyin "
                     (mapconcat 'shell-quote-argument dirs " -onlyin ")) ""))
        (query-str (md-compile-query query)))
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
   (format "%S" (md-compile-query query))
   dirs))


(provide 'mdfind)
;;; mdfind.el ends here.
