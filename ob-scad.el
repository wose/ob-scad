;;; ob-template.el --- org-babel functions for template evaluation

;; Copyright (C) Sebastian Woetzel

;; Author: Sebastian Woetzel
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/wose/ob-scad
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating OpenSCAD source code.
;;
;; For information on scad see http://www.openscad.org/
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in scad
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Requirements:

;; OpenSCAD Version 2013.05+ is required for png export.

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:scad
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a scad source block.")

(defun org-babel-expand-body:scad (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel--get-vars params))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
         (value (cdr pair)))
     (setq body
           (replace-regexp-in-string
        (concat "\$" (regexp-quote name))
        (if (stringp value) value (format "%S" value))
        body))))
     vars)
    body))

(defun org-babel-execute:scad (body params)
  "Execute a block of Scad code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assoc :result-params params)))
     (out-file (cdr (assoc :file params)))
     (cmdline (cdr (assoc :cmdline params)))
     (cmd (or (cdr (assoc :cmd params)) "openscad"))
     (in-file (org-babel-temp-file "scad-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:scad body params)))
    (org-babel-eval
     (concat cmd
         " " (org-babel-process-file-name in-file)
         " " cmdline
         " -o " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:scad (session params)
  "Return an error because Scad does not support sessions."
  (error "Scad does not support sessions"))

(provide 'ob-scad)

;;; ob-scad.el ends here
