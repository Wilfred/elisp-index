;;; elisp-index.el --- Generate a list of all symbols in an elisp file  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes
;; Version: 0.1

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0") (f "0.19.0") (ht "2.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'dash)
(require 'f)
(require 'json)
(require 'ht)

(defun elisp-index--symbols (buf)
  (let ((read-with-symbol-positions t)
        syms)
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (read buf)
            (-each read-symbol-positions-list
              (-lambda ((sym . pos))
                (push
                 (ht ("name" (symbol-name sym)) ("position" pos))
                 syms))))
        (error
         (if (equal (car err) 'end-of-file)
             (nreverse syms)
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  (buffer-file-name) (point) err)))))))

(defun elisp-index--function-def-p (form)
  ;; TODO: return a list of all the functions defined in this form.
  (let* ((expanded (macroexpand-all form))
         (head-sym (car-safe expanded))
         (fun-name (car-safe (cdr-safe expanded)))
         (body (car-safe (cdr-safe (cdr-safe expanded)))))
    (when
        (and (eq head-sym 'defalias)
             (eq (car-safe body) 'function))
      (nth 1 fun-name))))

(defun elisp-index--walk-calls (form)
  (cond
   ((not (consp form))
    nil)
   ((eq (car form) 'quote)
    nil)
   (t
    (cons
     (car form)
     (when (consp (cdr form))
       (--mapcat
        (elisp-index--walk-calls it)
        (cdr form)))))))

(defun elisp-index--fun-calls (form src-syms)
  "Return a list of all the functions called in FORM.
Ignore function calls that are only introduced by macros."
  (let ((expanded (macroexpand-all form)))))

(defun elisp-index--functions (buf)
  (let ((read-with-symbol-positions t)
        funs)
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (let* ((form (read buf))
                   (start-pos (scan-sexps (point) -1))
                   (fun-sym (elisp-index--function-def-p form)))
              (when fun-sym
                (push
                 (ht ("name" (symbol-name fun-sym)) ("position" start-pos))
                 funs))))
        (error
         (if (equal (car err) 'end-of-file)
             (nreverse funs)
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  (buffer-file-name) (point) err)))))))

(defun elisp-index--encode (path)
  (let* ((buf (find-file-noselect path))
         (json-encoding-pretty-print t))
    (json-encode
     (ht ("symbols" (elisp-index--symbols buf))
         ("functions" (elisp-index--functions buf))))))

(defun elisp-index--write (path dest-dir)
  "Read the elisp at PATH, and write a copy of the file and JSON
summary to DEST-DIR."
  (let* ((filename (f-filename path))
         (json-filename (format "%s.json" (f-no-ext filename)))
         (buf (find-file-noselect path))
         (src (with-current-buffer buf (buffer-string)))
         )
    (f-write
     (elisp-index--encode path)
     'utf-8
     (f-join dest-dir json-filename))
    (princ (format "Wrote %s\n" (f-join dest-dir json-filename)))
    ))

(provide 'elisp-index)
;;; elisp-index.el ends here
