;;; elisp-index.el --- Generate a list of all symbols in an elisp file  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

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

(defun elisp-index--symbols (buf)
  (let ((read-with-symbol-positions t)
        syms)
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (read buf)
            (setq syms
                  (append syms read-symbol-positions-list)))
        (error
         (if (equal (car err) 'end-of-file)
             syms
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
                (push (cons fun-sym start-pos) funs))))
        (error
         (if (equal (car err) 'end-of-file)
             (nreverse funs)
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  (buffer-file-name) (point) err)))))))

(defun elisp-index (path)
  (let ((buf (find-file-noselect path))
        )
    (list
     :functions (elisp-index--functions buf)
     :symbols (elisp-index--symbols buf))
    ))

(provide 'elisp-index)
;;; elisp-indexer.el ends here
