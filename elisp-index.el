;;; elisp-index.el --- Generate a list of all symbols in an elisp file  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes
;; Version: 0.1

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0") (f "0.19.0") (ht "2.2") (s "1.11.0"))

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

(defun elisp-index--mapcat-forms (buf callback)
  "Read every form in BUF and execute CALLBACK.

CALLBACK receives the form, and the start/end of the region that
contains the form. `read-symbol-positions-list' is also set
during the execution of CALLBACK.

CALLBACK should return a list, and the results are concatenated
into a single list."
  (let ((result))
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (let* ((read-with-symbol-positions t)
                   ;; Note that this is not the start of the form,
                   ;; because point may be before the form if there is
                   ;; preceding whitespace/comments.
                   (start-pos (point))
                   (form (read buf))
                   (end-pos (point)))
              (push
               (funcall callback form start-pos end-pos)
               result)))
        (error
         (unless (equal (car err) 'end-of-file)
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  (buffer-file-name) (point) err))))
      (apply #'append (nreverse result)))))

;; TODO: this is confused by
;; (condition-case nil nil (error (error "F")))
;; and thinks there are two calls to error.
(defun elisp-index--fn-calls-positions (form region-start _region-end)
  "Return all the function calls in FORM: symbol names and positions.

Function symbols that do not occur before macro expansion are ignored."
  (let ((fun-syms
         (elisp-index--walk-calls (macroexpand-all form)))
        syms)
    ;; For every symbol that occurred in the source code:
    (-each read-symbol-positions-list
      (-lambda ((sym . offset))
        ;; If it occurred as a function call.
        (when (memq sym fun-syms)
          (let* ((start-pos (+ region-start offset))
                 (end-pos (+ start-pos (length (symbol-name sym)))))
            (push
             (ht
              ("name" (symbol-name sym))
              ;; Subtract 1 because emacs positions are 1-indexed.
              ("start" (1- start-pos))
              ("end" (1- end-pos)))
             syms)))))
    (nreverse syms)))

(defun elisp-index--calls (buf)
  (let ((fn-calls
         (elisp-index--mapcat-forms
          buf
          #'elisp-index--fn-calls-positions))
        (mac-calls
         (elisp-index--mapcat-forms
          buf
          #'elisp-index--mac-calls-positions)))
    (--each fn-calls
      (ht-set it "namespace" "function"))
    (--each mac-calls
      (ht-set it "namespace" "macro"))
    (--sort
     (< (ht-get it "start")
        (ht-get other "start"))
     (append fn-calls mac-calls))))

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

(defun elisp-index--alist-val-count (alist val)
  "Return the count of the number of pairs in LIST
whose cdr is VAL."
  (let ((count 0))
    (--each alist
      (when (eq (cdr it) val)
        (setq count (1+ count))))
    count))

(defun elisp-index--uniquify-syms (form syms &optional mapping)
  "Replace any symbol in FORM with a unique symbol, if that symbool
occurs in SYMS.

Return a pair (REPLACED . MAPPING), where MAPPING translates
unique symbols to the original."
  (cond
   ((and (symbolp form)
         (memq form syms))
    (let ((new-sym
           (make-symbol
            (format "%s-%d" form
                    (elisp-index--alist-val-count mapping form)))))
      (cons
       new-sym
       (cons (cons new-sym form)
             mapping))))
   ((not (consp form))
    (cons form mapping))
   (t
    (-let* (((head . mapping)
             (elisp-index--uniquify-syms
              (car form)
              syms
              mapping))
            ((rest . mapping)
             (elisp-index--uniquify-syms
              (cdr form)
              syms
              mapping)))
      (cons
       (cons head rest)
       mapping)))))

(defun elisp-index--walk-calls (form)
  (cond
   ((not (consp form))
    nil)
   ((eq (car form) 'quote)
    nil)
   ((or (eq (car form) 'while)
        (eq (car form) 'if))
    (elisp-index--walk-calls-body (cdr form)))
   ((eq (car form) 'function)
    (if (symbolp (cadr form))
        ;; For #'foo, assume it's a call.
        (cdr form)
      ;; For #'(lambda ...), just proceed.
      (elisp-index--walk-calls (cadr form))))
   ((eq (car form) 'lambda)
    (elisp-index--walk-calls-body (cddr form)))
   ((eq (car form) 'defalias)
    (elisp-index--walk-calls (nth 2 form)))

   ((eq (car form) 'cond)
    (let* ((clauses (cdr form)))
      (--mapcat
       (elisp-index--walk-calls-body it)
       clauses)))

   ((eq (car form) 'condition-case)
    (let* ((body (nth 2 form))
           (clauses (cdddr form)))
      (append
       (elisp-index--walk-calls body)
       (--mapcat
        (elisp-index--walk-calls-body (cdr it))
        clauses))))

   ((or (eq (car form) 'let)
        (eq (car form) 'let*))
    (let ((head (nth 1 form))
          (body (cddr form)))
      (append
       (--mapcat
        (when (consp it)
          (elisp-index--walk-calls-body (cdr it)))
        head)
       (elisp-index--walk-calls-body body))))
   
   (t
    (cons
     (car form)
     (when (consp (cdr form))
       (elisp-index--walk-calls-body (cdr form)))))))

(defun elisp-index--walk-calls-body (body)
  (--mapcat (elisp-index--walk-calls it) body))

(defun elisp-index--mac-calls-positions (form region-start _region-end)
  "Return all the function calls in FORM: symbol names and positions.

Not namespace aware, so will erroneously report variables as
macro calls."
  (let ((macro-syms (elisp-index--called-macros-in form))
        syms)
    ;; For every symbol that occurred in the source code:
    (-each read-symbol-positions-list
      (-lambda ((sym . offset))
        ;; If it occurred as a macro call.
        (when (memq sym macro-syms)
          (let* ((start-pos (+ region-start offset))
                 (end-pos (+ start-pos (length (symbol-name sym)))))
            (push
             (ht
              ("name" (symbol-name sym))
              ;; Subtract 1 because emacs positions are 1-indexed.
              ("start" (1- start-pos))
              ("end" (1- end-pos)))
             syms)))))
    (nreverse syms)))

(defun elisp-index--called-macros-in (form)
  "Return a list of all the macros used in FORM.

If a macro expands to another macro, include both in the result."
  ;; Approximate the macro expansion that Emacs itself does. Emacs'
  ;; implementation works by `macroexp--expand-all' and
  ;; `macroexp--all-forms' calling each other.
  (when (consp form)
    ;; Perform one step of macro expansion of the car of this form.
    (let* ((expanded (macroexpand-1 form))
           ;; If we aren't quoted, expand any macro calls in the body.
           (rest
            (when (consp expanded)
              (cond
               ;; (lambda ...) expands to (function (lambda () ...)).
               ;; Expand the lambda body.
               ((and (eq (car expanded) 'function)
                     (eq (car-safe (car-safe (cdr expanded))) 'lambda))
                (let* ((lambda-form (nth 1 expanded))
                       (lambda-body (cddr lambda-form)))
                  (--mapcat (elisp-index--called-macros-in it) lambda-body)))
               ;; Otherwise, don't expand the body in (quote ...) or (function ...).
               ((memq (car expanded) (list 'quote 'function))
                nil)
               ((not (equal form expanded))
                ;; Keep expanding this form, in case the macro expands
                ;; to other macros.
                (elisp-index--called-macros-in expanded))
               (t
                ;; The car wasn't a macro, so recurse on the cdr. The
                ;; cdr should always be a proper list.
                (--mapcat (elisp-index--called-macros-in it) (cdr expanded)))))))
      (if (equal form expanded)
          ;; If we didn't expand anything at the top-level, the car
          ;; wasn't a macro.
          rest
        ;; The car was a macro, so add it to the list of found macros.
        (cons (car form) rest)))))

(defun elisp-index--definitions-in (form)
  "Return a list of all the functions defined in FORM.
Assumes FORM has been fully macro expanded."
  (cond
   ((not (consp form))
    nil)
   ((eq (car form) 'quote)
    nil)
   ((eq (car form) 'defalias)
    (let* ((quoted-sym (nth 1 form))
           (def (nth 2 form))
           (sym (nth 1 quoted-sym)))
      (cons sym (elisp-index--definitions-in def))))
   ((consp (cdr form))
    (--mapcat (elisp-index--definitions-in it) (cdr form)))))

(defun elisp-index--functions (buf)
  "Return a hashmap of all the functions defined in BUF."
  (let ((read-with-symbol-positions t)
        funs)
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (let* ((form (read buf))
                   (end-pos (point))
                   (start-pos (scan-sexps (point) -1))
                   )
              (--each (elisp-index--definitions-in (macroexpand-all form))
                (push
                 (ht ("name" (symbol-name it))
                     ("start" (1- start-pos))
                     ("end" (1- end-pos)))
                 funs))))
        (error
         (if (equal (car err) 'end-of-file)
             (nreverse funs)
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  (buffer-file-name) (point) err)))))))

(defun elisp-index--encode (path)
  (let* ((buf (find-file-noselect path))
         (json-encoding-pretty-print t)
         (src (with-current-buffer buf (buffer-string))))
    (json-encode
     (ht
      ("name" (f-filename path))
      ("source" src)
      ("functions" (elisp-index--functions buf))
      ("calls" (elisp-index--calls buf))))))

(defun elisp-index--write (path dest-dir)
  "Read the elisp at PATH, and write a copy of the file and JSON
summary to DEST-DIR."
  (let* ((filename (f-filename path))
         (json-filename
          (format "%s.json" (s-chop-suffixes '(".gz" ".el") filename))))
    (f-write
     (elisp-index--encode path)
     'utf-8
     (f-join dest-dir json-filename))
    (princ (format "Wrote %s\n" (f-join dest-dir json-filename)))
    ))

(provide 'elisp-index)
;;; elisp-index.el ends here
