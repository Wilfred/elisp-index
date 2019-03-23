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
        (syms))
    (with-current-buffer buf
      (goto-char (point-min))
      (condition-case err
          (while t
            (read buf)
            (setq syms
                  (append syms read-symbol-positions-list))
            )
        (error
         (if (equal (car err) 'end-of-file)
             syms
           ;; Some unexpected error, propagate.
           (error "Unexpected error whilst reading %s position %s: %s"
                  path (point) err)))))))

(defun elisp-index (path)
  (let ((buf (find-file-noselect path))
        )
    (elisp-index--symbols buf)
    ))

(provide 'elisp-index)
;;; elisp-indexer.el ends here
