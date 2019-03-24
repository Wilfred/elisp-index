#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*-emacs-lisp-*-

;; Based on https://swsnr.de/blog/2014/08/12/emacs-script-pitfalls/
(setq debug-on-error t)

(let* ((current-dir (expand-file-name default-directory))
       (cask-elpa-dir
        (concat current-dir ".cask/" emacs-version "/elpa/")))
  ;; Add the elisp-index directory to load-path, so we can execute it.
  (add-to-list 'load-path current-dir)
  ;; Also add all all the dependencies to load-path, so we can use
  ;; external packages.
  (dolist (dir (directory-files cask-elpa-dir))
    (add-to-list 'load-path (concat cask-elpa-dir dir)))

  (load "elisp-index.el" nil t t))



(if argv
    (let ((path (car argv)))
      (princ (format "Indexing %s\n" path))
      (elisp-index--write path (f-join default-directory "elisp-index/src/data")))
  (message "Usage: <PATH>")
  (kill-emacs 1))
(kill-emacs 0)
