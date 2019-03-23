#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*-emacs-lisp-*-

;; Based on https://swsnr.de/blog/2014/08/12/emacs-script-pitfalls/
(setq debug-on-error t)

(if argv
    (let ((file-name (car argv)))
      (princ (format "Indexing %S\n" file-name)))
  (message "Usage: <path>")
  (kill-emacs 1))
(kill-emacs 0)
