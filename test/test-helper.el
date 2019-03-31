;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((elisp-index-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elisp-index-dir))

(require 'undercover)
(undercover "elisp-index.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
