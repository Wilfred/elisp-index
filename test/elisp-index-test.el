(require 'ert)
(require 'elisp-index)

(ert-deftest elisp-index--walk-calls--quote ()
  (should
   (null
    (elisp-index--walk-calls
     ''foo))))

(ert-deftest elisp-index--walk-calls--calls ()
  (should
   (equal
    (elisp-index--walk-calls
     '(foo))
    (list 'foo)))
  (should
   (equal
    (elisp-index--walk-calls
     '(foo bar baz))
    (list 'foo))))

(ert-deftest elisp-index--walk-calls--nested-calls ()
  (should
   (equal
    (elisp-index--walk-calls
     '(foo (bar)))
    (list 'foo 'bar))))

(ert-deftest elisp-index--walk-calls--let ()
  (should
   (equal
    (elisp-index--walk-calls
     '(let (sym1 (sym2) (sym3 (foo))) (bar sym1)))
    (list 'foo 'bar))))

(ert-deftest elisp-index--walk-calls--defun ()
  (should
   (equal
    (elisp-index--walk-calls
     (macroexpand-all
      '(defun foo (x) (bar x))))
    (list 'bar))))

(ert-deftest elisp-index--walk-calls--while ()
  (should
   (equal
    (elisp-index--walk-calls
     '(while t (foo)))
    (list 'foo))))

(ert-deftest elisp-index--walk-calls--if ()
  (should
   (equal
    (elisp-index--walk-calls
     '(if t (foo)))
    (list 'foo))))

(ert-deftest elisp-index--walk-calls--condition-case ()
  (should
   (equal
    (elisp-index--walk-calls
     '(condition-case err
          (foo)
        (error (bar))
        (other-signal some-var)))
    (list 'foo 'bar))))

(ert-deftest elisp-index--walk-calls--cond ()
  (should
   (equal
    (elisp-index--walk-calls
     '(cond
       ((foo) (bar))
       (t (baz) (biz))))
    (list 'foo 'bar 'baz 'biz))))

(ert-deftest elisp-index--definitions-fun ()
  (should
   (equal
    (elisp-index--definitions-in
     (macroexpand-all '(defun foo () 1)))
    (list 'foo))))

(ert-deftest elisp-index--definitions-major-mode ()
  (should
   (equal
    (elisp-index--definitions-in
     (macroexpand-all
      '(define-derived-mode foo-mode prog-mode "Foo")))
    (list 'foo-mode))))

(ert-deftest elisp-index--called-macros-in ()
  (should
   (equal
    (elisp-index--called-macros-in
     '(defun foo () (when t (bar))))
    (list 'defun 'when))))

(ert-deftest elisp-index--called-macros-in-params ()
  "We shouldn't look inside parameter lists for macros."
  (should
   (equal
    (elisp-index--called-macros-in
     '(defun foo (when) 42))
    (list 'defun))))

(defmacro elisp-index--when (bool &rest body)
  `(when ,bool ,@body))

(ert-deftest elisp-index--called-macros-in-other-macros ()
  "Handle macros that expand to other macros."
  (should
   (equal
    (elisp-index--called-macros-in
     '(elisp-index--when t (foo) (bar)))
    (list 'elisp-index--when 'when))))

(ert-deftest elisp-index--called-macros-in-sublist ()
  "We should handle macros that are nested inside function calls"
  (should
   (equal
    (elisp-index--called-macros-in
     '(foo (when t (bar))))
    (list 'when))))

(ert-deftest elisp-index--called-macros-quote ()
  (should
   (equal
    (elisp-index--called-macros-in
     ''(defun foo () (when t (bar))))
    nil)))

(ert-deftest elisp-index--called-macros-rx ()
  "Ensure we handle macros that expand to non-lists."
  (should
   (equal
    (elisp-index--called-macros-in
     '(rx "foo"))
    (list 'rx))))

(ert-deftest elisp-index--called-macros-backquote ()
  (should
   (equal
    (elisp-index--called-macros-in
     '`(foo ,(concat "foo" "bar")))
    (list '\`))))

(ert-deftest elisp-index--called-macros-dotted-list ()
  "Regression test for dotted lists."
  (should
   (equal
    (elisp-index--called-macros-in
     '(foo (\` ((\, "bar") ((\, "exit") . baz)))))
    (list '\`))))
