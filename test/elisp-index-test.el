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

(ert-deftest elisp-index--called-macros ()
  (should
   (equal
    (elisp-index--called-macros
     '(defun foo () (when t (bar))))
    (list 'defun 'when))))

(ert-deftest elisp-index--called-macros-quote ()
  (should
   (equal
    (elisp-index--called-macros
     ''(defun foo () (when t (bar))))
    nil)))
