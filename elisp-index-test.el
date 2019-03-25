(require 'ert)

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
