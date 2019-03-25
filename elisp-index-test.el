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
