
(defun tricky-condition-case ()
  (condition-case nil
      nil
    ;; This isn't a call.
    (error
     ;; This is.
     (error ""))))

(defun tricky-clashing-namespace ()
  ;; This isn't a call.
  (let ((list '(1 2 3)))
    ;; This is.
    (list list)))

(defmacro tricky-ignore (&rest _)
  nil)

(defun tricky-macro-ignores-argument ()
  ;; This isn't a call.
  (tricky-ignore (foo))
  ;; This is.
  (foo))

(defmacro tricky-expands-to-foo (&rest _)
  `(foo))

(defun tricky-macro-inserts-call ()
  ;; This has a call to foo, but the 'foo as macro argument is
  ;; irrelevant.
  (tricky-expand-to-foo 'bar 'foo))

