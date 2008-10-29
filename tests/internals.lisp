(in-package :installer)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :tryil))



;; the start of tests
(define-test file-equalp
  (with-temp-file (temp)
    (with-open-file (outs temp :direction :output)
      (print "test" outs))
    (assert-true (file-equalp temp temp))))


(define-test with-filed-backed-up
  (with-temp-file (temp)
    (with-open-file (outs temp :direction :output)
      (print "test" outs))
    (assert-error 'error (with-filed-backed-up (temp)
                           (print "new" outs)
                           (error "failed")))
    (with-open-file (ins temp)
      (assert-equal "test" (read ins)))))



(define-test copy-file
  (with-temp-file (temp)
    (with-temp-file (temp2)
      (with-open-file (outs temp :direction :output)
        (print "test" outs))
      (copy-file temp temp2)
      (assert-true (file-equalp temp temp2)))))


(run-tests)
