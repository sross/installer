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

(define-test download-all-test
  (do-systems (system)
    (when (provider-of system)
      (with-temp-file (system-source)
         (assert-true (download-source system :into system-source))
        (unless (md5sum-of system)
          (warn "MD5Sum missing for ~S~%" system))
        (unless (assert-true (check-md5sum system-source system))
          (format t "~&MD5sum failed for ~S~%" system))))))

(run-tests)



