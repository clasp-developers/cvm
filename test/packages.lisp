(defpackage #:cvm.test.sham
  (:use)
  (:export #:expand-in-current-env #:notnot)
  (:export #:macroexpand-1 #:macroexpand)
  (:export #:multiple-value-bind #:setf #:incf #:decf))

(defpackage #:cvm.test
  (:use #:cl)
  (:local-nicknames (#:s #:cvm.test.sham))
  (:export #:run #:run!)
  ;; We don't define these. They're shadowed so that if you
  ;; use one accidentally in a test, you get an obvious error.
  ;; We could shadow eval but that messes with e.g. eval-when.
  (:shadow #:eval #:compile))
