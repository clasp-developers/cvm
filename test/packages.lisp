(defpackage #:cvm.test.sham
  (:use)
  (:export #:expand-in-current-env #:notnot)
  (:export #:macroexpand-1 #:macroexpand #:eval)
  (:export #:multiple-value-bind #:setf #:incf #:decf #:push
           #:prog1 #:when #:unless #:return #:prog))

(defpackage #:cvm.test
  (:use #:cl)
  (:local-nicknames (#:s #:cvm.test.sham))
  (:export #:run #:run!)
  (:export #:run-native! #:run-cross!)
  ;; We don't define these. They're shadowed so that if you
  ;; use one accidentally in a test, you get an obvious error.
  ;; We could shadow eval but that messes with e.g. eval-when.
  (:shadow #:eval #:compile))
