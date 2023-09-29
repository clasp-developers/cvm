(defpackage #:cvm.test
  (:use #:cl)
  (:shadow #:eval #:compile #:macroexpand-1 #:macroexpand #:incf)
  (:export #:run #:run!))
