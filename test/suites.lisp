(in-package #:cvm.test)

;;; We can do more tests with the cross version.
(5am:def-suite cvm-cross)

(5am:def-suite cvm :in cvm-cross)

(5am:def-suite ansi :in cvm)
(5am:def-suite eval-and-compile :in ansi)
(5am:def-suite data-and-control-flow :in ansi)

(5am:def-suite fasl :in cvm-cross)
