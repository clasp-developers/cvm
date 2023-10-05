(in-package #:cvm.machine)
 ;;; Some basic functions for getting at parts of the VM state.

(defgeneric symbol-value (client environment symbol))
(defgeneric (setf symbol-value) (new client environment symbol))
(defgeneric call-with-progv (client environment symbols values thunk))

(defmacro progv (client environment symbols values &body body)
  `(call-with-progv ,client ,environment ,symbols ,values
		    (lambda () ,@body)))
