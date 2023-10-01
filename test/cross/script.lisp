;;; This script is used by the github automation to run the tests.
;;; You can also use it yourself: just load this file. SBCL will quit
;;; with exit status based on whether everything passed.

(ql:quickload '(:cvm/test/cross :clostrum-basic))

(defpackage #:cvm.test.script
  (:use #:cl))

(in-package #:cvm.test.script)

;;; from ANSI tests
(defun exit (successp &aux (code (if successp 0 1)))
  #+abcl (ext:quit :status code)
  #+acl (excl:exit code :no-unwind t :quiet t)
  #+ccl (ccl:quit code)
  #+cmucl (handler-case (ext:quit nil code)
            ;; Only the most recent versions of cmucl support an exit code.
            ;; If it doesn't, we get a program error (wrong number of args),
            ;; so catch that and just call quit without the arg.
            (program-error ()
              (ext:quit)))
  #+(or clasp clisp ecl) (ext:quit code)
  #+gcl (lisp:quit code)
  #+lispworks (lispworks:quit :status code :ignore-errors-p t)
  #+sbcl (sb-ext:exit :code code))

(defun test ()
  (cvm.cross.vm:initialize-vm 20000)
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (cvm.test.cross:fill-environment rte)
    (exit (cvm.test.cross:run! ce))))

(test)
