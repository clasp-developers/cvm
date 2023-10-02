(in-package #:cvm.test)

(defun run-native! (native-client)
  (cvm.test:run! nil native-client))

(defun run-cross! ()
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (cvm.test.cross:fill-environment rte)
    (cvm.test.cross:run! ce)))
