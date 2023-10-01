(in-package #:cvm.test.cross)

(defvar *client* (make-instance 'client))

(defun fill-environment (environment)
  (%fill-environment *client* environment))

(defun run (environment) (cvm.test:run environment *client*))
(defun run! (environment) (cvm.test:run! environment *client*))
