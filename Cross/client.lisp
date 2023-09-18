(defpackage #:cvm.cross
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm.compile))
  (:export #:client))

(in-package #:cvm.cross)

(defclass client () ())

(defmethod cmp:load-literal-info ((client client) (info cmp:fdefinition-info)
                                  env)
  (clostrum-sys:operator-cell client env
                              (cmp:fdefinition-info-name info)))

(defmethod cmp:load-literal-info ((client client) (info cmp:value-cell-info)
                                  env)
  ;; A CONS where the CAR is the symbol name (for local bindings)
  ;; and the CDR is the global cell.
  (let ((name (cmp:value-cell-info-name info)))
    (cons name (clostrum-sys:variable-cell client env name))))

;;; should be in a clostrum-trucler system

(defmethod trucler:describe-variable ((client client) env name)
  (clostrum:variable-description client env name))
(defmethod trucler:describe-function ((client client) env name)
  (clostrum:function-description client env name))
