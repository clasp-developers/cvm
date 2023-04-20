(defpackage #:cvm/cross
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm/compile))
  (:export #:client))

(in-package #:cvm/cross)

(defclass client () ())

(defmethod cmp:load-literal-info ((client client) (info cmp:fdefinition-info)
                                  env)
  ;; env is a compilation environment, but for this we need the
  ;; evaluation environment that env is a child of.
  (clostrum:function-cell client
                          (clostrum:parent env)
                          (cmp:fdefinition-info-name info)))

(defmethod cmp:load-literal-info ((client client) (info cmp:value-cell-info)
                                  env)
  (clostrum:variable-cell client
                          (clostrum:parent env)
                          (cmp:value-cell-info-name info)))

;;; should be in a clostrum-trucler system

(defmethod trucler:describe-variable ((client client) env name)
  (clostrum:variable-description client env name))
(defmethod trucler:describe-function ((client client) env name)
  (clostrum:function-description client env name))
