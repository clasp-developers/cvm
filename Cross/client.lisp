(defpackage #:cvm.cross
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm.compile)
                    (#:m #:cvm.machine))
  (:export #:client))

(in-package #:cvm.cross)

(defclass client () ())

(defmethod cmp:run-time-environment ((client client) env)
  (clostrum:evaluation-environment client env))

(defmethod m:link-function ((client client) env fname)
  (clostrum-sys:operator-cell client env fname))

(defmethod m:link-variable ((client client) env name)
  (cons name (clostrum-sys:variable-cell client env name)))
