(defpackage #:cvm.cross
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm.compile))
  (:export #:client))

(in-package #:cvm.cross)

(defclass client () ())

(defmethod cmp:load-literal-info ((client client) (info cmp:fdefinition-info)
                                  env)
  ;; env is a compilation environment, but for this we need the
  ;; evaluation environment that env is a child of.
  (clostrum-sys:operator-cell client
                              (clostrum:evaluation-environment client env)
                              (cmp:fdefinition-info-name info)))

(defmethod cmp:load-literal-info ((client client) (info cmp:value-cell-info)
                                  env)
  (let ((name (cmp:value-cell-info-name info)))
    (cons name
          (clostrum-sys:variable-cell client
                                      (clostrum:evaluation-environment client env)
                                      name))))

(defmethod cmp:load-literal-info ((client client) (info cmp:env-info)
                                  env)
  (clostrum:evaluation-environment client env))
