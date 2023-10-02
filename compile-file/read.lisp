(in-package #:cvm.compile-file)

;;; Hooking up Eclector.
;;; Using Eclector means we don't need to worry about implementation-dependent
;;; expansions for backquote and such, and lets us hook up #. to evaluate in
;;; the correct environment.

(defclass reader-client () ())

;;; We have our own variable here rather than using m:*client* again because
;;; we need this stuff to work regardless of m:*client*. But compile-file also
;;; accepts a :reader-client argument.

(defvar *reader-client* (make-instance 'reader-client))

(defmethod eclector.reader:evaluate-expression ((client reader-client)
                                                expression)
  (cmp:eval expression *environment*))

;;; FIXME: make-structure-instance probably also needs specialization.
;;; The host reader macro will look up the structure name in the host
;;; global environment to get a class to instantiate.
