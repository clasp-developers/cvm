(in-package #:cvm.compile)

(define-condition unknown-reference (condition)
  ((%name :initarg :name :reader name)))

;;; Signal an UNKNOWN-REFERENCE condition in such a way that if we're in a
;;; compilation unit, it will be suppressed until the unit ends.
;;; Like WARN, returns NIL.
(defun warn-unknown (datum &rest arguments)
  (restart-case (apply #'warn datum arguments) (continue ())))

(define-condition unknown-variable (unknown-reference warning)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown variable ~s: treating as special"
                     (name condition)))))

(define-condition unknown-function (unknown-reference style-warning)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown operator ~s: treating as global function"
                     (name condition)))))

;;; Parent class for conditions that can be SIGNALed at compile time to
;;; indicate that a previously unknown name is now known.
(define-condition unknown-reference-resolution (condition)
  ((%name :initarg :name :reader name)))

;;; Iff RESOLUTION is a resolution of REFERENCE, returns true.
;;; May have other side effects (see ASSUMED-FUNCTION-NOW-MACRO below).
(defgeneric resolve-reference (resolution reference)
  (:method (resolution reference)
    (declare (ignore resolution reference))
    nil))

;;; Can be SIGNALed by e.g. DEFUN's expansion at compile time.
(define-condition resolve-function (unknown-reference-resolution) ())
(defmethod resolve-reference ((r1 resolve-function) (r2 unknown-function))
  (equal (name r1) (name r2)))

;;; Warning signaled when a macro has been newly defined, and the compiler
;;; previously saw macro forms that it assumed were function calls.
(define-condition assumed-function-now-macro (warning)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "Uses of newly noted macro ~s were previously assumed to be function calls"
                     (name condition)))))

;;; Can be SIGNALed by e.g. DEFMACRO's expansion at compile time.
(define-condition resolve-macro (unknown-reference-resolution) ())
(defmethod resolve-reference ((r1 resolve-macro) (r2 unknown-function))
  (when (equal (name r1) (name r2))
    (warn 'assumed-function-now-macro :name (name r1))
    t))
