;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 21 10:43:15 2002
;;;; Contains: Tests of EVAL

(in-package #:cvm.test)

(5am:def-suite eval :in eval-and-compile)
(5am:in-suite eval)

(deftest eval.1
    1
  1)

#+(or)
(deftest eval.2
  (loop for x being the symbols of "KEYWORD"
        always (eq (eval x) x))
  t)

(5am:test eval.3
  (5am:is-true (let ((s "abcd"))
                 (eql (eval s) s))))

#+(or)
(deftest eval.4
  (eval '(car '(a . b)))
  a)

(deftest eval.5
  (let ((x 0)) x)
  0)

(5am:test eval.6
  (5am:is (eql 1 (funcall #'eval 1))))

#+(or)
(deftest eval.order.1
  (let ((i 0))
    (values (eval (progn (incf i) 10)) i))
  10 1)

;;; Error cases

(5am:test eval.error.1
  (5am:signals program-error (eval)))

#+(or)
(deftest eval.error.2
  (signals-error (eval nil nil) program-error)
  t)

(5am:test eval.error.3
  (5am:signals undefined-function (eval (list (gensym)))))

(5am:test eval.error.4
  (5am:signals unbound-variable (eval (gensym))))
