(in-package #:cvm.test)

(defvar *environment*)
(defvar *client*)

(defun ceval (form)
  (cvm.compile:eval form *environment* *client*))

(defun ccompile (name definition)
  (declare (ignore name))
  (etypecase definition
    ((cons (eql lambda))
     (cvm.compile:compile definition *environment* *client*))
    ;; this happens in lambda.55,56
    (function definition)))

(defmacro is-true-eval (form)
  `(5am:is-true (ceval ',form)))

(defmacro signals-eval (condition-type form)
  `(5am:signals ,condition-type (ceval ',form)))

(defmacro is-values-eval (form &rest expected)
  `(5am:is (equal '(,@expected)
                  (multiple-value-list (ceval ',form)))))

(defmacro deftest (name form &rest expected)
  `(5am:test ,name
     (is-values-eval ,form ,@expected)))

(defun run (*environment* *client*) (5am:run 'cvm))

(defun run! (*environment* *client*) (5am:run! 'cvm))
