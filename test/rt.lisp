(in-package #:cvm.test)

;;; Force true values to T.
(defun notnot (v) (not (not v)))

;;; Macro used in tests of environments in system macros
;;; This was inspired by a bug in ACL 8.0 beta where CONSTANTP
;;; was being called in some system macros without the proper
;;; environment argument
(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(defvar *environment*)
(defvar *client*)

(defun eval (form)
  (cvm.compile:eval form *environment* *client*))

(defun compile (name lambda-expression)
  (declare (ignore name))
  (cvm.compile:compile lambda-expression *environment* *client*))

(defmacro is-true-eval (form)
  `(5am:is-true (eval ',form)))

(defmacro signals-eval (condition-type form)
  `(5am:signals ,condition-type (eval ',form)))

(defmacro deftest (name form &rest expected)
  `(5am:test ,name
     (5am:is (equal '(,@expected) (multiple-value-list (eval ',form))))))

(defun run (*environment* *client*) (5am:run 'cvm))

(defun run! (*environment* *client*) (5am:run! 'cvm))
