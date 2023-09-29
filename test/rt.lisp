(in-package #:cvm.test)

(defvar *environment*)
(defvar *client*)

;;; Force true values to T.
(defun notnot (v) (not (not v)))

(defun macroexpand-1 (form &optional env)
  (typecase form
    (symbol
     (let ((info (trucler:describe-variable *client* env form)))
       (if (typep info 'trucler:symbol-macro-description)
           (values (cvm.compile:symbol-macro-expansion info form env) t)
           (values form nil))))
    (cons
     (let* ((head (car form))
            (info (if (symbolp head)
                      (trucler:describe-function *client* env head)
                      nil)))
       (if (typep info 'trucler:macro-description)
           (values (cvm.compile:expand (trucler:expander info) form env) t)
           (values form nil))))
    (t (values form nil))))

(defun macroexpand (form &optional env)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp)
               (macroexpand-1 form env)
             (if expandedp
                 (setf ever-expanded t form expansion)
                 (return (values form ever-expanded))))))

;;; Macro used in tests of environments in system macros
;;; This was inspired by a bug in ACL 8.0 beta where CONSTANTP
;;; was being called in some system macros without the proper
;;; environment argument
(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

;;; Used extensively by the tests as a side effect, but not in
;;; any very complicated ways.
(defmacro incf (place &optional (delta 1) &environment env)
  ;; FIXME: Check for symbol macros.
  (if (symbolp place)
      ;; (this part will be a problem if place is a symbol macro)
      `(setq ,place (+ ,place ,delta))
      (error "Sham INCF not implemented for form: ~s" place)))

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
