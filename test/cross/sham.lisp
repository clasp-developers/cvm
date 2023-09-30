(in-package #:cvm.test.cross)

#| ; accounting for what's actually used
compile: (eq let quote)
dynamic-extent: (let quote list length vector flet mapcar labels consp cons car cdr most-positive-fixnum 1- values setq prog1 make-array coerce equal make-string every eql s:setf function)
eval: (let)
eval-when: (eval-when values setq let s:expand-in-current-env)
ignorable: ()
ignore: ()
lambda: (s:notnot 1+ s:incf) ; not the lambda macro, funnily enough
locally: (locally t nil macrolet) ; nil's probably in an earlier file, w/e
optimize: ()
special: ()
symbol-macrolet: (symbol-macrolet)
the: (the)
type: (s:decf)
block: (block return-from return tagbody)
flet: (:x) ; bla bla keywords will need a clostrum intercept
if: (if =)
labels:
let: (+)
multiple-value-call: (multiple-value-call floor)
multiple-value-prog1: (values-list)
progn: (progn go)
return-from: ()
tagbody: ()
|#

(defun define-specials (client environment)
  ;; from figure 3-2
  (loop for op in '(block      let*                  return-from
                    catch      load-time-value       setq
                    eval-when  locally               symbol-macrolet
                    flet       macrolet              tagbody
                    function   multiple-value-call   the
                    go         multiple-value-prog1  throw
                    if         progn                 unwind-protect
                    labels     progv
                    let        quote)
        do (clostrum:make-special-operator client environment op t)))

;;; define functions that can be just copied from the host
(defun define-aliases (client environment)
  (loop for op in '(list length vector consp cons car cdr null not
                    ;; coerce only ok because no test does
                    ;; (coerce foo 'function)
                    1+ 1- + = - values make-array coerce make-string
                    values-list eq eql equal equalp
                    error)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f)))

(defun define-sham-aliases (client environment)
  (loop for op in '(s:notnot)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f))
  (loop for op in '(s:macroexpand-1 s:macroexpand)
        for cl in '(  macroexpand-1   macroexpand)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f
                 (clostrum:fdefinition client environment cl) f)))

;;; functions that can be copied except we ban the env-specific parts
(defun define-stricter-aliases (client environment)
  (loop for op in '(mapc mapcar mapcan mapl maplist mapcon
                    every some notany notevery funcall apply)
        for f = (fdefinition op)
        for g = (let ((f f))
                  (lambda (fun &rest args)
                    (check-type fun function)
                    (apply f fun args)))
        do (setf (clostrum:fdefinition client environment op) g)))

;;; constants copied from the host
(defun define-constants (client environment)
  (loop for c in '(t nil most-positive-fixnum most-negative-fixnum)
        for v = (symbol-value c)
        do (clostrum:make-constant client environment c v)))

(defun define-macros (client environment)
  (loop for mac in '(s:expand-in-current-env setf-1)
        for f = (macro-function mac)
        do (setf (clostrum:macro-function client environment mac) f))
  (loop for mac in '(s:multiple-value-bind
                     s:setf s:incf s:decf
                     s:when s:unless s:prog1)
        for cl in    '(multiple-value-bind
                       setf   incf   decf
                       when   unless   prog1)
        for f = (macro-function mac)
        do (setf (clostrum:macro-function client environment mac) f
                 (clostrum:macro-function client environment cl) f)))

(defun %fill-environment (client environment)
  (define-specials client environment)
  (define-aliases client environment)
  (define-sham-aliases client environment)
  (define-stricter-aliases client environment)
  (define-constants client environment)
  (define-macros client environment))

;;; On top of all that, we need to define a client so that we
;;; can define some methods to automatically bind keywords.
(defclass client (cvm.cross:client) ())

(defmethod clostrum-sys:variable-cell :around ((client client)
                                               environment symbol)
  (let ((cell (call-next-method)))
    (when (keywordp symbol)
      (setf (clostrum-sys:variable-cell-value client cell) symbol))
    cell))

(defmethod clostrum-sys:variable-status :around ((client client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))
