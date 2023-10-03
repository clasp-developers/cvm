(in-package #:cvm.test)

(5am:def-suite syntax-errors :in compiler-conditions)
(5am:in-suite syntax-errors)

;;; Test that invalid code is reported by the compiler.
;;; These tests are not ANSI-required, because in ANSI CL the effects of running
;;; invalid code are often undefined. But we want to define them as failures.

(defun compilation-fails (form)
  ;; Allow either ccompile signaling an error, or returning a failure indication.
  (nth-value 2 (handler-case (ccompile nil `(lambda () ,form))
                 (error () (values nil t t)))))

(5am:test special-form-syntax
  (flet ((f (form)
           (5am:is-true (compilation-fails form)
                        "Compilation of ~s did not fail" form)))
    (mapc #'f
          '((block) (block . 2) (block nil . 3) (block 4)
            (catch) (catch . 5) (catch nil . 6)
            (eval-when) (eval-when t) (eval-when . t)
            (eval-when (:execute) . t) (eval-when (t))
            (flet) (flet . t) (flet t) (flet () . t) (flet (t))
            (flet ((f))) (flet ((f . t))) (flet ((f ()) . t)) (flet ((4 ())))
            (function) (function a b) (function . c) (function 4)
            (function (lambda)) (function (lambda . t)) (function (lambda t))
            (go) (go . t) (go (t)) (tagbody a (go a b))
            (if) (if t) (if . t) (if a . b) (if a b . c) (if a b c d)
            (labels) (labels . t) (labels t) (labels () . t) (labels (t))
            (labels ((f))) (labels ((f . t))) (labels ((f ()) . t))
            (labels ((4 ())))
            (let) (let . t) (let t) (let () . t) (let (4)) (let ((4 t)))
            (let ((4 . t))) (let ((a 4) . t))
            (let*) (let* . t) (let* t) (let* () . t) (let* (4)) (let* ((4 t)))
            (let* ((4 . t))) (let* ((a 4) . t))
            (load-time-value) (load-time-value . t) (load-time-value t t t)
            (locally . t) (locally (declare . t))
            (macrolet) (macrolet . t) (macrolet t) (macrolet () . t)
            (macrolet (t)) (macrolet ((f))) (macrolet ((f . t)))
            (macrolet ((f ()) . t)) (macrolet ((4 ())))
            (multiple-value-call) (multiple-value-call . t)
            (multiple-value-prog1) (multiple-value-prog1 . t)
            (progn . t)
            (progv) (progv ()) (progv () () . t)
            (quote) (quote . t) (quote a b) (quote a . b)
            (return-from) (return-from 4) (return-from . t)
            (block nil (return-from nil a b))
            (setq *print-circle*) (setq . t) (setq *print-circle* 4 *readtable*)
            (setq 4) (setq 4 5) (let ((L (list 1))) (setq (car L) 4))
            (symbol-macrolet) (symbol-macrolet . t) (symbol-macrolet t)
            (symbol-macrolet () . t) (symbol-macrolet (4))
            (symbol-macrolet ((4 t))) (symbol-macrolet ((4 . t)))
            (symbol-macrolet ((a 4) . t))
            (tagbody . t)
            (the cons) (the cons '(4) t) (the . t)
            (throw) (throw nil) (throw nil nil nil) (throw nil nil . t)
            (throw . t) (throw nil . t)
            (unwind-protect) (unwind-protect . t)))))

(5am:test unknown-exit
  ;; these must fail immediately, unlike unknown references, since they
  ;; cannot be resolved
  (compilation-fails '(return-from a))
  (compilation-fails '(go a)))
