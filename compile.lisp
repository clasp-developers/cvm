(ql:quickload '#:alexandria)

(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile #:macroexpand-1 #:macroexpand))

(in-package #:compile-to-vm)

(setq *print-circle* t)

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(loop for i from 0
                        for name in names
                        collect `(defconstant ,name ,i))
                (defparameter *codes* '(,@names))
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +parse-key-args+
    +jump+ +jump-if+ +jump-if-supplied+
    +check-arg-count<=+ +check-arg-count>=+ +check-arg-count=+
    +push-values+ +append-values+ +pop-values+
    +mv-call+ +mv-call-receive-one+ +mv-call-receive-fixed+
    +entry+ +exit+ +entry-close+
    +catch+ +throw+ +catch-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +progv+
    +fdefinition+
    +nil+
    +eq+))

;;;

(defun macroexpand-1 (form env) (declare (ignore env)) (values form nil))
(defun macroexpand (form env) (declare (ignore env)) (values form nil))

;;;

(defstruct label function position)

(defun emit-label (context label)
  (setf (label-position label) (length (context-assembly context)))
  (setf (label-function label) (context-function context)))

(defun assemble (context &rest values)
  (let ((assembly (context-assembly context)))
    (dolist (value values)
      (if (label-p value)
          (let ((fixup (list value (context-function context) (length assembly))))
            (push fixup (cmodule-fixups (context-module context)))
            (vector-push-extend 0 assembly))
          (vector-push-extend value assembly)))))

;;; Different kinds of things can go in the variable namespace and they can
;;; all shadow each other, so we use this structure to disambiguate.
(defstruct (var-info (:constructor make-var-info (kind data)))
  (kind #+(or)(member :local :special :symbol-macro :constant))
  data)

(defun make-lexical-var-info (frame-offset)
  (make-var-info :local frame-offset))
(defun make-special-var-info () (make-var-info :special nil))
(defun make-symbol-macro-var-info (expansion)
  (make-var-info :symbol-macro expansion))
(defun make-constant-var-info (value) (make-var-info :constant value))

(defstruct (lexical-environment (:constructor make-null-lexical-environment)
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An alist of (var . var-info) in the current environment.
  (vars nil :type list)
  ;; An alist of (tag tag-dynenv . label) in the current environment.
  (tags nil :type list)
  ;; An alist of (block block-dynenv . label) in the current environment.
  (blocks nil :type list)
  ;; An alist of (fun . fun-var) in the current environment.
  (funs nil :type list)
  ;; The current end of the frame.
  (frame-end 0 :type integer)
  ;; A list of the non-local vars in scope.
  (closure-vars nil :type list))

(defun make-lexical-environment (parent &key (vars (vars parent))
                                             (tags (tags parent))
                                             (blocks (blocks parent))
                                             (frame-end (frame-end parent))
                                             (closure-vars (closure-vars parent))
                                             (funs (funs parent)))
  (%make-lexical-environment
   :vars vars :tags tags :blocks blocks :frame-end frame-end :closure-vars closure-vars
   :funs funs))

;;; Bind each variable to a stack location, returning a new lexical
;;; environment. The max local count in the current function is also
;;; updated.
(defun bind-vars (vars env context)
  (let* ((frame-start (frame-end env))
         (var-count (length vars))
         (frame-end (+ frame-start var-count))
         (function (context-function context)))
    (setf (cfunction-nlocals function)
          (max (cfunction-nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (vars vars (rest vars))
         (new-vars (vars env)
                   (acons (first vars) (make-lexical-var-info index) new-vars)))
        ((>= index frame-end)
         (make-lexical-environment env :vars new-vars :frame-end frame-end))
      (when (constantp (first vars))
        (error "Cannot bind constant value ~a!" (first vars))))))

;;; Create a new lexical environment where the old environment's
;;; lexicals get closed over.
(defun enclose (env)
  (multiple-value-bind (lexical nonlexical)
      (loop for pair in (vars env)
            for (var . info) = pair
            ;; this is necessary because we throw things on alist style
            ;; but need to not record shadowed variables here.
            when (member var seen)
              do (progn)
            else if (eq (var-info-kind info) :local)
                   collect var into lexical
            else
              collect pair into nonlexical
            collect var into seen
            finally (return (values lexical nonlexical)))
    (make-lexical-environment
     env
     :vars nonlexical
     :frame-end 0
     :closure-vars (append lexical (closure-vars env)))))

;;; Get information about a variable.
;;; Returns two values.
;;; The first is :CLOSURE, :LOCAL, :SPECIAL, :CONSTANT, :SYMBOL-MACRO, or NIL.
;;; If the variable is lexical, the first is :CLOSURE or :LOCAL,
;;; and the second is an index into the associated data.
;;; If the variable is special, the first is :SPECIAL and the second is NIL.
;;; If the variable is a macro, the first is :SYMBOL-MACRO and the second is
;;; the expansion.
;;; If the variable is a constant, :CONSTANT and the value.
;;; If the first value is NIL, the variable is unknown, and the second
;;; value is NIL.
(defun var-info (symbol env context)
  (let ((info (cdr (assoc symbol (vars env)))))
    (cond (info (values (var-info-kind info) (var-info-data info)))
          ((member symbol (closure-vars env))
           (values :closure (closure-index symbol context)))
          ((constantp symbol nil) (values :constant (eval symbol)))
          (t (values nil nil)))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defstruct (cfunction (:constructor make-cfunction (cmodule)))
  cmodule
  (bytecode (make-array 0
                        :element-type '(signed-byte 8)
                        :fill-pointer 0 :adjustable t))
  (nlocals 0)
  (closed (make-array 0 :fill-pointer 0 :adjustable t))
  (entry-point (make-label))
  module-offset
  info)

(defstruct (cmodule (:constructor make-cmodule (literals)))
  cfunctions
  literals
  fixups)

;;; The context contains information about what the current form needs
;;; to know about what it is enclosed by.
(defstruct context receiving function)

(defun context-module (context)
  (cfunction-cmodule (context-function context)))

(defun context-assembly (context)
  (cfunction-bytecode (context-function context)))

(defun literal-index (literal context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (position literal literals)
        (vector-push-extend literal literals))))

(defun closure-index (symbol context)
  (let ((closed (cfunction-closed (context-function context))))
    (or (position symbol closed)
        (vector-push-extend symbol closed))))

(defun new-context (parent &key (receiving (context-receiving parent))
                                (function (context-function parent)))
  (make-context :receiving receiving :function function))

(defun compile (lambda-expression)
  (check-type lambda-expression lambda-expression)
  (let* ((env (make-null-lexical-environment))
         (module (make-cmodule (make-array 0 :fill-pointer 0 :adjustable t))))
    (link-function (compile-lambda lambda-expression env module))))

(defun compile-form (form env context)
  (etypecase form
    (symbol (compile-symbol form env context))
    (cons (compile-cons (car form) (cdr form) env context))
    (t (compile-literal form env context))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (case form
      ((nil) (assemble context +nil+))
      (t (assemble context +const+ (literal-index form context))))))

(defun compile-symbol (form env context)
  (multiple-value-bind (kind data) (var-info form env context)
    (cond ((eq kind :symbol-macro) (compile-form data env context))
          ;; A symbol macro could expand into something with arbitrary side
          ;; effects so we always have to compile that, but otherwise, if no
          ;; values are wanted, we want to not compile anything.
          ((eql (context-receiving context) 0))
          (t
           (ecase kind
             ((:local) (assemble context +ref+ data +cell-ref+))
             ((:special) (assemble context +symbol-value+
                           (literal-index form context)))
             ((:closure) (assemble context +closure+ data +cell-ref+))
             ((:constant) (compile-literal data env context))
             ((nil)
              (warn "Unknown variable ~a: treating as special" form)
              (assemble context +symbol-value+
                (literal-index form context))))))))

(defun compile-cons (head rest env context)
  (case head
    ((progn) (compile-progn rest env context))
    ((let) (compile-let (first rest) (rest rest) env context))
    ((let*) (compile-let* (first rest) (rest rest) env context))
    ((flet) (compile-flet (first rest) (rest rest) env context))
    ((labels) (compile-labels (first rest) (rest rest) env context))
    ((setq) (compile-setq rest env context))
    ((if) (compile-if (first rest) (second rest) (third rest) env context))
    ((function) (compile-function (first rest) env context))
    ((tagbody) (compile-tagbody rest env context))
    ((go) (compile-go (first rest) env context))
    ((block) (compile-block (first rest) (rest rest) env context))
    ((return-from) (compile-return-from (first rest) (second rest) env context))
    ((catch) (compile-catch (first rest) (rest rest) env context))
    ((throw) (compile-throw (first rest) (second rest) env context))
    ((progv) (compile-progv (first rest) (second rest) (rest (rest rest)) env context))
    ((quote) (compile-literal (first rest) env context))
    ((symbol-macrolet)
     (compile-symbol-macrolet (first rest) (rest rest) env context))
    ((multiple-value-call)
     (compile-multiple-value-call (first rest) (rest rest) env context))
    ((multiple-value-prog1)
     (compile-multiple-value-prog1 (first rest) (rest rest) env context))
    (otherwise ; function call
     (compile-function head env (new-context context :receiving 1))
     (dolist (arg rest)
       (compile-form arg env (new-context context :receiving 1)))
     (let ((receiving (context-receiving context)))
       (cond ((eq receiving t) (assemble context +call+ (length rest)))
             ((eql receiving 1) (assemble context +call-receive-one+ (length rest)))
             (t (assemble context +call-receive-fixed+ (length rest) receiving)))))))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms)))
      ((null (rest forms))
       (compile-form (first forms) env context))
    (compile-form (first forms) env (new-context context :receiving 0))))

;;; Perform the effects of DECLARATIONS on ENV.
(defun process-declarations (declarations env)
  (dolist (declaration declarations)
    (dolist (specifier (cdr declaration))
      (case (first specifier)
        (special
         (do ((vars (rest specifier) (rest vars))
              (new-vars (vars env) (acons (first vars)
                                          (make-special-var-info)
                                          new-vars)))
             ((null vars)
              (setq env (make-lexical-environment env :vars new-vars))))))))
  env)

(defun parse-let (bindings env context)
  (let ((lexical-bindings '())
        (special-bindings '()))
    (dolist (binding bindings)
      (if (symbolp binding)
          (if (eq (var-info binding env context) :special)
              (push (cons binding nil) special-bindings)
              (push (cons binding nil) lexical-bindings))
          (if (eq (var-info (first binding) env context) :special)
              (push binding special-bindings)
              (push binding lexical-bindings))))
    (values (nreverse lexical-bindings) (nreverse special-bindings))))

(defun compile-let (bindings body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let ((env (process-declarations decls env))
          (lexical-count 0))
      (multiple-value-bind (lexical-bindings special-bindings)
          (parse-let bindings env context)
        (dolist (binding lexical-bindings)
          (compile-form (second binding) env (new-context context :receiving 1))
          (assemble context +make-cell+)
          (incf lexical-count))
        (when lexical-bindings
          (assemble context +bind+ lexical-count (frame-end env)))
        (let ((env (bind-vars (mapcar #'first lexical-bindings) env context))
              (special-count 0))
          (dolist (binding special-bindings)
            (compile-form (second binding) env (new-context context :receiving 1))
            (assemble context +special-bind+ (literal-index (first binding) context))
            (incf special-count))
          (compile-progn body env context)
          (dotimes (_ special-count)
            (assemble context +unbind+)))))))

(defun compile-let* (bindings body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let ((env (process-declarations decls env)))
      (multiple-value-bind (lexical-bindings special-bindings)
          (parse-let bindings env context)
        (dolist (binding lexical-bindings)
          (compile-form (second binding) env (new-context context :receiving 1))
          (assemble context +make-cell+)
          (assemble context +set+ (frame-end env))
          (setq env (bind-vars (list (first binding)) env context)))
        (let ((special-count 0))
          (dolist (binding special-bindings)
            (compile-form (second binding) env (new-context context :receiving 1))
            (assemble context +special-bind+ (literal-index (first binding) context))
            (incf special-count))
          (compile-progn body env context)
          (dotimes (_ special-count)
            (assemble context +unbind+)))))))

(defun compile-setq (pairs env context)
  (if (null pairs)
      (unless (eql (context-receiving context) 0)
        (assemble context +nil+))
      (loop for (var valf . rest) on pairs by #'cddr
            do (compile-setq-1 var valf env
                               (if rest
                                   (new-context context :receiving 0)
                                   context)))))

(defun compile-setq-1 (var valf env context)
  (multiple-value-bind (kind data) (var-info var env context)
    (ecase kind
      ((:symbol-macro)
       (compile-form `(setf ,data ,valf) env context))
      ((:special nil)
       (when (null kind) 
         (warn "Unknown variable ~a: treating as special" var))
       (compile-form valf env (new-context context :receiving 1))
       ;; If we need to return the new value, stick it into a new local
       ;; variable, do the set, then return the lexical variable.
       ;; We can't just read from the special, since some other thread may
       ;; alter it.
       (let ((index (frame-end env)))
         (unless (eql (context-receiving context) 0)
           (assemble context +set+ index +ref+ index)
           ;; called for effect, i.e. to keep frame size correct
           (bind-vars (list var) env context))
         (assemble context +symbol-value-set+ (literal-index var context))
         (unless (eql (context-receiving context) 0)
           (assemble context +ref+ index))))
      ((:local)
       (assemble context +ref+ data)
       (compile-form valf env (new-context context :receiving 1))
       (assemble context +cell-set+)
       (unless (eql (context-receiving context) 0)
         (assemble context +ref+ data +cell-ref+)))
      ((:closure)
       (assemble context +closure+ data)
       (compile-form valf env (new-context context :receiving 1))
       ;; similar concerns to specials above.
       (let ((index (frame-end env)))
         (unless (eql (context-receiving context) 0)
           (assemble context +set+ index +ref+ index)
           (bind-vars (list var) env context))
         (assemble context +cell-set+)
         (unless (eql (context-receiving context) 0)
           (assemble context +ref+ index)))))))

(defun compile-flet (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             ,@(cddr definition))
                          env (new-context context :receiving 1))
        (assemble context +make-cell+)
        (push fun-var fun-vars)
        (push (cons name fun-var) funs)
        (incf fun-count)))
    (assemble context +bind+ fun-count (frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs funs)))
      (compile-progn body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push (cons name fun-var) funs)
        (push fun-var fun-vars)
        (incf fun-count)))
    (dotimes (i fun-count)
      (assemble context +nil+ +make-cell+))
    (assemble context +bind+ fun-count (frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs funs)))
      (dolist (definition definitions)
        (reference-var (cdr (assoc (first definition) (funs env)))
                       env context)
        (compile-function `(lambda ,(second definition)
                             ,@(cddr definition))
                          env (new-context context :receiving 1))
        (assemble context +cell-set+))
      (compile-progn body env context))))

(defun compile-if (condition then else env context)
  (compile-form condition env (new-context context :receiving 1))
  (let ((then-label (make-label))
        (done-label (make-label)))
    (assemble context +jump-if+ then-label)
    (compile-form else env context)
    (assemble context +jump+ done-label)
    (emit-label context then-label)
    (compile-form then env context)
    (emit-label context done-label)))

(defun compile-function (fnameoid env context)
  (unless (eql (context-receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let ((cfunction (compile-lambda fnameoid env (context-module context))))
          (loop for var across (cfunction-closed cfunction)
                do (reference-var var env context))
          (assemble context +make-closure+ (literal-index cfunction context)))
        (let ((pair (assoc fnameoid (funs env))))
          (cond (pair
                 (reference-var (cdr pair) env context)
                 (assemble context +cell-ref+))
                (t
                 (assemble context +fdefinition+ (literal-index fnameoid context))))))))

;;; Deal with lambda lists. Return the new environment resulting from
;;; binding these lambda vars.
;;; Optional/key handling is done in two steps:
;;;
;;; 1. Bind any supplied optional/key vars to the passed values.
;;;
;;; 2. Default any unsupplied optional/key values and set the
;;; corresponding suppliedp var for each optional/key.
(defun compile-lambda-list (lambda-list env context)
  (multiple-value-bind (required optionals rest keys aok-p aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let* ((function (context-function context))
           (entry-point (cfunction-entry-point function))
           (min-count (length required))
           (optional-count (length optionals))
           (max-count (+ min-count optional-count))
           (key-count (length keys))
           (more-p (or rest keys))
           (env (bind-vars required env context)))
      (emit-label context entry-point)
      ;; Check that a valid number of arguments have been
      ;; supplied to this function.
      (cond ((and required (= min-count max-count) (not more-p))
             (assemble context +check-arg-count=+ min-count))
            (t
             (when required
               (assemble context +check-arg-count>=+ min-count))
             (when (not more-p)
               (assemble context +check-arg-count<=+ max-count))))
      (when required
        (assemble context +bind-required-args+ min-count)
        ;; Make mutable cells.
        (loop for i from (1- min-count) downto 0 do
          (assemble context +ref+ i +make-cell+))
        (when required
          (assemble context +bind+ min-count 0)))
      (when optionals
        (assemble context +bind-optional-args+
          min-count
          optional-count)
        (setq env (bind-vars (mapcar #'first optionals) env context)))
      (when rest
        (assemble context +listify-rest-args+ max-count)
        (assemble context +make-cell+)
        (assemble context +set+ (frame-end env))
        (setq env (bind-vars (list rest) env context)))
      (when keys
        (let ((key-name (mapcar #'caar keys)))
          (assemble context +parse-key-args+
            max-count
            (if aok-p (- key-count) key-count)
            (literal-index (first key-name) context)
            (frame-end env))
          (dolist (key-name (rest key-name))
            (literal-index key-name context)))
        (setq env (bind-vars (mapcar #'cadar keys) env context)))
      ;; Now emit code to default unsupplied values after the args
      ;; area and arg count are no longer needed, so it is safe to
      ;; destroy those with arbitrary code. Duplicate variables are
      ;; not allowed so we don't need to worry much about shadowing in
      ;; the environment. Supplied variables must be sequentially
      ;; bound however. KLUDGE: There has to be some way to share the
      ;; code below???
      (when optionals
        (do ((optionals optionals (rest optionals))
             (optional-label (make-label) next-optional-label)
             (next-optional-label (make-label) (make-label)))
            ((null optionals)
             (emit-label context optional-label))
          (emit-label context optional-label)
          (destructuring-bind (optional-var defaulting-form supplied-var)
              (first optionals)
            (flet ((default (suppliedp where)
                     (if suppliedp
                         (assemble context +ref+ where)
                         (compile-form defaulting-form env
                                       (new-context context :receiving 1)))
                     (assemble context +make-cell+)
                     (assemble context +set+ where))
                   (supply (suppliedp where)
                     (if suppliedp
                         (compile-literal t env (new-context context :receiving 1))
                         (assemble context +nil+))
                     (assemble context +make-cell+)
                     (assemble context +set+ where)))
              (let ((supplied-label (make-label))
                    (var-where (nth-value 1 (var-info optional-var env context)))
                    (supplied-var-where (frame-end env)))
                (assemble context +jump-if-supplied+ var-where supplied-label)
                (default nil var-where)
                (when supplied-var
                  (supply nil supplied-var-where))
                (assemble context +jump+ next-optional-label)
                (emit-label context supplied-label)
                (default t var-where)
                (when supplied-var
                  (supply t supplied-var-where)))
              (when supplied-var
                (setq env (bind-vars (list supplied-var) env context)))))))
      (when keys
        (do ((keys keys (rest keys))
             (key-label (make-label) next-key-label)
             (next-key-label (make-label) (make-label)))
            ((null keys)
             (emit-label context key-label))
          (emit-label context key-label)
          (destructuring-bind ((key-name key-var) defaulting-form supplied-var)
              (first keys)
            (declare (ignore key-name))
            (flet ((default (suppliedp where)
                     (if suppliedp
                         (assemble context +ref+ where)
                         (compile-form defaulting-form env
                                       (new-context context :receiving 1)))
                     (assemble context +make-cell+)
                     (assemble context +set+ where))
                   (supply (suppliedp where)
                     (if suppliedp
                         (compile-literal t env (new-context context :receiving 1))
                         (assemble context +nil+))
                     (assemble context +make-cell+)
                     (assemble context +set+ where)))
              (let ((supplied-label (make-label))
                    (var-where (nth-value 1 (var-info key-var env context)))
                    (supplied-var-where (frame-end env)))
                (assemble context +jump-if-supplied+ var-where supplied-label)
                (default nil var-where)
                (when supplied-var
                  (supply nil supplied-var-where))
                (assemble context +jump+ next-key-label)
                (emit-label context supplied-label)
                (default t var-where)
                (when supplied-var
                  (supply t supplied-var-where)))
              (when supplied-var
                (setq env (bind-vars (list supplied-var) env context)))))))
      (values aux env))))

;;; Compile the lambda form in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (form env module)
  (let* ((function (make-cfunction module))
         (context (make-context :receiving t :function function))
         (env (enclose env)))
    (push function (cmodule-cfunctions module))
    (multiple-value-bind (aux-bindings env)
        (compile-lambda-list (cadr form) env context)
      (compile-let* aux-bindings (cddr form) env context))
    (assemble context +return+)
    function))

;;; Push VAR's value to the stack. VAR is known to be lexical.
(defun reference-var (var env context)
  (multiple-value-bind (kind index) (var-info var env context)
    (ecase kind
      ((:local) (assemble context +ref+ index))
      ((:closure) (assemble context +closure+ index)))))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let ((new-tags (tags env))
        (tagbody-dynenv (gensym "TAG-DYNENV")))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement tagbody-dynenv (make-label))
              new-tags)))
    (assemble context +entry+)
    (let ((env (make-lexical-environment
                (bind-vars (list tagbody-dynenv) env context)
                :tags new-tags)))
      ;; Bind the dynamic environment. We don't need a cell as it is
      ;; not mutable.
      (multiple-value-bind (kind index)
          (var-info tagbody-dynenv env context)
        (assert (eq kind :local))
        (assemble context +set+ index))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cddr (assoc statement (tags env))))
            (compile-form statement env (new-context context :receiving 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (context-receiving context) 0)
    (assemble context +nil+)))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (tags env))))
    (if pair
        (destructuring-bind (tag-dynenv . tag-label) (cdr pair)
          (reference-var tag-dynenv env context)
          (assemble context +exit+ tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (make-lexical-environment
               (bind-vars (list block-dynenv) env context)
               :blocks (acons name (cons block-dynenv (make-label))
                              (blocks env)))))
    (assemble context +entry+)
    ;; Bind the dynamic environment. We don't need a cell as it is
    ;; not mutable.
    (multiple-value-bind (kind index)
        (var-info block-dynenv env context)
      (assert (eq kind :local))
      (assemble context +set+ index))
    (compile-progn body env context)
    (emit-label context (cddr (assoc name (blocks env))))
    (assemble context +entry-close+)))

(defun compile-return-from (name value env context)
  ;;; FIXME: We currently do the wrong thing with fixed return values!
  (compile-form value env (new-context context :receiving t))
  (let ((pair (assoc name (blocks env))))
    (if pair
        (destructuring-bind (block-dynenv . block-label) (cdr pair)
          (reference-var block-dynenv env context)
          (assemble context +exit+ block-label))
        (error "The block ~a does not exist." name))))

(defun compile-catch (tag body env context)
  (compile-form tag env (new-context context :receiving 1))
  (let ((target (make-label)))
    (assemble context +catch+ target)
    (compile-progn body env context)
    (assemble context +catch-close+)
    (emit-label context target)))

(defun compile-throw (tag result env context)
  (compile-form tag env (new-context context :receiving 1))
  ;; FIXME: same values problem as with RETURN-FROM, except worse,
  ;; because we rely on no additional stack values here to even
  ;; perform the throw!
  (compile-form result env (new-context context :receiving t))
  (assemble context +throw+))

(defun compile-progv (symbols values body env context)
  (compile-form symbols env (new-context context :receiving 1))
  (compile-form values env (new-context context :receiving 1))
  (assemble context +progv+)
  (compile-progn body env context)
  (assemble context +unbind+))

(defun compile-symbol-macrolet (bindings body env context)
  (let* ((smacros (loop for (symbol expansion) in bindings
                        for info = (make-symbol-macro-var-info expansion)
                        collect (cons symbol info)))
         (new-env (make-lexical-environment
                   env :vars (append smacros (vars env)))))
    (compile-progn body new-env context)))

(defun compile-multiple-value-call (function-form forms env context)
  (compile-form function-form env (new-context context :receiving 1))
  (let ((first (first forms))
        (rest (rest forms)))
    (compile-form first env (new-context context :receiving t))
    (when rest
      (assemble context +push-values+)
      (dolist (form rest)
        (compile-form form env (new-context context :receiving t))
        (assemble context +append-values+))
      (assemble context +pop-values+)))
  (let ((receiving (context-receiving context)))
    (cond ((eq receiving t) (assemble context +mv-call+))
          ((eql receiving 1) (assemble context +mv-call-receive-one+))
          (t (assemble context +mv-call-receive-fixed+)))))

(defun compile-multiple-value-prog1 (first-form forms env context)
  (compile-form first-form env context)
  (unless (member (context-receiving context) '(0 1))
    (assemble context +push-values+))
  (dolist (form forms)
    (compile-form form env (new-context context :receiving 0)))
  (unless (member (context-receiving context) '(0 1))
    (assemble context +pop-values+)))

;;;; linkage

;;; Run down the hierarchy and link the compile time representations
;;; of modules and functions together into runtime objects. Return the
;;; bytecode function corresponding to CFUNCTION.
(defun link-function (cfunction)
  (let ((cmodule (cfunction-cmodule cfunction))
        (bytecode-size 0)
        (bytecode-module (vm::make-bytecode-module)))
    ;; First, create the real function objects. determining the length
    ;; of the bytecode-module bytecode vector.
    (dolist (cfunction (cmodule-cfunctions cmodule))
      (let ((bytecode-function
              (vm::make-bytecode-function
               :module bytecode-module
               :locals-frame-size (cfunction-nlocals cfunction)
               :environment-size (length (cfunction-closed cfunction)))))
        (setf (cfunction-module-offset cfunction) bytecode-size)
        (setf (cfunction-info cfunction) bytecode-function)
        (incf bytecode-size (length (cfunction-bytecode cfunction)))))
    (let* ((cmodule-literals (cmodule-literals cmodule))
           (literal-length (length cmodule-literals))
           (bytecode (make-array bytecode-size :element-type '(signed-byte 8)))
           (literals (make-array literal-length)))
      ;; Next, fill in the module bytecode vector.
      (let ((index 0))
        (dolist (cfunction (cmodule-cfunctions cmodule))
          (let ((function-bytecode (cfunction-bytecode cfunction)))
            (dotimes (local-index (length function-bytecode))
              (setf (aref bytecode index)
                    (aref function-bytecode local-index))
              (incf index)))))
      (flet ((compute-position (function offset)
               (+ (cfunction-module-offset function) offset)))
        ;; Do label fixups in the module.
        (dolist (fixup (cmodule-fixups cmodule))
          (destructuring-bind (label function offset) fixup
            (let ((position (compute-position function offset)))
              (setf (aref bytecode position)
                    (- (compute-position (label-function label)
                                         (label-position label))
                       position)))))
        ;; Compute entry points.
        (dolist (cfunction (cmodule-cfunctions cmodule))
          (setf (vm::bytecode-function-entry-pc (cfunction-info cfunction))
                (compute-position cfunction
                                  (label-position (cfunction-entry-point cfunction))))))
      ;; Now replace the cfunctions in the cmodule literal vector with
      ;; real bytecode functions.
      (dotimes (index (length (cmodule-literals cmodule)))
        (setf (aref literals index)
              (let ((literal (aref cmodule-literals index)))
                (if (cfunction-p literal)
                    (cfunction-info literal)
                    literal))))
      (setf (vm::bytecode-module-bytecode bytecode-module) bytecode)
      (setf (vm::bytecode-module-literals bytecode-module) literals)
      (cfunction-info cfunction))))

;;; --------------------------------------------------
;;;
;;; Generate C++ code for the VM bytecodes
;;;
;;; Generate an enum called vm_codes that sets the values
;;; for all of the vm bytecodes according to the order in
;;; which they are defined above in *codes*.
;;;

(defun c++ify (name)
  (flet ((submatch (substr remain)
           (let ((sublen (length substr)))
             (and (>= (length remain) sublen) (string= substr remain :start2 0 :end2 sublen)))))
    (with-output-to-string (sout)
      (loop for index below (length name)
            for remain = (subseq name index)
            for chr = (elt remain 0)
            do (cond
                 ((submatch "/=" remain)
                  (format sout "_NE_")
                  (incf index))
                 ((submatch ">=" remain)
                  (format sout "_GE_")
                  (incf index))
                 ((submatch "<=" remain)
                  (format sout "_LE_")
                  (incf index))
                 ((char= chr #\=) (format sout "_EQ_"))
                 ((char= chr #\<) (format sout "_LT_"))
                 ((char= chr #\>) (format sout "_GT_"))
                 ((char= chr #\-) (format sout "_"))
                 (t (format sout "~a" chr)))))))

(defun generate-header (&optional (file-name "virtualMachine.h"))
  (with-open-file (fout file-name :direction :output :if-exists :supersede)
    (let ((enums (loop for sym in *codes*
                       for index from 0
                       for trimmed-sym-name = (string-downcase (string-trim "+" (symbol-name sym)))
                       for sym-name = (format nil "vm_~a" (c++ify trimmed-sym-name))
                       collect (format nil "~a=~a" sym-name index))))
      (format fout "enum vm_codes {~%~{   ~a~^,~^~%~} };~%" enums))))
