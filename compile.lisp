(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile #:macroexpand-1 #:macroexpand #:constantp))

(in-package #:compile-to-vm)

(setq *print-circle* t)

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(loop for i from 0
                        for name in names
                        collect `(defconstant ,name ,i))
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+
    +return+
    +jump+ +jump-if+
    +entry+ +exit+ +entry-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +fdefinition+
    +nil+
    +eq+))

;;;

(defun macroexpand-1 (form env) (declare (ignore env)) (values form nil))
(defun macroexpand (form env) (declare (ignore env)) (values form nil))
(defun specialp (symbol env) (declare (ignore symbol env)) nil)
(defun constantp (symbol env) (declare (ignore symbol env)) nil)
(defun constant-form-value (symbol env) (declare (ignore symbol env)))

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

(defstruct (lexical-environment (:constructor make-null-lexical-environment)
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An alist of (var . frame-offset) in the current environment.
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
         (new-vars (vars env) (acons (first vars) index new-vars)))
        ((>= index frame-end)
         (make-lexical-environment env :vars new-vars :frame-end frame-end)))))

;;; Create a new lexical environment where the old environment's
;;; lexicals get closed over.
(defun enclose (env)
  (make-lexical-environment
   env
   :vars '()
   :frame-end 0
   :closure-vars (append (mapcar #'first (vars env)) (closure-vars env))))

;;; Get information about a lexical variable.
;;; Returns two values. The first is :CLOSURE or :LOCAL or NIL.
;;; The second is an index into the associated data corresponding to the symbol, or NIL.
;;; If the first value is NIL, the variable is unknown.
(defun var-location (symbol env context)
  (let ((pair (assoc symbol (vars env))))
    (if pair
        (values :local (cdr pair))
        (if (member symbol (closure-vars env))
            (values :closure (closure-index symbol context))
            (values nil nil)))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defstruct (cfunction (:constructor make-cfunction (cmodule bytecode nlocals closed)))
  cmodule bytecode nlocals closed info)

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
  (let ((form (macroexpand form env)))
    (etypecase form
      (symbol (compile-symbol form env context))
      (cons (compile-cons (car form) (cdr form) env context))
      (t (compile-literal form env context)))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (assemble context +const+ (literal-index form context))))

(defun compile-symbol (form env context)
  (unless (eql (context-receiving context) 0)
    (cond ((specialp form env)
           (assemble context +symbol-value+ (literal-index form context)))
          ((constantp form env)
           (assemble context +const+ (literal-index (constant-form-value form env) context)))
          (t ; lexical
           (multiple-value-bind (kind index) (var-location form env context)
             (ecase kind
               ((:local) (assemble context +ref+ index +cell-ref+))
               ((:closure) (assemble context +closure+ index +cell-ref+))
               ((nil)
                (warn "Unknown variable ~a: treating as special" form)
                (assemble context +symbol-value+ (literal-index form context)))))))))

(defun compile-cons (head rest env context)
  (case head
    ((progn) (compile-progn rest env context))
    ((let) (compile-let (first rest) (rest rest) env context))
    ((flet) (compile-flet (first rest) (rest rest) env context))
    ((labels) (compile-labels (first rest) (rest rest) env context))
    ((setq) (compile-setq rest env context))
    ((if) (compile-if (first rest) (second rest) (third rest) env context))
    ((function) (compile-function (first rest) env context))
    ((tagbody) (compile-tagbody rest env context))
    ((go) (compile-go (first rest) env context))
    ((block) (compile-block (first rest) (rest rest) env context))
    ((return-from) (compile-return-from (first rest) (second rest) env context))
    ((quote) (compile-literal (first rest) env context))
    (otherwise ; function call
     (dolist (arg rest)
       (compile-form arg env (new-context context :receiving 1)))
     (compile-function head env (new-context context :receiving 1))
     (let ((receiving (context-receiving context)))
       (cond ((eq receiving t) (assemble context +call+ (length rest)))
             ((eql receiving 1) (assemble context +call-receive-one+ (length rest)))
             (t (assemble context +call-receive-fixed+ (length rest) receiving)))))))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms)))
      ((null (rest forms))
       (compile-form (first forms) env context))
    (compile-form (first forms) env (new-context context :receiving 0))))

(defun compile-let (bindings body env context)
  (let ((vars
          ;; Compile the values as we go.
          ;; FIXME: NLX will complicate this.
          (loop for binding in bindings
                if (symbolp binding)
                  collect binding
                  and do (assemble context +nil+)
                if (and (consp binding) (null (cdr binding)))
                  collect (car binding)
                  and do (assemble context +nil+)
                if (and (consp binding) (consp (cdr binding)) (null (cddr binding)))
                  collect (car binding)
                  and do (compile-form (cadr binding) env (new-context context :receiving 1))
                do (assemble context +make-cell+))))
    (assemble context +bind+ (length vars) (frame-end env))
    (compile-progn body (bind-vars vars env context) context)))

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
  (cond ((nth-value 1 (macroexpand-1 var env))
         (compile-form `(setf ,var ,valf) env context))
        ((specialp var env)
         (compile-form valf env (new-context context :receiving 1))
         (assemble context +symbol-value-set+ (literal-index var context))
         (unless (eql (context-receiving context) 0)
           ;; this is a bit tricky - we can't just read the symbol-value, since
           ;; that could have been changed in the interim.
           ;; The right thing to do is probably to bind a new lexical variable
           ;; to hold the value temporarily.
           (error "Can't return the value of special variable binding ~a yet! :(" var)))
        ((constantp var env) (error "Can't SETQ a constant: ~a" var))
        (t ; lexical
         (multiple-value-bind (kind index) (var-location var env context)
           (ecase kind
             ((:local)
              (assemble context +ref+ index)
              (compile-form valf env (new-context context :receiving 1))
              (assemble context +cell-set+))
             ((:closure)
              (assemble context +closure+ index)
              (compile-form valf env (new-context context :receiving 1))
              (assemble context +cell-set+))
             ((nil)
              (warn "Unknown variable ~a: treating as special" var)
              (compile-form valf env (new-context context :receiving 1))
              (assemble context +symbol-value-set+ (literal-index var context))
              (unless (eql (context-receiving context) 0)
                (error "Can't return the value of special variable binding ~a yet! :(" var))))))))

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

;;; Compile the lambda form in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (form env module)
  ;; TODO: Emit code to process lambda args.
  (let* ((lambda-list (cadr form))
         (body (cddr form))
         (function
           (make-cfunction
            module
            (make-array 0 :element-type '(signed-byte 8)
                          :fill-pointer 0 :adjustable t)
            0
            (make-array 0 :fill-pointer 0 :adjustable t)))
         (context (make-context :receiving t :function function))
         (env (enclose env)))
    (push function (cmodule-cfunctions module))
    ;; Initialize variables. Replace each value on the stack with a mutable cell
    ;; containing that value.
    (loop for i from 0 for arg in lambda-list
          do (assemble context +ref+ i +make-cell+ +set+ i))
    (compile-progn body (bind-vars lambda-list env context) context)
    (assemble context +return+)
    function))

;;; Push VAR's value to the stack.
(defun reference-var (var env context)
  (multiple-value-bind (kind index) (var-location var env context)
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
          (var-location tagbody-dynenv env context)
        (assert (eq kind :local))
        (assemble context +set+ index))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cdr (assoc statement (tags env))))
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
        (var-location block-dynenv env context)
      (assert (eq kind :local))
      (assemble context +set+ index))
    (compile-progn body env context)
    (emit-label context (cdr (block-info name env)))
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
               :environment-size (length (cfunction-closed cfunction))
               :entry-pc bytecode-size)))
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
      ;; Do label fixups in the module.
      (dolist (fixup (cmodule-fixups cmodule))
        (destructuring-bind (label function offset) fixup
          (flet ((compute-position (function offset)
                   (+ (vm::bytecode-function-entry-pc
                       (cfunction-info function))
                      offset)))
            (let ((position (compute-position function offset)))
              (setf (aref bytecode position)
                    (- (compute-position (label-function label)
                                         (label-position label))
                       position))))))
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
