(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '#:alexandria)
  (ql:quickload '#:trivial-cltl2))

(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile #:macroexpand-1 #:macroexpand))

(in-package #:compile-to-vm)

(setq *print-circle* t)

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(let ((forms nil))
                    (do ((i 0 (1+ i))
                         (names names (cdr names)))
                        ((endp names) forms)
                      (push `(defconstant ,(first names) ,i) forms)))
                (defparameter *codes* '(,@names))
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+ +make-uninitialized-closure+ +initialize-closure+
    +return+
    +bind-required-args+ +bind-optional-args+
    +listify-rest-args+ +parse-key-args+
    +jump-8+ +jump-16+ +jump-24+
    +jump-if-8+ +jump-if-16+ +jump-if-24+
    +jump-if-supplied-8+ +jump-if-supplied-16+
    +check-arg-count<=+ +check-arg-count>=+ +check-arg-count=+
    +push-values+ +append-values+ +pop-values+
    +mv-call+ +mv-call-receive-one+ +mv-call-receive-fixed+
    +entry+
    +exit-8+ +exit-16+ +exit-24+
    +entry-close+
    +catch-8+ +catch-16+
    +throw+ +catch-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +progv+
    +fdefinition+
    +nil+
    +eq+
    +pop+
    +push+
    +long+))

;;;

(defun macroexpand-1 (form env) (declare (ignore env)) (values form nil))
(defun macroexpand (form env) (declare (ignore env)) (values form nil))

;;;

;; An annotation in the function.
(defstruct annotation
  ;; The function containing this annotation.
  function
  ;; The index of this annotation in its function's annotations.
  index
  ;; The current (optimistic) position of this annotation in this function.
  position
  ;; The initial position of this annotaiton in this function.
  initial-position)

(defstruct (label (:include annotation)))

(defstruct (fixup (:include annotation)
                  (:constructor make-fixup (label initial-size emitter resizer
                                            &aux (size initial-size))))
  ;; The label this fixup references.
  label
  ;; The current (optimistic) size of this fixup in bytes.
  size
  ;; The initial size of this fixup in bytes.
  initial-size
  ;; How to emit this fixup once sizes are resolved.
  emitter
  ;; How to resize this fixup. Returns the new size.
  resizer)

(defmethod print-object ((label label) stream)
  (print-unreadable-object (label stream :identity t)
    (format stream "LABEL :POSITION ~d" (annotation-position label))))

(defmethod print-object ((fixup fixup) stream)
  (print-unreadable-object (fixup stream :identity t)
    (format stream "FIXUP :POSITION ~d :SIZE ~d"
            (annotation-position fixup)
            (fixup-size fixup))))

;;; Optimistic positioning of ANNOTATION in its module.
(defun annotation-module-position (annotation)
  (+ (cfunction-position (annotation-function annotation))
     (annotation-position annotation)))

;;; The (module) displacement from this fixup to its label,
(defun fixup-delta (fixup)
  (- (annotation-module-position (fixup-label fixup))
     (annotation-module-position fixup)))

(defun emit-label (context label)
  (setf (label-position label) (length (context-assembly context)))
  (let ((function (context-function context)))
    (setf (label-function label) function)
    (setf (label-index label)
          (vector-push-extend label (cfunction-annotations function)))))

(defun assemble (context &rest values)
  (let ((assembly (context-assembly context)))
    (dolist (value values)
      (vector-push-extend value assembly))))

(defun assemble-into (code position &rest values)
  (do ((values values (rest values))
       (position position (1+ position)))
      ((null values))
    (setf (aref code position) (first values))))

;;; Write WORD of bytesize SIZE to VECTOR at POSITION.
(defun write-le-unsigned (vector word size position)
  (let ((end (+ position size)))
    (do ((position position (1+ position))
         (word word (ash word -8)))
        ((>= position end))
      (setf (aref vector position) (logand word #xff)))))

;;; Emit FIXUP into CONTEXT.
(defun emit-fixup (context fixup)
  (let* ((assembly (context-assembly context))
         (cfunction (context-function context))
         (position (length assembly)))
    (setf (fixup-function fixup) cfunction)
    (setf (fixup-initial-position fixup) position)
    (setf (fixup-position fixup) position)
    (setf (fixup-index fixup)
          (vector-push-extend fixup (cfunction-annotations cfunction)))
    (dotimes (i (fixup-initial-size fixup))
      (vector-push-extend 0 assembly))))

;;; Emit OPCODE and then a label reference.
(defun emit-control+label (context opcode8 opcode16 opcode24 label)
  (flet ((emitter (fixup position code)
           (let* ((size (fixup-size fixup))
                  (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
             (setf (aref code position)
                   (ecase size (2 opcode8) (3 opcode16) (4 opcode24)))
             (write-le-unsigned code offset (1- size) (1+ position))))
         (resizer (fixup)
           (typecase (fixup-delta fixup)
             ((signed-byte 8) 2)
             ((signed-byte 16) 3)
             ((signed-byte 24) 4)
             (t (error "???? PC offset too big ????")))))
    (emit-fixup context (make-fixup label 2 #'emitter #'resizer))))

(defun emit-jump (context label)
  (emit-control+label context +jump-8+ +jump-16+ +jump-24+ label))
(defun emit-jump-if (context label)
  (emit-control+label context +jump-if-8+ +jump-if-16+ +jump-if-24+ label))
(defun emit-exit (context label)
  (emit-control+label context +exit-8+ +exit-16+ +exit-24+ label))
(defun emit-catch (context label)
  (emit-control+label context +catch-8+ +catch-16+ nil label))

(defun emit-jump-if-supplied (context index label)
  (flet ((emitter (fixup position code)
           (let* ((size (fixup-size fixup))
                  (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
             (setf (aref code position)
                   (ecase size
                     (3 +jump-if-supplied-8+)
                     (4 +jump-if-supplied-16+)))
             (setf (aref code (1+ position)) index)
             (write-le-unsigned code offset (- size 2) (+ position 2))))
         (resizer (fixup)
           (typecase (fixup-delta fixup)
             ((signed-byte 8) 3)
             ((signed-byte 16) 4)
             (t (error "???? PC offset too big ????")))))
    (emit-fixup context (make-fixup label 3 #'emitter #'resizer))))

(defun emit-const (context index)
  (if (> index 255)
      (assemble context
        +long+ +const+
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context +const+ index)))

(defun emit-fdefinition (context index)
  (if (> index 255)
      (assemble context
        +long+ +fdefinition+
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context +fdefinition+ index)))

(defun emit-parse-key-args (context max-count key-count key-names env aok-p)
  (if (<= key-count 127)
      (assemble context +parse-key-args+
                max-count
                (if aok-p (boole boole-ior 128 key-count) key-count)
                (literal-index (first key-names) context)
                (frame-end env))
      (error "Handle more than 127 keyword parameters - you need ~s" key-count)))

(defun emit-bind (context count offset)
  (cond ((= count 1) (assemble context +set+ offset))
        ((= count 0))
        (t (assemble context +bind+ count offset))))

;;; Different kinds of things can go in the variable namespace and they can
;;; all shadow each other, so we use this structure to disambiguate.
(defstruct (var-info (:constructor make-var-info (kind data)))
  (kind (error "kind required")
   :type (member :lexical :special :symbol-macro :constant))
  data)

(defstruct (lexical-info (:constructor make-lexical-info (frame-offset function)))
  frame-offset
  function
  (closed-over-p nil)
  (set-p nil))

;;; Does the variable with LEXICAL-INFO need a cell?
(defun indirect-lexical-p (lexical-info)
  (and (lexical-info-closed-over-p lexical-info)
       (lexical-info-set-p lexical-info)))

(defun make-lexical-var-info (frame-offset function)
  (make-var-info :lexical (make-lexical-info frame-offset function)))
(defun make-special-var-info () (make-var-info :special nil))
(defun make-symbol-macro-var-info (expansion)
  (make-var-info :symbol-macro expansion))
(defun make-constant-var-info (value) (make-var-info :constant value))

(defstruct (fun-info (:constructor make-fun-info (kind data)))
  (kind (error "kind required")
   :type (member :global-function :global-macro
                 :local-function :local-macro))
  data)

(defun make-global-function-fun-info () (make-fun-info :global-function nil))
(defun make-global-macro-fun-info (expander)
  (make-fun-info :global-macro expander))
(defun make-local-function-fun-info (fun-var)
  (make-fun-info :local-function fun-var))
(defun make-local-macro-fun-info (expander)
  (make-fun-info :local-macro expander))

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
  (frame-end 0 :type integer))

(defun make-lexical-environment (parent &key (vars (vars parent))
                                             (tags (tags parent))
                                             (blocks (blocks parent))
                                             (frame-end (frame-end parent))
                                             (funs (funs parent)))
  (%make-lexical-environment
   :vars vars :tags tags :blocks blocks :frame-end frame-end :funs funs))

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
                   (acons (first vars) (make-lexical-var-info index function) new-vars)))
        ((>= index frame-end)
         (make-lexical-environment env :vars new-vars :frame-end frame-end))
      (when (constantp (first vars))
        (error "Cannot bind constant value ~a!" (first vars))))))

;;; Get information about a variable.
;;; Returns two values.
;;; The first is :LEXICAL, :SPECIAL, :CONSTANT, :SYMBOL-MACRO, or NIL.
;;; If the variable is lexical, the first is :LEXICAL and the second is more info.
;;; If the variable is special, the first is :SPECIAL and the second is NIL.
;;; If the variable is a macro, the first is :SYMBOL-MACRO and the second is
;;; the expansion.
;;; If the variable is a constant, :CONSTANT and the value.
;;; If the first value is NIL, the variable is unknown, and the second
;;; value is NIL.
(defun var-info (symbol env)
  (let ((info (cdr (assoc symbol (vars env)))))
    (cond (info (values (var-info-kind info) (var-info-data info)))
          ((constantp symbol nil) (values :constant (eval symbol)))
          ;; globally special
          ((trivial-cltl2:variable-information symbol) (values :special nil))
          (t (values nil nil)))))

;;; Like the above. Check the struct for details.
(defun fun-info (symbol env)
  (let ((info (cdr (assoc symbol (funs env)))))
    (cond (info (values (fun-info-kind info) (fun-info-data info)))
          ((macro-function symbol nil)
           (values :global-macro (macro-function symbol nil)))
          ((special-operator-p symbol)
           (error "Tried to get FUN-INFO for a special operator - impossible"))
          ((fboundp symbol) (values :global-function nil))
          (t (values nil nil)))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defstruct (cfunction (:constructor make-cfunction (cmodule)))
  cmodule
  ;; Bytecode vector for this function.
  (bytecode (make-array 0 :element-type '(unsigned-byte 8)
                          :fill-pointer 0 :adjustable t))
  ;; An ordered vector of annotations emitted in this function.
  (annotations (make-array 0 :fill-pointer 0 :adjustable t))
  (nlocals 0)
  (closed (make-array 0 :fill-pointer 0 :adjustable t))
  (entry-point (make-label))
  ;; The position of the start of this function in this module
  ;; (optimistic).
  position
  ;; How much to add to the bytecode vector length for increased fixup
  ;; sizes for the true length.
  (extra 0)
  ;; The index of this function in the containing module's function
  ;; vector.
  index
  info)

(defstruct (cmodule (:constructor make-cmodule (literals)))
  (cfunctions (make-array 1 :fill-pointer 0 :adjustable t))
  literals)

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

(defun closure-index (info context)
  (let ((closed (cfunction-closed (context-function context))))
    (or (position info closed)
        (vector-push-extend info closed))))

(defun new-context (parent &key (receiving (context-receiving parent))
                                (function (context-function parent)))
  (make-context :receiving receiving :function function))

(defun compile (lambda-expression
                &optional (env (make-null-lexical-environment)))
  (check-type lambda-expression lambda-expression)
  (let ((module (make-cmodule (make-array 0 :fill-pointer 0 :adjustable t)))
        (lambda-list (cadr lambda-expression))
        (body (cddr lambda-expression)))
    (link-function (compile-lambda lambda-list body env module))))

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
      (t (emit-const context (literal-index form context))))
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(defun compile-load-time-value (form read-only-p env context)
  (declare (ignore read-only-p))
  (compile-literal (eval form) env context))

(flet ((maybe-emit (lexical-info opcode context)
         (assert lexical-info)
         (flet ((emitter (fixup position code)
                  (assert (= (fixup-size fixup) 1))
                  (setf (aref code position) opcode))
                (resizer (fixup)
                  (declare (ignore fixup))
                  (if (indirect-lexical-p lexical-info) 1 0)))
           (emit-fixup context
                       (make-fixup lexical-info 0 #'emitter #'resizer)))))
  (defun maybe-emit-make-cell (lexical-info context)
    (maybe-emit lexical-info +make-cell+ context))
  (defun maybe-emit-cell-ref (lexical-info context)
    (maybe-emit lexical-info +cell-ref+ context)))

;;; FIXME: This is probably a good candidate for a specialized
;;; instruction.
(defun maybe-emit-encage (lexical-info context)
  (let ((index (lexical-info-frame-offset lexical-info)))
    (flet ((emitter (fixup position code)
             (assert (= (fixup-size fixup) 5))
             (assemble-into code position
                            +ref+ index +make-cell+ +set+ index))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-info) 5 0)))
      (emit-fixup context (make-fixup lexical-info 0 #'emitter #'resizer)))))

(defun emit-lexical-set (lexical-info context)
  (let ((index (lexical-info-frame-offset lexical-info)))
    (flet ((emitter (fixup position code)
             (if (= (fixup-size fixup) 3)
                 (assemble-into code position +ref+ index +cell-set+)
                 (assemble-into code position +set+ index)))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-info) 3 2)))
      (emit-fixup context (make-fixup lexical-info 2 #'emitter #'resizer)))))

(defun compile-symbol (form env context)
  (multiple-value-bind (kind data) (var-info form env)
    (cond ((eq kind :symbol-macro) (compile-form data env context))
          ;; A symbol macro could expand into something with arbitrary side
          ;; effects so we always have to compile that, but otherwise, if no
          ;; values are wanted, we want to not compile anything.
          ((eql (context-receiving context) 0))
          (t
           (ecase kind
             ((:lexical)
              (cond ((eq (lexical-info-function data) (context-function context))
                     (assemble context +ref+ (lexical-info-frame-offset data)))
                    (t
                     (setf (lexical-info-closed-over-p data) t)
                     (assemble context +closure+ (closure-index data context))))
              (maybe-emit-cell-ref data context))
             ((:special) (assemble context +symbol-value+
                           (literal-index form context)))
             ((:constant) (return-from compile-symbol ; don't pop again.
                            (compile-literal data env context)))
             ((nil)
              (warn "Unknown variable ~a: treating as special" form)
              (assemble context +symbol-value+
                (literal-index form context))))
           (when (eq (context-receiving context) t)
             (assemble context +pop+))))))

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
    ((load-time-value) (compile-load-time-value (first rest) (second rest) env context))
    ((symbol-macrolet)
     (compile-symbol-macrolet (first rest) (rest rest) env context))
    ((macrolet)
     (compile-macrolet (first rest) (rest rest) env context))
    ((multiple-value-call)
     (compile-multiple-value-call (first rest) (rest rest) env context))
    ((multiple-value-prog1)
     (compile-multiple-value-prog1 (first rest) (rest rest) env context))
    ((locally) (compile-locally rest env context))
    ((eval-when) (compile-eval-when (first rest) (rest rest) env context))
    ((the) ; don't do anything.
     (compile-form (second rest) env context))
    (otherwise ; function call or macro
     (multiple-value-bind (kind data) (fun-info head env)
       (ecase kind
         ((:global-macro :local-macro)
          (compile-form (funcall *macroexpand-hook* data (cons head rest) env)
                        env context))
         ((:global-function :local-function nil)
          ;; unknown function warning handled by compile-function
          ;; note we do a double lookup, which is inefficient
          (compile-function head env (new-context context :receiving 1))
          (dolist (arg rest)
            (compile-form arg env (new-context context :receiving 1)))
          (let ((receiving (context-receiving context)))
            (cond ((eq receiving t) (assemble context +call+ (length rest)))
                  ((eql receiving 1)
                   (assemble context +call-receive-one+ (length rest)))
                  (t (assemble context
                       +call-receive-fixed+ (length rest) receiving))))))))))

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

(defun compile-locally (body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (compile-progn body (process-declarations decls env) context)))

(defun compile-eval-when (situations body env context)
  (if (or (member 'cl:eval situations) (member :execute situations))
      (compile-progn body env context)
      (compile-literal nil env context)))

;;; Return the variable and value form for a LET binding.
(defun canonicalize-binding (binding)
  (if (consp binding)
      (values (first binding) (second binding))
      (values binding nil)))

(defun compile-let (bindings body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let ((env (process-declarations decls env))
          (lexical-binding-count 0)
          (special-binding-count 0)
          (post-binding-env
            (bind-vars
             (remove-if (lambda (var)
                          (eq (var-info var env) :special))
                        (mapcar #'canonicalize-binding bindings))
             env
             context)))
      (dolist (binding bindings)
        (multiple-value-bind (var valf) (canonicalize-binding binding)
          (compile-form valf env (new-context context :receiving 1))
          (cond ((eq (var-info var env) :special)
                 (incf special-binding-count)
                 (assemble context +special-bind+ (literal-index var context)))
                (t
                 (incf lexical-binding-count)
                 (maybe-emit-make-cell (nth-value 1 (var-info var post-binding-env))
                                       context)))))
      (emit-bind context lexical-binding-count (frame-end env))
      (compile-progn body post-binding-env context)
      (dotimes (_ special-binding-count)
        (assemble context +unbind+)))))

(defun compile-let* (bindings body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let ((env (process-declarations decls env))
          (special-binding-count 0))
      (dolist (binding bindings)
        (multiple-value-bind (var valf) (canonicalize-binding binding)
          (compile-form valf env (new-context context :receiving 1))
          (cond ((eq (var-info var env) :special)
                 (incf special-binding-count)
                 (assemble context +special-bind+ (literal-index var context)))
                (t
                 (let ((frame-start (frame-end env)))
                   (setq env (bind-vars (list var) env context))
                   (maybe-emit-make-cell (nth-value 1 (var-info var env))
                                         context)
                   (assemble context +set+ frame-start))))))
      (compile-progn body env context)
      (dotimes (_ special-binding-count)
        (assemble context +unbind+)))))

(defun compile-setq (pairs env context)
  (if (null pairs)
      (unless (eql (context-receiving context) 0)
        (assemble context +nil+))
      (do ((pairs pairs (cddr pairs)))
          ((endp pairs))
        (let ((var (car pairs))
              (valf (cadr pairs))
              (rest (cddr pairs)))
          (compile-setq-1 var valf env
                          (if rest
                              (new-context context :receiving 0)
                              context))))))

(defun compile-setq-1 (var valf env context)
  (multiple-value-bind (kind data) (var-info var env)
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
           (assemble context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+)))))
      ((:lexical)
       (let ((localp (eq (lexical-info-function data)
                         (context-function context)))
             (index (frame-end env)))
         (unless localp
           (setf (lexical-info-closed-over-p data) t))
         (setf (lexical-info-set-p data) t)
         (compile-form valf env (new-context context :receiving 1))
         ;; similar concerns to specials above.
         (unless (eql (context-receiving context) 0)
           (assemble context +set+ index +ref+ index)
           (bind-vars (list var) env context))
         (cond (localp
                (emit-lexical-set data context))
               ;; Don't emit a fixup if we already know we need a cell.
               (t
                (assemble context +closure+ (closure-index data context))
                (assemble context +cell-set+)))
         (unless (eql (context-receiving context) 0)
           (assemble context +ref+ index)
           (when (eql (context-receiving context) t)
             (assemble context +pop+))))))))

(defun fun-name-block-name (fun-name)
  (if (symbolp fun-name)
      fun-name
      ;; setf name
      (second fun-name)))

(defun compile-flet (definitions body env context)
  (let ((fun-vars '())
        (funs '())
        (fun-count 0)
        ;; HACK FIXME
        (frame-slot (frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "FLET-FUN")))
        (compile-function `(lambda ,(second definition)
                             (block ,(fun-name-block-name name)
                               (locally ,@(cddr definition))))
                          env (new-context context :receiving 1))
        (push fun-var fun-vars)
        (push (cons name (make-local-function-fun-info
                          (make-lexical-info frame-slot
                                             (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (emit-bind context fun-count (frame-end env))
    (let ((env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (funs env)))))
      (compile-progn body env context))))

(defun compile-labels (definitions body env context)
  (let ((fun-count 0)
        (funs '())
        (fun-vars '())
        (closures '())
        (env env)
        (frame-start (frame-end env))
        (frame-slot (frame-end env)))
    (dolist (definition definitions)
      (let ((name (first definition))
            (fun-var (gensym "LABELS-FUN")))
        (push fun-var fun-vars)
        (push (cons name (make-local-function-fun-info
                          (make-lexical-info frame-slot
                                             (context-function context))))
              funs)
        (incf frame-slot)
        (incf fun-count)))
    (let ((frame-slot (frame-end env))
          (env (make-lexical-environment
                (bind-vars fun-vars env context)
                :funs (append funs (funs env)))))
      (dolist (definition definitions)
        (let* ((name (first definition))
               (fun (compile-lambda (second definition)
                                    `((block ,(fun-name-block-name name)
                                        (locally ,@(cddr definition))))
                                    env
                                    (context-module context)))
               (literal-index (literal-index fun context)))
          (cond ((zerop (length (cfunction-closed fun)))
                 (emit-const context literal-index))
                (t
                 (push (cons fun frame-slot) closures)
                 (assemble context +make-uninitialized-closure+
                   literal-index))))
        (incf frame-slot))
      (emit-bind context fun-count frame-start)
      (dolist (closure closures)
        (loop for info across (cfunction-closed (car closure)) do
          (reference-lexical-info info context))
        (assemble context +initialize-closure+ (cdr closure)))
      (compile-progn body env context))))

(defun compile-if (condition then else env context)
  (compile-form condition env (new-context context :receiving 1))
  (let ((then-label (make-label))
        (done-label (make-label)))
    (emit-jump-if context then-label)
    (compile-form else env context)
    (emit-jump context done-label)
    (emit-label context then-label)
    (compile-form then env context)
    (emit-label context done-label)))

;;; Push the immutable value or cell of lexical in CONTEXT.
(defun reference-lexical-info (info context)
  (if (eq (lexical-info-function info) (context-function context))
      (assemble context +ref+ (lexical-info-frame-offset info))
      (assemble context +closure+ (closure-index info context))))

(defun compile-function (fnameoid env context)
  (unless (eql (context-receiving context) 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((cfunction (compile-lambda (cadr fnameoid) (cddr fnameoid)
                                         env (context-module context)))
               (closed (cfunction-closed cfunction)))
          (loop for info across closed do
            (reference-lexical-info info context))
          (if (zerop (length closed))
              (emit-const context (literal-index cfunction context))
              (assemble context +make-closure+ (literal-index cfunction context))))
        (multiple-value-bind (kind data) (fun-info fnameoid env)
          (ecase kind
            ((:global-function nil)
             (when (null kind) (warn "Unknown function ~a" fnameoid))
             (emit-fdefinition context (literal-index fnameoid context)))
            ((:local-function)
             (reference-lexical-info data context)))))
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

;;; Deal with lambda lists. Return the new environment resulting from
;;; binding these lambda vars.
;;; Optional/key handling is done in two steps:
;;;
;;; 1. Bind any supplied optional/key vars to the passed values.
;;;
;;; 2. Default any unsupplied optional/key values and set the
;;; corresponding suppliedp var for each optional/key.
(defun compile-lambda-list (lambda-list env context)
  (multiple-value-bind (required optionals rest keys aok-p aux key-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let* ((function (context-function context))
           (entry-point (cfunction-entry-point function))
           (min-count (length required))
           (optional-count (length optionals))
           (max-count (+ min-count optional-count))
           (key-count (length keys))
           (more-p (or rest key-p))
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
      (unless (zerop min-count)
        (assemble context +bind-required-args+ min-count)
        (dolist (var required)
          (maybe-emit-encage (nth-value 1 (var-info var env)) context)))
      (unless (zerop optional-count)
        (assemble context +bind-optional-args+
          min-count
          optional-count)
        (setq env (bind-vars (mapcar #'first optionals) env context)))
      (when rest
        (assemble context +listify-rest-args+ max-count)
        (assemble context +set+ (frame-end env))
        (setq env (bind-vars (list rest) env context))
        (maybe-emit-encage (nth-value 1 (var-info rest env)) context))
      (when key-p
        (let ((key-names (mapcar #'caar keys)))
          (emit-parse-key-args context max-count key-count key-names env aok-p)
          (dolist (key-name (rest key-names))
            (literal-index key-name context)))
        (setq env (bind-vars (mapcar #'cadar keys) env context)))
      (unless (zerop optional-count)
        (do ((optionals optionals (rest optionals))
             (optional-label (make-label) next-optional-label)
             (next-optional-label (make-label) (make-label)))
            ((null optionals)
             (emit-label context optional-label))
          (emit-label context optional-label)
          (destructuring-bind (optional-var defaulting-form supplied-var)
              (first optionals)
            (setq env
                  (compile-optional/key-item optional-var defaulting-form supplied-var
                                             next-optional-label context env)))))
      (when key-p
        (do ((keys keys (rest keys))
             (key-label (make-label) next-key-label)
             (next-key-label (make-label) (make-label)))
            ((null keys)
             (emit-label context key-label))
          (emit-label context key-label)
          (destructuring-bind ((key-name key-var) defaulting-form supplied-var)
              (first keys)
            (declare (ignore key-name))
            (setq env
                  (compile-optional/key-item key-var defaulting-form supplied-var
                                             next-key-label context env)))))
      (values aux env))))

;;; Compile an optional/key item and return the resulting environment.
(defun compile-optional/key-item (var defaulting-form supplied-var next-label
                                  context env)
  (flet ((default (suppliedp info)
           (cond (suppliedp
                  (maybe-emit-encage info context))
                 (t
                  (compile-form defaulting-form env
                                (new-context context :receiving 1))
                  (maybe-emit-make-cell info context)
                  (assemble context +set+ (lexical-info-frame-offset info)))))
         (supply (suppliedp info)
           (if suppliedp
               (compile-literal t env (new-context context :receiving 1))
               (assemble context +nil+))
           (maybe-emit-make-cell info context)
           (assemble context +set+ (lexical-info-frame-offset info))))
    (let ((supplied-label (make-label))
          (var-info (nth-value 1 (var-info var env))))
      (multiple-value-bind (env supplied-var-info)
          (if supplied-var
              (let ((env (bind-vars (list supplied-var) env context)))
                (values env (nth-value 1 (var-info supplied-var env))))
              (values env nil))
        (emit-jump-if-supplied context (lexical-info-frame-offset var-info) supplied-label)
        (default nil var-info)
        (when supplied-var
          (supply nil supplied-var-info))
        (emit-jump context next-label)
        (emit-label context supplied-label)
        (default t var-info)
        (when supplied-var
          (supply t supplied-var-info))
        env))))

;;; Compile the lambda in MODULE, returning the resulting
;;; CFUNCTION.
(defun compile-lambda (lambda-list body env module)
  (let* ((function (make-cfunction module))
         (context (make-context :receiving t :function function))
         (env (make-lexical-environment env :frame-end 0)))
    (setf (cfunction-index function)
          (vector-push-extend function (cmodule-cfunctions module)))
    (multiple-value-bind (aux-bindings env)
        (compile-lambda-list lambda-list env context)
      (compile-let* aux-bindings body env context))
    (assemble context +return+)
    function))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defun compile-tagbody (statements env context)
  (let* ((new-tags (tags env))
         (tagbody-dynenv (gensym "TAG-DYNENV"))
         (env (bind-vars (list tagbody-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info tagbody-dynenv env))))
    (dolist (statement statements)
      (when (go-tag-p statement)
        (push (list* statement dynenv-info (make-label))
              new-tags)))
    (assemble context +entry+)
    (let ((env (make-lexical-environment env :tags new-tags)))
      ;; Bind the dynamic environment. We don't need a cell as it is
      ;; not mutable.
      (assemble context +set+ (lexical-info-frame-offset dynenv-info))
      ;; Compile the body, emitting the tag destination labels.
      (dolist (statement statements)
        (if (go-tag-p statement)
            (emit-label context (cddr (assoc statement (tags env))))
            (compile-form statement env (new-context context :receiving 0))))))
  (assemble context +entry-close+)
  ;; return nil if we really have to
  (unless (eql (context-receiving context) 0)
    (assemble context +nil+)
    (when (eql (context-receiving context) t)
      (assemble context +pop+))))

(defun compile-go (tag env context)
  (let ((pair (assoc tag (tags env))))
    (if pair
        (destructuring-bind (dynenv-info . tag-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context tag-label))
        (error "The GO tag ~a does not exist." tag))))

(defun compile-block (name body env context)
  (let* ((block-dynenv (gensym "BLOCK-DYNENV"))
         (env (bind-vars (list block-dynenv) env context))
         (dynenv-info (nth-value 1 (var-info block-dynenv env)))
         (label (make-label))
         (normal-label (make-label)))
    (assemble context +entry+)
    ;; Bind the dynamic environment. We don't need a cell as it is
    ;; not mutable.
    (assemble context +set+ (lexical-info-frame-offset dynenv-info))
    (let ((env (make-lexical-environment
                env
                :blocks (acons name (cons dynenv-info label) (blocks env)))))
      (compile-progn body env context))
    (when (eql (context-receiving context) 1)
      (emit-jump context normal-label))
    (emit-label context label)
    ;; When we need 1 value, we have to make sure that the
    ;; "exceptional" case pushes a single value onto the stack.
    (when (eql (context-receiving context) 1)
      (assemble context +push+)
      (emit-label context normal-label))
    (assemble context +entry-close+)))

(defun compile-return-from (name value env context)
  (compile-form value env (new-context context :receiving t))
  (let ((pair (assoc name (blocks env))))
    (if pair
        (destructuring-bind (dynenv-info . block-label) (cdr pair)
          (reference-lexical-info dynenv-info context)
          (emit-exit context block-label))
        (error "The block ~a does not exist." name))))

(defun compile-catch (tag body env context)
  (compile-form tag env (new-context context :receiving 1))
  (let ((target (make-label)))
    (emit-catch context target)
    (compile-progn body env context)
    (assemble context +catch-close+)
    (emit-label context target)))

(defun compile-throw (tag result env context)
  (compile-form tag env (new-context context :receiving 1))
  (compile-form result env (new-context context :receiving t))
  (assemble context +throw+))

(defun compile-progv (symbols values body env context)
  (compile-form symbols env (new-context context :receiving 1))
  (compile-form values env (new-context context :receiving 1))
  (assemble context +progv+)
  (compile-progn body env context)
  (assemble context +unbind+))

(defun compile-symbol-macrolet (bindings body env context)
  (let ((smacros nil))
    (dolist (binding bindings)
      (push (cons (car binding) (make-symbol-macro-var-info (cadr binding)))
            smacros))
    (compile-progn body (make-lexical-environment
                         env
                         :vars (append (nreverse smacros) (vars env)))
                   context)))

(defun lexenv-for-macrolet (env)
  ;; Macrolet expanders need to be compiled in the local compilation environment,
  ;; so that e.g. their bodies can use macros defined in outer macrolets.
  ;; At the same time, they obviously do not have access to any runtime
  ;; environment. Taking out all runtime information is one way to do this but
  ;; it's slightly not-nice in that if someone writes a macroexpander that does
  ;; try to use local runtime information may fail silently by using global info
  ;; instead. So: KLUDGE.
  (make-lexical-environment
   env
   :vars (let ((cpairs nil))
           (dolist (pair (vars env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (member (var-info-kind info) '(:constant :symbol-macro))
                 (push pair cpairs)))))
   :funs (let ((cpairs nil))
           (dolist (pair (funs env) (nreverse cpairs))
             (let ((info (cdr pair)))
               (when (member (fun-info-kind info) '(:global-macro :local-macro))
                 (push pair cpairs)))))
   :tags nil :blocks nil :frame-end 0))

(defun compile-macrolet (bindings body env context)
  (let ((macros nil))
    (dolist (binding bindings)
      (let* ((name (car binding))
             (lambda-list (cadr binding))
             (body (cddr binding))
             (info (make-local-macro-fun-info
                    (compile (trivial-cltl2:parse-macro name lambda-list body)
                             (lexenv-for-macrolet env)))))
        (push (cons name info) macros)))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (funs env)))
                     context)))

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
          (t (assemble context +mv-call-receive-fixed+ receiving)))))

(defun compile-multiple-value-prog1 (first-form forms env context)
  (compile-form first-form env context)
  (unless (member (context-receiving context) '(0 1))
    (assemble context +push-values+))
  (dolist (form forms)
    (compile-form form env (new-context context :receiving 0)))
  (unless (member (context-receiving context) '(0 1))
    (assemble context +pop-values+)))

;;;; linkage

(defun unsigned (x size)
  (logand x (1- (ash 1 size))))

;;; Use the optimistic bytecode vector sizes to initialize the optimistic cfunction position.
(defun initialize-cfunction-positions (cmodule)
  (let ((position 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let ((function (aref (cmodule-cfunctions cmodule) i)))
        (setf (cfunction-position function) position)
        (incf position (length (cfunction-bytecode function)))))))

;;; Update the positions of all affected functions and annotations
;;; from the effect of increasing the size of FIXUP by INCREASE. The
;;; resizer has already updated the size of the the fixup.
(defun update-positions (fixup increase)
  (let ((function (fixup-function fixup)))
    ;; Update affected annotation positions in this function.
    (let ((annotations (cfunction-annotations function)))
      (do ((index (1+ (fixup-index fixup)) (1+ index)))
          ((= index (length annotations)))
        (let ((annotation (aref annotations index)))
          (incf (annotation-position annotation) increase))))
    ;; Increase the size of this function to account for fixup growth.
    (incf (cfunction-extra function) increase)
    ;; Update module offsets for affected functions.
    (let ((functions (cmodule-cfunctions (cfunction-cmodule function))))
      (do ((index (1+ (cfunction-index function)) (1+ index)))
          ((= index (length functions)))
        (let ((function (aref functions index)))
          (incf (cfunction-position function) increase))))))

;;; With all functions and annotations initialized with optimistic
;;; sizes, resize fixups until no more expansion is needed.
(defun resolve-fixup-sizes (cmodule)
  (loop
    (let ((changed-p nil)
          (functions (cmodule-cfunctions cmodule)))
      (dotimes (i (length functions))
        (dotimes (j (length (cfunction-annotations (aref functions i))))
          (let ((annotation (aref (cfunction-annotations (aref functions i)) j)))
            (when (fixup-p annotation)
              (let ((old-size (fixup-size annotation))
                    (new-size (funcall (fixup-resizer annotation) annotation)))
                (unless (= old-size new-size)
                  (assert (>= new-size old-size))
                  (setf (fixup-size annotation) new-size)
                  (setq changed-p t)
                  (update-positions annotation (- new-size old-size))))))))
      (unless changed-p
        (return)))))

;;; The size of the module bytecode vector.
(defun module-bytecode-size (cmodule)
  (let* ((cfunctions (cmodule-cfunctions cmodule))
         (last-cfunction (aref cfunctions (1- (length cfunctions)))))
    (+ (cfunction-position last-cfunction)
       (length (cfunction-bytecode last-cfunction))
       (cfunction-extra last-cfunction))))

;;; Create the bytecode module vector. We scan over the fixups in the
;;; module and copy segments of bytecode between fixup positions.
(defun create-module-bytecode (cmodule)
  (let ((bytecode (make-array (module-bytecode-size cmodule)
                              :element-type '(unsigned-byte 8)))
        (index 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let* ((function (aref (cmodule-cfunctions cmodule) i))
             (cfunction-bytecode (cfunction-bytecode function))
             (position 0))
        (dotimes (i (length (cfunction-annotations function)))
          (let ((annotation (aref (cfunction-annotations function) i)))
            (when (fixup-p annotation)
            (unless (zerop (fixup-size annotation))
              (assert (= (fixup-size annotation)
                         (funcall (fixup-resizer annotation) annotation)))
              ;; Copy bytes in this segment.
              (let ((end (fixup-initial-position annotation)))
                (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
                (incf index (- end position))
                (setf position end))
              (assert (= index (annotation-module-position annotation)))
              ;; Emit fixup.
              (funcall (fixup-emitter annotation)
                       annotation
                       index
                       bytecode)
              (incf position (fixup-initial-size annotation))
              (incf index (fixup-size annotation))))))
        ;; Copy any remaining bytes from this function to the module.
        (let ((end (length cfunction-bytecode)))
          (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
          (incf index (- end position)))))
    bytecode))

;;; Run down the hierarchy and link the compile time representations
;;; of modules and functions together into runtime objects. Return the
;;; bytecode function corresponding to CFUNCTION.
(defun link-function (cfunction)
  (declare (optimize debug))
  (let ((cmodule (cfunction-cmodule cfunction)))
    (initialize-cfunction-positions cmodule)
    (resolve-fixup-sizes cmodule)
    (let* ((cmodule-literals (cmodule-literals cmodule))
           (literal-length (length cmodule-literals))
           (literals (make-array literal-length))
           (bytecode (create-module-bytecode cmodule))
           (bytecode-module
             #-clasp
             (vm::make-bytecode-module
              :bytecode bytecode
              :literals literals)
             #+clasp
             (core:bytecode-module/make)))
      ;; Create the real function objects.
      (dotimes (i (length (cmodule-cfunctions cmodule)))
        (let ((cfunction (aref (cmodule-cfunctions cmodule) i)))
          (setf (cfunction-info cfunction)
                #-clasp
                (vm::make-bytecode-function
                 bytecode-module
                 (cfunction-nlocals cfunction)
                 (length (cfunction-closed cfunction))
                 (annotation-module-position (cfunction-entry-point cfunction)))
                #+clasp
                (core:global-bytecode-entry-point/make
                 (core:function-description/make :function-name 'test)
                 bytecode-module
                 (cfunction-nlocals cfunction)
                 0 0 0 0 nil 0 ; unused at the moment
                 (length (cfunction-closed cfunction))
                 (make-list 7 :initial-element
                            (annotation-module-position (cfunction-entry-point cfunction)))))))
      ;; Now replace the cfunctions in the cmodule literal vector with
      ;; real bytecode functions.
      (dotimes (index literal-length)
        (setf (aref literals index)
              (let ((literal (aref cmodule-literals index)))
                (if (cfunction-p literal)
                    (cfunction-info literal)
                    literal))))
      #+clasp
      (progn
        (core:bytecode-module/setf-literals bytecode-module literals)
        ;; Now just install the bytecode and Bob's your uncle.
        (core:bytecode-module/setf-bytecode bytecode-module bytecode))))
  (cfunction-info cfunction))

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
    (write-string "#ifndef virtualMachine_H" fout) (terpri fout)
    (write-string "#define virtualMachine_H" fout) (terpri fout) (terpri fout)
    (let ((enums (loop for sym in *codes*
                       for index from 0
                       for trimmed-sym-name = (string-downcase (string-trim "+" (symbol-name sym)))
                       for sym-name = (format nil "vm_~a" (c++ify trimmed-sym-name))
                       collect (format nil "~a=~a" sym-name index))))
      (format fout "enum vm_codes {~%~{   ~a~^,~^~%~} };~%" enums))
    (terpri fout)
    (write-string "#endif /*guard */" fout) (terpri fout)))
