(defpackage #:cvm.cross.vm
  (:use #:cl)
  (:local-nicknames (#:m #:cvm.machine))
  (:export #:initialize-vm)
  (:export #:*trace*)
  (:export #:symbol-cell))

(in-package #:cvm.cross.vm)

(define-condition wrong-number-of-arguments (program-error)
  ((%called-function :initform nil :initarg :called-function
                     :reader called-function)
   (%given-nargs :initarg :given-nargs :reader given-nargs)
   (%min-nargs :initarg :min-nargs :reader min-nargs :initform nil)
   (%max-nargs :initarg :max-nargs :reader max-nargs :initform nil))
  (:report (lambda (condition stream)
             (let* ((min (min-nargs condition))
                    (max (max-nargs condition))
                    ;; FIXME: get an actual name if possible
                    (dname nil))
               (format stream "~@[Calling ~a - ~]Got ~d arguments, but expected ~@?"
                       dname (given-nargs condition)
                       (cond ((null max)  "at least ~d")
                             ((null min)  "at most ~*~d")
                             ;; I think "exactly 0" is better than "at most 0", thus duplication
                             ((= min max) "exactly ~d")
                             ((zerop min) "at most ~*~d")
                             (t           "between ~d and ~d"))
                       min max)))))

(define-condition odd-keywords (program-error)
  ((%called-function :initarg :called-function :reader called-function
                     :initform nil))
  (:report (lambda (condition stream)
             (format stream "Odd number of keyword arguments~:[~; for ~s~]."
                     (called-function condition)
                     ;; FIXME: again, get an actual name somehow.
                     nil))))

(define-condition unrecognized-keyword-argument (program-error)
  ((%called-function :initarg :called-function :reader called-function
                     :initform nil)
   (%unrecognized-keywords :initarg :unrecognized-keywords
                           :reader unrecognized-keywords))
  (:report (lambda (condition stream)
             (format stream "Unrecognized keyword arguments ~S~:[~; for ~S~]."
                     (unrecognized-keywords condition)
                     (called-function condition) ; FIXME: name
                     nil))))

(defstruct vm
  (values nil :type list)
  (stack #() :type simple-vector)
  (stack-top 0 :type (and unsigned-byte fixnum))
  (frame-pointer 0 :type (and unsigned-byte fixnum))
  (args 0 :type (and unsigned-byte fixnum))
  (arg-count 0 :type (and unsigned-byte fixnum))
  (pc 0 :type (and unsigned-byte fixnum))
  (dynenv-stack nil :type list))

(defvar *vm*)
(declaim (type vm *vm*))

(defvar *trace* nil)

(defstruct (cell (:constructor make-cell (value))) value)
(defstruct unbound-marker)
(defvar *unbound* (make-unbound-marker))

(defstruct dynenv)
(defstruct (entry-dynenv (:include dynenv)
                         (:constructor make-entry-dynenv (tag)))
  (tag (error "missing arg")))
(defstruct (sbind-dynenv (:include dynenv)
                         (:constructor %make-sbind-dynenv (symbol cell)))
  symbol cell)

;;; For uniformity, we put a Clostrum-style cell into these structs.
(defun make-sbind-dynenv (symbol value)
  (%make-sbind-dynenv symbol (cons value *unbound*)))

(defun bytecode-call (template closure-env args)
  (declare (optimize speed)
           (type list args))
  (let* ((entry-pc (m:bytecode-function-entry-pc template))
         (frame-size (m:bytecode-function-locals-frame-size template))
         (module (m:bytecode-function-module template))
         (bytecode (m:bytecode-module-bytecode module))
         (literals (m:bytecode-module-literals module)))
    (declare (type (unsigned-byte 16) frame-size))
    ;; Set up the stack, then call VM.
    (let* ((vm *vm*)
           (stack (vm-stack vm)))
      (setf (vm-args vm) (vm-stack-top vm))
      ;; Pass the argments on the stack.
      (dolist (arg args)
        (setf (aref stack (vm-stack-top vm)) arg)
        (incf (vm-stack-top vm)))
      (setf (vm-arg-count vm) (length args))
      ;; Save the previous frame pointer and pc
      (let ((old-fp (vm-frame-pointer vm))
            (old-pc (vm-pc vm))
            (old-de-stack (vm-dynenv-stack vm)))
        (setf (vm-frame-pointer vm) (vm-stack-top vm))
        (setf (vm-pc vm) entry-pc)
        (setf (vm-stack-top vm) (+ (vm-frame-pointer vm) frame-size))
        ;; set up the stack, then call vm
        (unwind-protect
             (vm bytecode closure-env literals frame-size)
          (setf (vm-dynenv-stack vm) old-de-stack))
        ;; tear down the frame.
        (setf (vm-stack-top vm) (- (vm-frame-pointer vm) (length args)))
        (setf (vm-frame-pointer vm) old-fp)
        (setf (vm-pc vm) old-pc))
      (values-list (vm-values vm)))))

(defun initialize-vm (stack-size)
  (setf *vm*
        (make-vm :stack (make-array stack-size)
                 :frame-pointer 0
                 :stack-top 0))
  (values))

(declaim (inline signed))
(defun signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun %find-sbind-dynenv (symbol stack)
  (dolist (de stack)
    (when (eq symbol (sbind-dynenv-symbol de))
      (return de))))

(defun symbol-cell (symbol global-cell)
  (let* ((de (%find-sbind-dynenv symbol (vm-dynenv-stack *vm*))))
    (if de
        (sbind-dynenv-cell de)
        global-cell)))

(defun %symbol-value (symbol global-cell)
  (let* ((cell (symbol-cell symbol global-cell))
         (value (car cell)))
    (if (eq value (cdr cell))
        (error 'unbound-variable :name symbol)
        value)))

(defun (setf %symbol-value) (new symbol global-cell)
  (let ((cell (symbol-cell symbol global-cell)))
    (setf (car cell) new)))

(defvar *dynenv* nil)

(defun instruction-trace (bytecode stack ip bp sp frame-size)
  (fresh-line *trace-output*)
  (let ((frame-end (+ bp frame-size))
        ;; skip package prefixes on inst names.
        (*package* (find-package "CVM/MACHINE")))
    (prin1 (list (m:disassemble-instruction bytecode ip)
                 bp
                 sp
                 (subseq stack bp frame-end)
                 ;; We take the max for partial frames.
                 (subseq stack frame-end (max sp frame-end)))
           *trace-output*)))

(defun vm (bytecode closure constants frame-size)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
           (type (simple-array t (*)) closure constants)
           (type (unsigned-byte 16) frame-size)
           (optimize debug #+(or) speed))
  (let* ((vm *vm*)
         (stack (vm-stack vm))
         (ip (vm-pc vm))
         (sp (vm-stack-top vm))
         (bp (vm-frame-pointer vm)))
    (declare (type (simple-array t (*)) stack)
             (type (and unsigned-byte fixnum) ip sp bp))
    (labels ((stack (index)
               ;;(declare (optimize (safety 0))) ; avoid bounds check
               (svref stack index))
             ((setf stack) (object index)
               ;;(declare (optimize (safety 0)))
               ;; I do not understand SBCL's complaint, so
               #+sbcl
               (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
               (setf (svref stack index) object))
             (spush (object)
               (prog1 (setf (stack sp) object) (incf sp)))
             (spop () (stack (decf sp)))
             (code ()
               ;;(declare (optimize (safety 0)))
               (aref bytecode ip))
             (next-code ()
               ;;(declare (optimize safety 0))
               (aref bytecode (incf ip)))
             (next-code-signed ()
               (signed (aref bytecode (+ ip 1)) 8))
             (next-code-signed-16 ()
               (signed (+ (aref bytecode (+ ip 1))
                          (ash (aref bytecode (+ ip 2)) 8))
                       16))
             (next-code-signed-24 ()
               (signed (+ (aref bytecode (+ ip 1))
                          (ash (aref bytecode (+ ip 2)) 8)
                          (ash (aref bytecode (+ ip 3)) 16))
                       24))
             (constant (index)
               ;;(declare (optimize (safety 0)))
               (aref constants index))
             (closure (index)
               ;;(declare (optimize (safety 0)))
               (aref closure index))
             (gather (n)
               (declare (type (unsigned-byte 16) n))
               (let ((result nil)) ; put the most recent value on the end
                 (loop repeat n do (push (spop) result))
                 result)))
      (declare (inline stack (setf stack) spush spop
                       code next-code constant closure))
      (prog ((end (length bytecode))
             (trace *trace*)
             ;; KLUDGE: we can't use bp directly since catch uses eq.
             (tag (list bp)))
       loop
         (when (>= ip end)
           (error "Invalid bytecode: Reached end"))
         (when trace
           (instruction-trace bytecode stack ip bp sp frame-size))
         (setf ip
               (catch tag
                 (ecase (code)
                   ((#.m:ref) (spush (stack (+ bp (next-code))))
                    (incf ip))
                   ((#.m:const) (spush (constant (next-code))) (incf ip))
                   ((#.m:closure) (spush (closure (next-code))) (incf ip))
                   ((#.m:call)
                    (setf (vm-values vm)
                          (multiple-value-list
                           (let ((args (gather (next-code)))
                                 (callee (spop)))
                             (declare (type function callee))
                             (setf (vm-stack-top vm) sp)
                             (apply callee args))))
                    (incf ip))
                   ((#.m:call-receive-one)
                    (spush (let ((args (gather (next-code)))
                                 (callee (spop)))
                             (declare (type function callee))
                             (setf (vm-stack-top vm) sp)
                             (apply callee args)))
                    (incf ip))
                   ((#.m:call-receive-fixed)
                    (let ((args (gather (next-code))) (mvals (next-code))
                          (fun (spop)))
                      (declare (function fun))
                      (setf (vm-stack-top vm) sp)
                      (case mvals
                        ((0) (apply fun args))
                        (t (mapcar #'spush (subseq (multiple-value-list (apply fun args))
                                                   0 mvals)))))
                    (incf ip))
                   ((#.m:bind)
                    ;; Most recent push goes to the last local.
                    (let ((nvars (next-code)))
                      (loop repeat nvars
                            for bsp downfrom (+ bp (next-code) nvars -1)
                            do (setf (stack bsp) (spop))))
                    (incf ip))
                   ((#.m:set)
                    (setf (stack (+ bp (next-code))) (spop))
                    (incf ip))
                   ((#.m:make-cell) (spush (make-cell (spop))) (incf ip))
                   ((#.m:cell-ref) (spush (cell-value (spop))) (incf ip))
                   ((#.m:cell-set)
                    (setf (cell-value (spop)) (spop))
                    (incf ip))
                   ((#.m:make-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              m:*client*
                              template
                              (coerce (gather
                                       (m:bytecode-function-environment-size template))
                                      'simple-vector))))
                    (incf ip))
                   ((#.m:make-uninitialized-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              m:*client*
                              template
                              (make-array
                               (m:bytecode-function-environment-size template)))))
                    (incf ip))
                   ((#.m:initialize-closure)
                    (let ((env (m:bytecode-closure-env (stack (+ bp (next-code))))))
                      (declare (type simple-vector env))
                      (loop for i from (1- (length env)) downto 0 do
                        (setf (aref env i) (spop))))
                    (incf ip))
                   ((#.m:return)
                    ;; Assert that all temporaries are popped off..
                    (locally
                        ;; SBCL complains that ASSERT is inefficient.
                        #+sbcl(declare (sb-ext:muffle-conditions
                                        sb-ext:compiler-note))
                        (assert (eql sp (+ bp frame-size))))
                    (return))
                   ((#.m:jump-8) (incf ip (next-code-signed)))
                   ((#.m:jump-16) (incf ip (next-code-signed-16)))
                   ((#.m:jump-24) (incf ip (next-code-signed-24)))
                   ((#.m:jump-if-8) (incf ip (if (spop) (next-code-signed) 2)))
                   ((#.m:jump-if-16) (incf ip (if (spop) (next-code-signed-16) 3)))
                   ((#.m:jump-if-24) (incf ip (if (spop) (next-code-signed-24) 4)))
                   ((#.m:check-arg-count-<=)
                    (let ((n (next-code)))
                      (unless (<= (vm-arg-count vm) n)
                        (error 'wrong-number-of-arguments
                               :given-nargs (vm-arg-count vm)
                               :max-nargs n)))
                    (incf ip))
                   ((#.m:check-arg-count->=)
                    (let ((n (next-code)))
                      (unless (>= (vm-arg-count vm) n)
                        (error 'wrong-number-of-arguments
                               :given-nargs (vm-arg-count vm)
                               :min-nargs n)))
                    (incf ip))
                   ((#.m:check-arg-count-=)
                    (let ((n (next-code)))
                      (unless (= (vm-arg-count vm) n)
                        (error 'wrong-number-of-arguments
                               :given-nargs (vm-arg-count vm)
                               :min-nargs n :max-nargs n)))
                    (incf ip))
                   ((#.m:jump-if-supplied-8)
                    (incf ip (if (typep (stack (+ bp (next-code))) 'unbound-marker)
                                 2
                                 (1- (next-code-signed)))))
                   ((#.m:jump-if-supplied-16)
                    (incf ip (if (typep (stack (+ bp (next-code))) 'unbound-marker)
                                 3
                                 (1- (next-code-signed-16)))))
                   ((#.m:bind-required-args)
                    ;; Use memcpy for this.
                    (let* ((args (vm-args vm))
                           (args-end (+ args (next-code))))
                      (do ((arg-index args (1+ arg-index))
                           (frame-slot bp (1+ frame-slot)))
                          ((>= arg-index args-end))
                        (setf (stack frame-slot) (stack arg-index))))
                    (incf ip))
                   ((#.m:bind-optional-args)
                    (let* ((args (vm-args vm))
                           (required-count (next-code))
                           (optional-start (+ args required-count))
                           (optional-count (next-code))
                           (args-end (+ args (vm-arg-count vm)))
                           (end (+ optional-start optional-count))
                           (optional-frame-offset (+ bp required-count))
                           (optional-frame-end (+ optional-frame-offset optional-count)))
                      (if (<= args-end end)
                          ;; Could be coded as memcpy in C.
                          (do ((arg-index optional-start (1+ arg-index))
                               (frame-slot optional-frame-offset (1+ frame-slot)))
                              ((>= arg-index args-end)
                               ;; memcpy or similar. (blit bit
                               ;; pattern?)
                               (do ((frame-slot frame-slot (1+ frame-slot)))
                                   ((>= frame-slot optional-frame-end))
                                 (setf (stack frame-slot) (make-unbound-marker))))
                            (setf (stack frame-slot) (stack arg-index)))
                          ;; Could also be coded as memcpy.
                          (do ((arg-index optional-start (1+ arg-index))
                               (frame-slot optional-frame-offset (1+ frame-slot)))
                              ((>= arg-index end))
                            (setf (stack frame-slot) (stack arg-index))))
                      (incf ip)))
                   ((#.m:listify-rest-args)
                    (spush (loop for index from (next-code) below (vm-arg-count vm)
                                 collect (stack (+ (vm-args vm) index))))
                    (incf ip))
                   ((#.m:parse-key-args)
                    (let* ((args (vm-args vm))
                           (end (+ args (vm-arg-count vm)))
                           (more-start (+ args (next-code)))
                           (key-count-info (next-code))
                           (key-count (logand key-count-info #x7f))
                           (key-literal-start (next-code))
                           (key-literal-end (+ key-literal-start key-count))
                           (key-frame-start (+ bp (next-code)))
                           (unknown-keys nil)
                           (allow-other-keys-p nil))
                      ;; Initialize all key values to #<unbound-marker>
                      (loop for index from key-frame-start below (+ key-frame-start key-count)
                            do (setf (stack index) (make-unbound-marker)))
                      (when (> end more-start)
                        (do ((arg-index (- end 1) (- arg-index 2)))
                            ((< arg-index more-start)
                             (cond ((= arg-index (1- more-start)))
                                   ((= arg-index (- more-start 2))
                                    (error 'odd-keywords))
                                   (t
                                    (error "BUG! This can't happen!"))))
                          (let ((key (stack (1- arg-index))))
                            (when (eq key :allow-other-keys)
                              (setf allow-other-keys-p (stack arg-index)))
                            (loop for key-index from key-literal-start
                                    below key-literal-end
                                  for offset of-type (unsigned-byte 16)
                                  from key-frame-start
                                  do (when (eq (constant key-index) key)
                                       (setf (stack offset) (stack arg-index))
                                       (return))
                                  finally (unless (or allow-other-keys-p
                                                      (eq key :allow-other-keys))
                                            (push key unknown-keys))))))
                      (when (and (not (or (logbitp 7 key-count-info)
                                          allow-other-keys-p))
                                 unknown-keys)
                        (error 'unrecognized-keyword-argument
                               :unrecognized-keywords unknown-keys)))
                    (incf ip))
                   ((#.m:save-sp)
                    (setf (stack (+ bp (next-code))) sp)
                    (incf ip))
                   ((#.m:restore-sp)
                    (setf sp (stack (+ bp (next-code))))
                    (incf ip))
                   ((#.m:entry)
                    (let ((de (make-entry-dynenv tag)))
                      (push de (vm-dynenv-stack vm))
                      (setf (stack (+ bp (next-code))) de)
                      (incf ip)))
                   #+(or)
                   ((#.m:catch-8)
                    (let ((target (+ ip (next-code-signed) 1))
                          (tag (spop))
                          (old-sp sp)
                          (old-bp bp))
                      (incf ip)
                      (catch tag
                        (vm bytecode closure constants frame-size))
                      (setf ip target)
                      (setf sp old-sp)
                      (setf bp old-bp)))
                   #+(or)
                   ((#.m:catch-16)
                    (let ((target (+ ip (next-code-signed-16) 1))
                          (tag (spop))
                          (old-sp sp)
                          (old-bp bp))
                      (incf ip)
                      (catch tag
                        (vm bytecode closure constants frame-size))
                      (setf ip target)
                      (setf sp old-sp)
                      (setf bp old-bp)))
                   #+(or)
                   ((#.m:throw) (throw (spop) (values)))
                   #+(or)
                   ((#.m:catch-close)
                    (incf ip)
                    (return))
                   ((#.m:exit-8)
                    (incf ip (next-code-signed))
                    (throw (entry-dynenv-tag (spop)) ip))
                   ((#.m:exit-16)
                    (incf ip (next-code-signed-16))
                    (throw (entry-dynenv-tag (spop)) ip))
                   ((#.m:exit-24)
                    (incf ip (next-code-signed-24))
                    (throw (entry-dynenv-tag (spop)) ip))
                   ((#.m:entry-close)
                    (pop (vm-dynenv-stack vm))
                    (incf ip))
                   ((#.m:special-bind)
                    (let ((de (make-sbind-dynenv
                               (car (constant (next-code))) (spop))))
                      (push de (vm-dynenv-stack vm)))
                    (incf ip))
                   ((#.m:symbol-value)
                    (let ((vcell (constant (next-code))))
                      (spush (%symbol-value (car vcell) (cdr vcell))))
                    (incf ip))
                   ((#.m:symbol-value-set)
                    (let ((vcell (constant (next-code))))
                      (setf (%symbol-value (car vcell) (cdr vcell))
                            (spop)))
                    (incf ip))
                   #+(or)
                   ((#.m:progv)
                    (let ((values (spop)))
                      (progv (spop) values
                        (incf ip)
                        (vm bytecode closure constants frame-size))))
                   ((#.m:unbind)
                    (pop (vm-dynenv-stack vm))
                    (incf ip))
                   ((#.m:push-values)
                    (dolist (value (vm-values vm))
                      (spush value))
                    (spush (length (vm-values vm)))
                    (incf ip))
                   ((#.m:append-values)
                    (let ((n (spop)))
                      (declare (type (and unsigned-byte fixnum) n))
                      (dolist (value (vm-values vm))
                        (spush value))
                      (spush (+ n (length (vm-values vm))))
                      (incf ip)))
                   ((#.m:pop-values)
                    (setf (vm-values vm) (gather (spop)))
                    (incf ip))
                   ((#.m:mv-call)
                    (setf (vm-stack-top vm) sp
                          (vm-values vm)
                          (multiple-value-list
                           (apply (the function (spop)) (vm-values vm))))
                    (incf ip))
                   ((#.m:mv-call-receive-one)
                    (setf (vm-stack-top vm) sp)
                    (spush (apply (the function (spop)) (vm-values vm)))
                    (incf ip))
                   ((#.m:mv-call-receive-fixed)
                    (let ((args (vm-values vm))
                          (mvals (next-code))
                          (fun (spop)))
                      (declare (function fun))
                      (setf (vm-stack-top vm) sp)
                      (case mvals
                        ((0) (apply fun args))
                        (t (mapcar #'spush (subseq (multiple-value-list (apply fun args))
                                                   0 mvals)))))
                    (incf ip))
                   ((#.m:fdefinition)
                    (spush (car (constant (next-code)))) (incf ip))
                   ((#.m:nil) (spush nil) (incf ip))
                   ((#.m:eq) (spush (eq (spop) (spop))) (incf ip))
                   ((#.m:pop) (setf (vm-values vm) (list (spop))) (incf ip))
                   ((#.m:push) (spush (first (vm-values vm))) (incf ip))
                   ((#.m:long)
                    (ecase (next-code)
                      (#.m:const
                       (spush (constant (+ (next-code) (ash (next-code) 8))))
                       (incf ip)))))
                 (go loop)))))))

(defmethod m:compute-instance-function ((client cvm.cross:client)
                                        (closure m:bytecode-closure))
  (let ((template (m:bytecode-closure-template closure))
        (env (m:bytecode-closure-env closure)))
    (lambda (&rest args)
      (bytecode-call template env args))))

(defmethod m:compute-instance-function ((client cvm.cross:client)
                                        (fun m:bytecode-function))
  (lambda (&rest args)
    (bytecode-call fun #() args)))
