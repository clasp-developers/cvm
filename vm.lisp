(defpackage #:cvm/vm
  (:use #:cl)
  (:local-nicknames (#:m #:cvm/machine))
  (:export #:initialize-vm))

(in-package #:cvm/vm)

(defstruct vm
  values
  stack
  stack-top
  frame-pointer
  closure
  literals
  args
  arg-count
  pc)

(defvar *vm*)

(declaim (type vm *vm*))

(defun bytecode-call (template env args)
  (let* ((entry-pc (m:bytecode-function-entry-pc template))
         (frame-size (m:bytecode-function-locals-frame-size template))
         (module (m:bytecode-function-module template))
         (bytecode (m:bytecode-module-bytecode module))
         (literals (m:bytecode-module-literals module)))
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
            (old-pc (vm-pc vm)))
        (setf (vm-frame-pointer vm) (vm-stack-top vm))
        (setf (vm-pc vm) entry-pc)
        (setf (vm-stack-top vm) (+ (vm-frame-pointer vm) frame-size))
        ;; set up the stack, then call vm
        (vm bytecode env literals frame-size)
        ;; tear down the frame.
        (setf (vm-stack-top vm) (vm-frame-pointer vm))
        (setf (vm-frame-pointer vm) old-fp)
        (setf (vm-pc vm) old-pc))
      (values-list (vm-values vm)))))

(defun initialize-vm (stack-size)
  (setf *vm*
        (make-vm :stack (make-array stack-size)
                 :frame-pointer 0
                 :stack-top 0))
  (values))

(defun signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defstruct (cell (:constructor make-cell (value))) value)
(defstruct (unbound-marker (:constructor make-unbound-marker)))

(defvar *trace* nil)

(defstruct dynenv)
(defstruct (entry-dynenv (:include dynenv)
                         (:constructor make-entry-dynenv (fun)))
  (fun (error "missing arg") :type function))
(defstruct (sbind-dynenv (:include dynenv)
                         (:constructor make-sbind-dynenv ())))

(defvar *dynenv* nil)

(defun vm (bytecode closure constants frame-size)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
           (type (simple-array t (*)) closure constants)
           (optimize debug))
  (let* ((vm *vm*)
         (stack (vm-stack *vm*)))
    (declare (type (simple-array t (*)) stack))
    (symbol-macrolet ((ip (vm-pc vm))
                      (sp (vm-stack-top vm))
                      (bp (vm-frame-pointer vm)))
      (labels ((stack (index)
                 ;;(declare (optimize (safety 0))) ; avoid bounds check
                 (svref stack index))
               ((setf stack) (object index)
                 ;;(declare (optimize (safety 0)))
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
                 (let ((result nil)) ; put the most recent value on the end
                   (loop repeat n do (push (spop) result))
                   result)))
        #+(or)
        (declare (inline stack (setf stack) spush spop
                         code next-code constant closure))
        (loop with end = (length bytecode)
              until (eql ip end)
              when *trace*
                do (fresh-line)
                   (let ((frame-end (+ bp frame-size)))
                     (prin1 (list (disassemble-instruction bytecode :ip ip)
                                  bp
                                  sp
                                  (subseq stack bp frame-end)
                                  ;; We take the max for partial frames.
                                  (subseq stack frame-end (max sp frame-end)))))
              do (ecase (code)
                   ((#.m:ref) (spush (stack (+ bp (next-code)))) (incf ip))
                   ((#.m:const) (spush (constant (next-code))) (incf ip))
                   ((#.m:closure) (spush (closure (next-code))) (incf ip))
                   ((#.m:call)
                    (setf (vm-values vm)
                          (multiple-value-list
                           (let ((args (gather (next-code))))
                             (apply (spop) args))))
                    (incf ip))
                   ((#.m:call-receive-one)
                    (spush (let ((args (gather (next-code))))
                             (apply (spop) args)))
                    (incf ip))
                   ((#.m:call-receive-fixed)
                    (let ((args (gather (next-code))) (mvals (next-code))
                          (fun (spop)))
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
                              template
                              (coerce (gather
                                       (m:bytecode-function-environment-size template))
                                      'simple-vector))))
                    (incf ip))
                   ((#.m:make-uninitialized-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              template
                              (make-array
                               (m:bytecode-function-environment-size template)))))
                    (incf ip))
                   ((#.m:initialize-closure)
                    (let ((env (m:bytecode-closure-env (stack (+ bp (next-code))))))
                      (loop for i from (1- (length env)) downto 0 do
                        (setf (aref env i) (spop))))
                    (incf ip))
                   ((#.m:return)
                    ;; Assert that all temporaries are popped off..
                    (assert (eql sp (+ bp frame-size)))
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
                        (error "Invalid number of arguments: Got ~d, need at most ~d."
                               (vm-arg-count vm) n)))
                    (incf ip))
                   ((#.m:check-arg-count->=)
                    (let ((n (next-code)))
                      (unless (>= (vm-arg-count vm) n)
                        (error "Invalid number of arguments: Got ~d, need at least ~d."
                               (vm-arg-count vm) n)))
                    (incf ip))
                   ((#.m:check-arg-count-=)
                    (let ((n (next-code)))
                      (unless (= (vm-arg-count vm) n)
                        (error "Invalid number of arguments: Got ~d, need exactly ~d."
                               (vm-arg-count vm) n)))
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
                           (unknown-key-p nil)
                           (allow-other-keys-p nil))
                      ;; Initialize all key values to #<unbound-marker>
                      (loop for index from key-frame-start below (+ key-frame-start key-count)
                            do (setf (stack index) (make-unbound-marker)))
                      (when (> end more-start)
                        (do ((arg-index (- end 1) (- arg-index 2)))
                            ((< arg-index more-start)
                             (cond ((= arg-index (1- more-start)))
                                   ((= arg-index (- more-start 2))
                                    (error "Passed odd number of &KEY args!"))
                                   (t
                                    (error "BUG! This can't happen!"))))
                          (let ((key (stack (1- arg-index))))
                            (if (eq key :allow-other-keys)
                                (setf allow-other-keys-p (stack arg-index))
                                (loop for key-index from key-literal-start below key-literal-end
                                      for offset from key-frame-start
                                      do (when (eq (constant key-index) key)
                                           (setf (stack offset) (stack arg-index))
                                           (return))
                                      finally (setf unknown-key-p key))))))
                      (when (and (not (or (logbitp 7 key-count-info)
                                          allow-other-keys-p))
                                 unknown-key-p)
                        (error "Unknown key arg ~a!" unknown-key-p)))
                    (incf ip))
                   ((#.m:save-sp)
                    (setf (stack (+ bp (next-code))) sp)
                    (incf ip))
                   ((#.m:restore-sp)
                    (setf sp (stack (+ bp (next-code))))
                    (incf ip))
                   ((#.m:entry)
                    (let ((*dynenv* *dynenv*))
                      (tagbody
                         (setf *dynenv*
                               (make-entry-dynenv
                                (let ((old-sp sp)
                                      (old-bp bp))
                                  (lambda ()
                                    (setf sp old-sp
                                          bp old-bp)
                                    (go loop)))))
                         (setf (stack (+ bp (next-code))) *dynenv*)
                         (incf ip)
                       loop
                         (vm bytecode closure constants frame-size))))
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
                   ((#.m:throw) (throw (spop) (values)))
                   ((#.m:catch-close)
                    (incf ip)
                    (return))
                   ((#.m:exit-8)
                    (incf ip (next-code-signed))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:exit-16)
                    (incf ip (next-code-signed-16))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:exit-24)
                    (incf ip (next-code-signed-24))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:entry-close)
                    (incf ip)
                    (return))
                   ((#.m:special-bind)
                    (let ((*dynenv* (make-sbind-dynenv)))
                      (progv (list (constant (next-code))) (list (spop))
                        (incf ip)
                        (vm bytecode closure constants frame-size))))
                   ((#.m:symbol-value)
                    (spush (symbol-value (constant (next-code))))
                    (incf ip))
                   ((#.m:symbol-value-set)
                    (setf (symbol-value (constant (next-code))) (spop))
                    (incf ip))
                   ((#.m:progv)
                    (let ((*dynenv* (make-sbind-dynenv))
                          (values (spop)))
                      (progv (spop) values
                        (incf ip)
                        (vm bytecode closure constants frame-size))))
                   ((#.m:unbind)
                    (incf ip)
                    (return))
                   ((#.m:push-values)
                    (dolist (value (vm-values vm))
                      (spush value))
                    (spush (length (vm-values vm)))
                    (incf ip))
                   ((#.m:append-values)
                    (let ((n (spop)))
                      (dolist (value (vm-values vm))
                        (spush value))
                      (spush (+ n (length (vm-values vm))))
                      (incf ip)))
                   ((#.m:pop-values)
                    (setf (vm-values vm) (gather (spop)))
                    (incf ip))
                   ((#.m:mv-call)
                    (setf (vm-values vm)
                          (multiple-value-list
                           (apply (spop) (vm-values vm))))
                    (incf ip))
                   ((#.m:mv-call-receive-one)
                    (spush (apply (spop) (vm-values vm)))
                    (incf ip))
                   ((#.m:mv-call-receive-fixed)
                    (let ((args (vm-values vm))
                          (mvals (next-code))
                          (fun (spop)))
                      (case mvals
                        ((0) (apply fun args))
                        (t (mapcar #'spush (subseq (multiple-value-list (apply fun args))
                                                   0 mvals)))))
                    (incf ip))
                   ((#.m:fdefinition) (spush (fdefinition (constant (next-code)))) (incf ip))
                   ((#.m:nil) (spush nil) (incf ip))
                   ((#.m:eq) (spush (eq (spop) (spop))) (incf ip))
                   ((#.m:pop) (setf (vm-values vm) (list (spop))) (incf ip))
                   ((#.m:push) (spush (first (vm-values vm))) (incf ip))
                   ((#.m:long)
                    (ecase (next-code)
                      (#.m:const
                       (spush (constant (+ (next-code) (ash (next-code) 8))))
                       (incf ip))))))))))

(defmethod initialize-instance :after ((c m:bytecode-closure) &key)
  (let ((template (m:bytecode-closure-template c))
        (env (m:bytecode-closure-env c)))
    (closer-mop:set-funcallable-instance-function
      c
      (lambda (&rest args)
        (bytecode-call template env args)))))

(defmethod initialize-instance :after ((fun m:bytecode-function) &key)
  (closer-mop:set-funcallable-instance-function
   fun
   (lambda (&rest args)
     (bytecode-call fun #() args))))
