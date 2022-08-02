(defpackage #:vm
  (:use #:cl)
  (:shadow #:disassemble))

(in-package #:vm)

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

;;; The VM objects we need.

(defstruct bytecode-module
  bytecode
  literals)

(defstruct bytecode-closure
  template
  env)

(defstruct bytecode-function
  module
  locals-frame-size
  environment-size
  ;; Should become entry-pcs once we have lambda list processing.
  entry-pc)

(defmacro assemble (&rest codes)
  `(make-array ,(length codes) :element-type '(signed-byte 8)
                               :initial-contents (list ,@codes)))

(defun disassemble-bytecode (bytecode &key (ip 0) (ninstructions t))
  (loop with blen = (length bytecode)
        for ndis from 0
        until (or (= ip blen) (and (integerp ninstructions) (eql ndis ninstructions)))
        collect (macrolet ((fixed (n)
                             `(prog1
                                  (list op ,@(loop repeat n
                                                   collect `(aref bytecode (incf ip))))
                                (incf ip))))
                  (let ((op (decode (aref bytecode ip))))
                    (ecase op
                      ((+make-cell+ +cell-ref+ +cell-set+
                                    +return+
                                    +entry+
                                    +entry-close+ +unbind+
                                    +nil+ +eq+)
                       (fixed 0))
                      ((+ref+ +const+ +closure+
                              +call+ +call-receive-one+
                              +set+ +make-closure+
                              +exit+ +special-bind+ +symbol-value+ +symbol-value-set+
                              +fdefinition+)
                       (fixed 1))
                      ;; These have labels, not integers, as arguments.
                      ;; TODO: Impose labels on the disassembly.
                      ((+jump+ +jump-if+) (fixed 1))
                      ((+call-receive-fixed+ +bind+) (fixed 2)))))))

(defun disassemble (bytecode-module)
  (disassemble-bytecode (bytecode-module-bytecode bytecode-module)))

(defstruct (cell (:constructor make-cell (value))) value)

(defun make-closure (bytecode-closure)
  (declare (type (and fixnum (integer 0))))
  (let* ((template (bytecode-closure-template bytecode-closure))
         (entry-pc (bytecode-function-entry-pc template))
         (frame-size (bytecode-function-locals-frame-size template))
         (module (bytecode-function-module template))
         (bytecode (bytecode-module-bytecode module))
         (literals (bytecode-module-literals module))
         (closure (bytecode-closure-env bytecode-closure)))
    (lambda (&rest args)
      ;; set up the stack, then call vm
      (loop with stack = (make-array 100) ; FIXME
            for sp from 0
            for arg in args
            do (setf (aref stack sp) arg)
            finally (return (vm bytecode stack closure literals frame-size :sp frame-size :bp 0 :ip entry-pc))))))

(defvar *trace* nil)

(defstruct dynenv)
(defstruct (entry-dynenv (:include dynenv)
                         (:constructor make-entry-dynenv (fun)))
  (fun (error "missing arg") :type function))
(defstruct (sbind-dynenv (:include dynenv)
                         (:constructor make-sbind-dynenv ())))

(defvar *dynenv* nil)

(defun vm (bytecode stack closure constants frame-size &key (ip 0) (sp 0) (bp sp) (mv nil))
  (declare (type (simple-array (signed-byte 8) (*)) bytecode)
           (type (simple-array t (*)) stack closure constants)
           (type (and fixnum (integer 0)) start sp bp)
           (optimize debug))
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
           (acode (ip)
             ;;(declare (optimize (safety 0)))
             (aref bytecode ip))
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
                     code next-code acode constant closure))
    (loop with end = (length bytecode)
          until (eql ip end)
          when *trace*
            do (fresh-line)
               (prin1 (list (first (disassemble-bytecode bytecode :ip ip :ninstructions 1))
                            (subseq stack 0 frame-size)
                            ;; We take the max for partial frames.
                            (subseq stack frame-size (max sp frame-size))))
          do (ecase (code)
               ((#.+ref+) (spush (stack (+ bp (next-code)))) (incf ip))
               ((#.+const+) (spush (constant (next-code))) (incf ip))
               ((#.+closure+) (spush (closure (next-code))) (incf ip))
               ((#.+call+)
                (setf mv (multiple-value-list
                          (apply (the function (spop)) (gather (next-code)))))
                (incf ip))
               ((#.+call-receive-one+)
                (spush (apply (the function (spop)) (gather (next-code))))
                (incf ip))
               ((#.+call-receive-fixed+)
                (let ((fun (the function (spop)))
                      (args (gather (next-code))) (mvals (next-code)))
                  (case mvals
                    ((0) (apply fun args))
                    (t (mapcar #'spush (subseq (multiple-value-list (apply fun args))
                                               0 mvals)))))
                (incf ip))
               ((#.+bind+)
                (loop repeat (next-code)
                      for bsp from (next-code)
                      do (setf (stack bsp) (spop)))
                (incf ip))
               ((#.+set+)
                (setf (stack (+ bp (next-code))) (spop))
                (incf ip))
               ((#.+make-cell+) (spush (make-cell (spop))) (incf ip))
               ((#.+cell-ref+) (spush (cell-value (spop))) (incf ip))
               ((#.+cell-set+)
                (let ((val (spop))) (setf (cell-value (spop)) val))
                (incf ip))
               ((#.+make-closure+)
                (spush (make-closure
                        (let ((template (constant (next-code))))
                          (make-bytecode-closure
                           :template template
                           :env (coerce (gather
                                         (bytecode-function-environment-size template))
                                        'simple-vector)))))
                (incf ip))
               ((#.+return+)
                (return (values-list (if (eql sp frame-size) mv (gather (- sp frame-size))))))
               ((#.+jump+) (incf ip (next-code)))
               ((#.+jump-if+)
                (incf ip (if (spop) (next-code) (+ 2 ip))))
               ((#.+entry+)
                (let ((*dynenv* *dynenv*))
                  (incf ip)
                  (tagbody
                     (setf *dynenv*
                           (make-entry-dynenv
                             (let ((old-sp sp)
                                   (old-bp bp))
                               (lambda (next-ip new-mv)
                                 (setf ip next-ip
                                       mv new-mv
                                       sp old-sp
                                       bp old-bp)
                                 (go loop)))))
                     (spush *dynenv*)
                   loop
                     (setf (values ip sp mv)
                           (vm bytecode stack closure constants frame-size
                               :ip ip :sp sp :bp bp :mv mv)))))
               ((#.+exit+)
                (funcall (entry-dynenv-fun (spop)) (+ ip 1 (next-code)) mv))
               ((#.+entry-close+) (return (values (1+ ip) sp mv)))
               ((#.+special-bind+)
                (let ((*dynenv* (make-sbind-dynenv)))
                  (progv (list (constant (next-code))) (list (spop))
                    (setf ip
                          (vm bytecode stack closure constants frame-size :ip ip :sp sp :bp bp)))))
               ((#.+symbol-value+) (symbol-value (constant (next-code))) (incf ip))
               ((#.+symbol-value-set+)
                (setf (symbol-value (constant (next-code))) (spop))
                (incf ip))
               ((#.+unbind+) (return (1+ ip)))
               ((#.+fdefinition+) (spush (fdefinition (constant (next-code)))) (incf ip))
               ((#.+nil+) (spush nil) (incf ip))
               ((#.+eq+) (spush (eq (spop) (spop))) (incf ip))))))
