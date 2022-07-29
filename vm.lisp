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
    +jump-if+
    +block-open+ +return-from+ +block-close+
    +tagbody-open+ +go+ +tagbody-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +fdefinition+
    +nil+
    +eq+))

(defmacro assemble (&rest codes)
  `(make-array ,(length codes) :element-type '(unsigned-byte 8)
                               :initial-contents (list ,@codes)))

(defun disassemble (bytecode &key (ip 0) (ninstructions t))
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
                                    +block-close+ +tagbody-close+ +unbind+
                                    +nil+ +eq+)
                       (fixed 0))
                      ((+ref+ +const+ +closure+
                              +call+ +call-receive-one+
                              +set+
                              +go+ +special-bind+ +symbol-value+ +symbol-value-set+
                              +fdefinition+)
                       (fixed 1))
                      ;; These have labels, not integers, as arguments.
                      ;; TODO: Impose labels on the disassembly.
                      ((+jump-if+ +block-open+) (fixed 1))
                      ((+call-receive-fixed+ +bind+ +make-closure+) (fixed 2))
                      ((+tagbody-open+)
                       (let ((ntags (aref bytecode (incf ip))))
                         (prog1 (list* op ntags (loop repeat ntags
                                                      collect (aref bytecode (incf ip))))
                           (incf ip)))))))))

(defstruct (cell (:constructor make-cell (value))) value)

(defun make-closure (bytecode stack-size closure constants)
  (declare (type (and fixnum (integer 0)) stack-size))
  (lambda (&rest args)
    ;; set up the stack, then call vm
    (loop with stack = (make-array stack-size)
          for sp from 0
          for arg in args
          do (setf (aref stack sp) arg)
          finally (return (vm bytecode stack closure constants :sp sp)))))

(defvar *trace* nil)

(defstruct dynenv)
(defstruct (block-dynenv (:include dynenv)
                         (:constructor make-block-dynenv (fun)))
  (fun (error "missing arg") :type function))
(defstruct (tagbody-dynenv (:include dynenv)
                           (:constructor make-tagbody-dynenv (fun)))
  (fun (error "missing arg") :type function))
(defstruct (sbind-dynenv (:include dynenv)
                         (:constructor make-sbind-dynenv ())))

(defvar *dynenv* nil)

(defun vm (bytecode stack closure constants &key (ip 0) (sp 0) (bp sp) &aux (mv nil))
  (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
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
             (svref constants index))
           (closure (index)
             ;;(declare (optimize (safety 0)))
             (svref closure index))
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
            do (print (list (first (disassemble bytecode :ip ip :ninstructions 1))
                            (subseq stack 0 bp)
                            (subseq stack bp sp)))
          do (ecase (code)
               ((#.+ref+) (spush (stack (- bp (next-code) 1))) (incf ip))
               ((#.+const+) (spush (constant (next-code))) (incf ip))
               ((#.+closure+) (spush (closure (next-code))) (incf ip))
               ((#.+call+)
                (setf mv (multiple-value-list
                          (apply (the function (spop)) (gather (next-code)))))
                (incf ip))
               ((#.+call-receive-one+)
                (spush (apply (the function (spop)) (gather (next-code))))
                (incf ip))
               ;; TODO
               #+(or)((#.+call-receive-fixed+))
               ((#.+bind+)
                (loop repeat (next-code)
                      for bsp downfrom (- bp (next-code) 1)
                      do (setf (stack bsp) (spop)))
                (incf ip))
               ((#.+set+)
                (setf (stack (- bp (next-code) 1)) (spop))
                (incf ip))
               ((#.+make-cell+) (spush (make-cell (spop))) (incf ip))
               ((#.+cell-ref+) (spush (cell-value (spop))) (incf ip))
               ((#.+cell-set+)
                (let ((val (spop))) (setf (cell-value (spop)) val))
                (incf ip))
               ((#.+make-closure+)
                (let ((closure-size (next-code)))
                  (destructuring-bind (cbytecode stack-size cconstants)
                      (constant (next-code))
                    (spush (make-closure cbytecode stack-size
                                         (coerce (gather closure-size) 'simple-vector)
                                         cconstants))))
                (incf ip))
               ((#.+return+)
                (return (values-list (if (eql bp sp) mv (gather (- sp bp))))))
               ((#.+jump-if+)
                (setf ip (if (spop) (next-code) (+ 2 ip))))
               ((#.+block-open+)
                (setf mv
                      (block nil
                        (let ((*dynenv*
                                (make-block-dynenv
                                 (lambda (mv) (return (values-list mv))))))
                          (spush *dynenv*)
                          (multiple-value-list
                           (vm bytecode stack closure constants
                               ;; +2 to skip over this instruction, including the label
                               :ip (+ 2 ip) :sp sp :bp bp))))
                      ip (next-code)))
               ((#.+return-from+) (funcall (block-dynenv-fun (spop)) mv))
               ((#.+block-close+) (return (values-list mv)))
               ((#.+tagbody-open+)
                (let ((next-ip (+ ip (next-code) 2)) ; the tagbody's prefix
                      (*dynenv* *dynenv*))
                  (tagbody
                     (setf *dynenv*
                           (make-tagbody-dynenv
                            (lambda (n new-sp)
                              (setf next-ip (acode (+ ip n 1)) sp new-sp)
                              (go loop))))
                     (spush *dynenv*)
                   loop
                     (setf (values ip sp)
                           (vm bytecode stack closure constants
                               :ip next-ip :sp sp :bp bp)))))
               ((#.+go+) (funcall (tagbody-dynenv-fun (spop)) (next-code) sp))
               ((#.+tagbody-close+) (return (values (1+ ip) sp)))
               ((#.+special-bind+)
                (let ((*dynenv* (make-sbind-dynenv)))
                  (progv (list (constant (next-code))) (list (spop))
                    (setf ip
                          (vm bytecode stack closure constants :ip ip :sp sp :bp bp)))))
               ((#.+symbol-value+) (symbol-value (constant (next-code))) (incf ip))
               ((#.+symbol-value-set+)
                (setf (symbol-value (constant (next-code))) (spop))
                (incf ip))
               ((#.+unbind+) (return (1+ ip)))
               ((#.+fdefinition+) (spush (fdefinition (constant (next-code)))) (incf ip))
               ((#.+nil+) (spush nil) (incf ip))
               ((#.+eq+) (spush (eq (spop) (spop))) (incf ip))))))
