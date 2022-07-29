(defpackage #:vm
  (:use #:cl))

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
    +nil+
    +eq+))

(defmacro assemble (&rest codes)
  `(make-array ,(length codes) :element-type '(unsigned-byte 8)
               :initial-contents (list ,@codes)))

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

(defun vm (bytecode stack closure constants &key (ip 0) (sp 0) (mv nil) &aux (bp sp))
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
           (constant (index)
             ;;(declare (optimize (safety 0)))
             (svref constants index))
           (closure (index)
             ;;(declare (optimize (safety 0)))
             (svref closure index))
           (gather (n)
             (loop repeat n collecting (spop))))
    #+(or)
    (declare (inline stack (setf stack) spush spop
                     code next-code constant closure))
    (loop with end = (length bytecode)
          until (eql ip end)
          when *trace*
            do (print (list (decode (code)) stack sp))
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
               ((#.+make-cell+) (spush (make-cell nil)) (incf ip))
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
               ((#.+nil+) (spush nil) (incf ip))
               ((#.+eq+) (spush (eq (spop) (spop))) (incf ip))))))
