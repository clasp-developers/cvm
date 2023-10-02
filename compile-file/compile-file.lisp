(in-package #:cvm.compile-file)

;;; Main external entry points: COMPILE-STREAM and COMPILE-FILE.

;; Print information about a form for *compile-print*.
(defun describe-form (form)
  (fresh-line)
  (write-string ";   ")
  (write form :length 2 :level 2 :lines 1 :pretty nil)
  (terpri)
  (values))

;; input is a character stream. output is a ub8 stream.
(defun compile-stream (input output
                       &key environment (reader-client *reader-client*)
                       &allow-other-keys)
  (with-constants ()
    ;; Read and compile the forms.
    (loop with env = (cmp:coerce-to-lexenv environment)
          with eof = (gensym "EOF")
          with *compile-time-too* = nil
          with *environment* = environment
          with eclector.base:*client* = reader-client
          for form = (eclector.reader:read input nil eof)
          until (eq form eof)
          when *compile-print*
            do (describe-form form)
          do (compile-toplevel form env))
    ;; Write out the FASO bytecode.
    (write-bytecode (reverse *instructions*) output)))

;;; TODO?: This is not a full compile-file - it returns different values
;;; and is not good at handling errors, etc.
(defun compile-file (input-file
                     &rest keys
                     &key (output-file nil ofp) (external-format :default)
                       ((:verbose *compile-verbose*) *compile-verbose*)
                       ((:print *compile-print*) *compile-print*)
                       environment (reader-client *reader-client*)
                     &allow-other-keys)
  (declare (ignore environment reader-client)) ; passed to compile-stream
  (let ((output-file (if ofp
                         output-file
                         (make-pathname :type "faslbc" :defaults input-file))))
    (with-open-file (in input-file :external-format external-format)
      (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
        (apply #'compile-stream in out keys)))
    (values output-file nil nil)))
