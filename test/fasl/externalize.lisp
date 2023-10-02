(in-package #:cvm.test)

(5am:def-suite externalize :in fasl)
(5am:in-suite externalize)

;;; Make a fresh environment containing only SETQ, QUOTE, *SOURCE*,
;;; and *COMPILED*. The file will be compiled there.
(defun make-clean-compilation-environment (object)
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (clostrum:make-special-operator *client* rte 'setq t)
    (clostrum:make-special-operator *client* rte 'quote t)
    (clostrum:make-parameter *client* rte '*source* object)
    ;; FIXME: make-variable's value should be optional. Oopsie.
    (clostrum:make-variable *client* rte '*compiled* nil)
    ce))

;;; Make a fresh environment containing just *COMPILED*.
(defun make-clean-load-environment ()
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (clostrum:make-variable *client* rte '*compiled* nil)
    ce))

(defun compile-test-file (input-file &rest keys &key &allow-other-keys)
  ;; Check that warning behavior matches return values.
  (let ((signaled-warnings-p nil) (signaled-failure-p nil))
    (multiple-value-call
        (lambda (&optional output warningsp (failurep nil 3vp)
                 &rest extra)
          (5am:is-true 3vp
                       "<3 values returned from ~s" 'compile-file)
          (5am:is (null extra)
                  "Extra values returned from ~s" 'compile-file)
          (5am:is (eql signaled-failure-p (not (not failurep)))
                  "~s signaled failure but didn't return failurep"
                  'compile-file)
          (5am:is (eql signaled-warnings-p (not (not warningsp)))
                  "~s signaled warnings but didn't return warningsp"
                  'compile-file)
          (values output failurep warningsp))
      (handler-bind ((warning (lambda (c)
                                (declare (ignore c))
                                (setq signaled-warnings-p t)))
                     ((or error (and warning (not style-warning)))
                       (lambda (c)
                         (declare (ignore c))
                         (setq signaled-failure-p t))))
        (apply #'cvm.compile-file:compile-file input-file keys)))))

(defun test-externalize (object)
  (with-open-file (s "externalization-test-file.lisp"
                     :direction :output :if-exists :supersede)
    (write-line "(setq *compiled* '#.*source*)" s))
  (multiple-value-bind (output failurep warningsp)
      (compile-test-file "externalization-test-file.lisp"
                         :environment (make-clean-compilation-environment object))
    (5am:is-false failurep "Compilation failed")
    (5am:is-false warningsp "Compilation signaled warnings")
    (let* ((load-env (make-clean-load-environment))
           (load-rte
             (clostrum:evaluation-environment *client* load-env)))
      (cvm.load:load-bytecode output load-rte)
      (let ((c (cvm.compile:eval '*compiled* load-env *client*)))
        (5am:is (similarp object c) "~s externalization was unsimilar")))))

;;; List of objects to test externalization of.
(defparameter *externalization-universe*
  '(12 #.(* 3 most-positive-fixnum)
    #-sbcl #p"SYS:SRC;CORE;LISPLIST.CC"
    #-sbcl #p"/usr/share/Adobe/doc/example/android_vm/root/sbin/ls.jar"
    -17 0 #.(- (* most-negative-fixnum 7) 8)
    4/3 -3/2 7.6s0 3.2f-4 4.1d8 5.6l23 #c(3.2 4.7) #c(-8 3/2)
    #\Newline #\7 dribble #:make-load-form #:nonexist
    #.*package* (machine-instance . 223) #1=(#1# (#1# . #1#) . #1#)
    #3=(#2=(#3# #2#) #1# . #2#) "Hello, world!" #*11010110101
    #0a17 #4=#0a#4# #5=#2a((#5# #4#) (#3# #5#))
    #.(make-array '(3 2) :element-type 'standard-char
       :initial-contents '((#\f #\7) (#\Space #\\) (#\. #\%)))
    #.(make-array '(3 3 3) :adjustable t)
    ;; This constant crashes SBCL - see SBCL bug 2038233
    #-sbcl #.(make-array 2 :displaced-to '#5#)
    #.(make-array 7 :element-type '(unsigned-byte 3) :fill-pointer 2)
    #(2 #:blue #'standard-object "who?" #2a((0 1) (1 0)))))

(5am:test externalize-objects
  (mapc #'test-externalize *externalization-universe*)
  ;; Plus.
  (test-externalize *externalization-universe*)
  (test-externalize (let ((o (copy-list *externalization-universe*)))
                      (nconc o o)))
  ;; FIXME: dunno about other implementations
  #+sbcl(5am:skip "Can't externalize SBCL pathnames as the hosts are bespoke and unexternalizable"))
