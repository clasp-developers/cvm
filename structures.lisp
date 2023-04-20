(in-package #:cvm/machine)

(defvar *client*)

(defstruct bytecode-module
  bytecode
  literals)

(defclass bytecode-closure ()
  ((%template :initarg :template :accessor bytecode-closure-template)
   (%env :initarg :env :accessor bytecode-closure-env))
  (:metaclass closer-mop:funcallable-standard-class))

(defgeneric compute-instance-function (client function))

(defun make-bytecode-closure (client template env)
  (let ((clos
          (make-instance 'bytecode-closure :template template :env env)))
    (closer-mop:set-funcallable-instance-function
     clos (compute-instance-function client clos))
    clos))

(defclass bytecode-function ()
  ((%module :initarg :module :accessor bytecode-function-module)
   (%locals-frame-size :initarg :locals-frame-size :accessor bytecode-function-locals-frame-size)
   (%environment-size :initarg :environment-size :accessor bytecode-function-environment-size)
   (%entry-pc :initarg :entry-pc :accessor bytecode-function-entry-pc)
   (%size :initarg :size :accessor bytecode-function-size))
  (:metaclass closer-mop:funcallable-standard-class))

(defun make-bytecode-function (client module locals-frame-size environment-size entry-pc size)
  (let ((fun (make-instance 'bytecode-function
               :module module
               :locals-frame-size locals-frame-size
               :environment-size environment-size
               :entry-pc entry-pc
               :size size)))
    (closer-mop:set-funcallable-instance-function
     fun (compute-instance-function client fun))
    fun))
