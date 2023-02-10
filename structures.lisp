(in-package #:cvm/machine)

(defstruct bytecode-module
  bytecode
  literals)

(defclass bytecode-closure ()
  ((%template :initarg :template :accessor bytecode-closure-template)
   (%env :initarg :env :accessor bytecode-closure-env))
  (:metaclass closer-mop:funcallable-standard-class))

(defun make-bytecode-closure (template env)
  (make-instance 'bytecode-closure :template template :env env))

(defclass bytecode-function ()
  ((%module :initarg :module :accessor bytecode-function-module)
   (%locals-frame-size :initarg :locals-frame-size :accessor bytecode-function-locals-frame-size)
   (%environment-size :initarg :environment-size :accessor bytecode-function-environment-size)
   (%entry-pc :initarg :entry-pc :accessor bytecode-function-entry-pc))
  (:metaclass closer-mop:funcallable-standard-class))

(defun make-bytecode-function (module locals-frame-size environment-size entry-pc)
  (make-instance 'bytecode-function
                 :module module
                 :locals-frame-size locals-frame-size
                 :environment-size environment-size
                 :entry-pc entry-pc))
