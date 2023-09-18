This system allows CVM to be used for compiling and running Lisp code in arbitrary first-class environments, in concert with [Clostrum](https://github.com/s-expressionists/Clostrum). Here is an example:

```lisp
(ql:quickload :cvm-cross)
;;; cvm-cross does not itself load a global environment implementation,
;;; since it can be used with any. Here we use clostrum-basic for that.
(ql:quickload :clostrum-basic)

;;; Set up the client to use cvm-cross, and initialize the VM.
(setf cvm.machine:*client* (make-instance 'cvm.cross:client))
(cvm.cross.vm:initialize-vm 20000)

;;; Construct our environments.
(defvar *rte* (make-instance 'clostrum-basic:run-time-environment))
(defvar *env* (make-instance 'clostrum-basic:compilation-environment
                 :parent *rte*))

;;; These new environments are totally devoid of bindings.
;;; To do anything useful, we have to define at least a few.
;;; In this simple example, we will define CL:PROGN since it is used
;;; by CVM.COMPILE:EVAL.
(clostrum:make-special-operator cvm.machine:*client* *rte* 'progn t)

;;; We'll define + and *readtable* weirdly to emphasize that we are
;;; not operating in the host environment.
(setf (clostrum:fdefinition cvm.machine:*client* *rte* '+) #'-)
(clostrum:make-variable cvm.machine:*client* *rte* '*readtable* 17)

;;; Now behold:
(cvm.compile:eval '(+ *readtable* 3) *env*) ; => 14
;;; And of course, the host *READTABLE* and + are unaffected.
```

Implementation note: The cross VM is separate from the main one a directory up because it is by nature uncooperative with the implementation - for example host bindings are ignored by the VM and VM bindings are ignored by the host. But they share almost all of their code, so in the future we should probably work out a cleverer solution.
