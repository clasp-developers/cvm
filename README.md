Lisp implementation of Common Lisp VM used in Clasp and possibly for other purposes.

Specification is at https://github.com/clasp-developers/clasp/wiki/Virtual-machine-design

# Design goals

* Quick compilation of CL code (in more-or-less one pass)
* Reasonably quick execution of the bytecode
* Compatibility with native code, i.e. VM functions can call native functions and vice versa, with practical performance
* Proper involvement of environments, so code can be compiled and/or loaded in artificial environments
* Portability between implementations, i.e. bytecode compiled in one host Lisp can be loaded in any implementation of the VM

# Quick start

Load the `cvm/compile` and `cvm/vm-native` ASDF systems.

Before compiling or evaluating code, you need to set the client in order to inform Trucler how to get global definitions. On SBCL you can use the host environment as follows:

```lisp
(setf cvm.machine:*client* (make-instance 'trucler-native-sbcl:client))
```

The procedure on CCL is analogous. Or, you can use some other trucler client and environment, such as Trucler's reference implementation.

Now you can compile code with `cvm.compile:compile` and disassemble it with `cvm.machine:disassemble`:

```lisp
(defvar *f* (cvm.compile:compile '(lambda (x) (let ((y 5)) (print y) #'(lambda () (+ y x))))))
(cvm.machine:disassemble *f*) ; =>
---module---
  check-arg-count-= 1
  bind-required-args 1
  const '5
  set 1
  fdefinition 'PRINT
  ref 1
  call 1
  ref 1
  ref 0
  make-closure '#<CVM.MACHINE:BYTECODE-FUNCTION {100C2D803B}>
  pop
  return
; No value
```

To actually run code, first set up a stack for the VM with `(cvm.vm-native:initialize-vm N)`, where N is how many objects the stack will be able to hold, say 20000. Then you can simply call the functions returned by `compile`:

```lisp
(funcall *f* 5) ; =>
5
#<CVM.MACHINE:BYTECODE-CLOSURE>
```

You can get a running trace of the machine state by binding `cvm.vm-native:*trace*` to true around a call:

```lisp
(let ((cvm.vm-native:*trace* t)) (funcall *f* 3)) ; =>

((CHECK-ARG-COUNT-= COMMON-LISP:NIL (:OPERAND 1)) 4 6 #(5 0) #())
((BIND-REQUIRED-ARGS COMMON-LISP:NIL (:OPERAND 1)) 4 6 #(5 0) #())
((CONST COMMON-LISP:NIL (:CONSTANT 0)) 4 6 #(3 0) #())
((SET COMMON-LISP:NIL (:OPERAND 1)) 4 7 #(3 0) #(5))
((FDEFINITION COMMON-LISP:NIL (:CONSTANT 1)) 4 6 #(3 5) #())
((REF COMMON-LISP:NIL (:OPERAND 1)) 4 7 #(3 5) #(#<FUNCTION PRINT>))
((CALL COMMON-LISP:NIL (:OPERAND 1)) 4 8 #(3 5) #(#<FUNCTION PRINT> 5))
5
((REF COMMON-LISP:NIL (:OPERAND 1)) 4 6 #(3 5) #())
((REF COMMON-LISP:NIL (:OPERAND 0)) 4 7 #(3 5) #(5))
((MAKE-CLOSURE COMMON-LISP:NIL (:CONSTANT 3)) 4 8 #(3 5) #(5 3))
((POP COMMON-LISP:NIL) 4 7 #(3 5) #(#<BYTECODE-CLOSURE {1002F681CB}>))
((RETURN COMMON-LISP:NIL) 4 6 #(3 5) #())

#<CVM.MACHINE:BYTECODE-CLOSURE {100C2D80CB}>
```

# First-class environments

The `cvm/vm-cross` subsystem allows CVM to be used for compiling and running Lisp code in arbitrary first-class environments, in concert with [Clostrum](https://github.com/s-expressionists/Clostrum). Here is an example:

```lisp
(ql:quickload '(:cvm/compile :cvm/vm-cross))
;;; cvm-cross does not itself load a global environment implementation,
;;; since it can be used with any. Here we use clostrum-basic for that.
;;; We also need clostrum-trucler to be able to compile relative to
;;; a Clostrum environment.
(ql:quickload '(:clostrum-basic :clostrum-trucler))

;;; Set up the client to use cvm-cross, and initialize the VM.
(setf cvm.machine:*client* (make-instance 'cvm.vm-cross:client))
(cvm.vm-cross:initialize-vm 20000)

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

# Subsystems

CVM defines a variety of subsystems that can be loaded independently. It's set up this way so that you can, for example, load one of the VM definitions and run bytecode compiled elsewhere, without needing to load any of the compiler's multitudinous dependencies.

* `cvm` is the base system. Everything depends on CVM. CVM defines various shared conditions, the MOP magic that lets bytecode functions be run in a host Lisp,the names of instructions, and the disassembler.
* `cvm/compile` turns Lisp forms into bytecode. You need it in order to compile or evaluate forms. But this alone won't let you run bytecode; you'll need one of the VM systems for that. And Lisp compilation frequently involves evaluation, so you'll probably need to load a VM before you can compile much of anything.
* `cvm/compile-file` implements the file compiler. It depends on the compiler in `cvm/compile` to do that.
* `cvm/vm-native` is the "native" implementation of the VM, which is to say that it operates entirely in the host Lisp's normal global environment. This is simple but a bit inflexible.
* `cvm/vm-cross` is an implementation of the VM that operates relative to a Clostrum environment. This is what you want to do anything first-class-environment-related.
* `cvm/load` loads FASL files created by `cvm/compile-file`. `cvm/load` and one of the VMs is sufficient to load and run FASLs.

Assuming the compilation and loader environments match (e.g. any function appearing in a macroexpansion is actually available in the load-time environment), there is no problem with compiling code with one VM and loading it with another. Using multiple VMs in the same image also works.

# Implementation status

Works. Except:

* A bespoke environment structure is used rather than host environments. As such, imported host macros that actually access their environment (e.g. `setf`) will not work.

## More TODO

* VM optimizations
  * Profile
  * Values
  * Arguments
  * Maybe elide some array bounds checks when safe, but keep a guard on overflowing the stack
* Compiler optimizations
  * Entirely elide unused blocks (i.e. don't even do `save-sp`)
  * Use `restore-sp` + `jump` for local exits to blocks even if they are also nonlocally exited to
  * Optimize some degenerate forms, like `(block nil)` => `nil`
  * Inline lambda forms
  * Inline functions more generally
  * Contify?
  * Use multiple-value contexts for `multiple-value-call` with a lambda
* Instructions for inline operations like `car`, possibly
* Better syntax errors (required for serious use as a frontend)
