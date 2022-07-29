Prototype for a Common Lisp VM to be used in Clasp and possibly for other purposes.

Specification is at https://github.com/clasp-developers/clasp/wiki/Virtual-machine-design

# Design goals

* Quick compilation of CL code (in more-or-less one pass)
* Reasonably quick execution of the bytecode
* Compatibility with native code, i.e. VM functions can call native functions and vice versa, with practical performance

Possibly also to let function stack frames be of compile-time-known size.

# Implementation status

## VM

Handles all the specified instructions plus an FDEFINITION instruction (which I think is required for sensitivity to redefined functions). MAKE-CELL works slightly differently from the spec by popping a value to use as the initial value of the cell. I have not tested the NLX instructions.

CALL-etc and MAKE-CLOSURE are considered to work with the leftmost argument closer to the bottom of the stack, which is not totally clear to me in the spec. This is so that when compiling a call, the leftmost argument can be compiled first, and its bytecode pushes something; then the second argument is compiled and its bytecode pushes something; etc., and then the call is executed.

## Compiler

Accepts only a small subset of Common Lisp: No macros, no declarations, no lambda lists except for required parameters, and the only available special operators are `progn let function`. Doesn't compute stack frame size.

In the example in the VM spec, also seen below, the inner lambda has a closure vector of (Y X) instead of (X Y) as in the example. This is because straight-line compilation sees and references Y first.

# Example use

First, compile and load vm.lisp and compile.lisp.

```lisp
(defvar *proto* (compile-to-vm::compile '(lambda (x) (let ((y 5)) (print y) #'(lambda () (+ y x)))) nil))

(vm::disassemble (compile-to-vm::function-prototype-bytecode *proto*))
#|
((VM::+REF+ 0) (VM::+MAKE-CELL+) (VM::+SET+ 0) (VM::+CONST+ 0)
 (VM::+MAKE-CELL+) (VM::+BIND+ 1 1) (VM::+REF+ 1) (VM::+CELL-REF+)
 (VM::+FDEFINITION+ 1) (VM::+CALL+ 1) (VM::+REF+ 1) (VM::+REF+ 0)
 (VM::+MAKE-CLOSURE+ 2 2) (VM::+RETURN+))
|#

(let ((vm::*trace* t)) (vm::vm (compile-to-vm::function-prototype-bytecode *proto*) #(nil 7 nil nil nil) #() (compile-to-vm::function-prototype-constants *proto*) :sp (compile-to-vm::function-prototype-nlocals *proto*)))
#|
((VM::+REF+ 0) #(NIL 7) #()) 
((VM::+MAKE-CELL+) #(NIL 7) #(7)) 
((VM::+SET+ 0) #(NIL 7) #(#S(VM::CELL :VALUE 7))) 
((VM::+CONST+ 0) #(NIL #S(VM::CELL :VALUE 7)) #()) 
((VM::+MAKE-CELL+) #(NIL #S(VM::CELL :VALUE 7)) #(5)) 
((VM::+BIND+ 1 1) #(NIL #S(VM::CELL :VALUE 7)) #(#S(VM::CELL :VALUE 5))) 
((VM::+REF+ 1) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7)) #()) 
((VM::+CELL-REF+) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7))
 #(#S(VM::CELL :VALUE 5))) 
((VM::+FDEFINITION+ 1) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7)) #(5)) 
((VM::+CALL+ 1) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7))
 #(5 #<FUNCTION PRINT>)) 
5 
((VM::+REF+ 1) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7)) #()) 
((VM::+REF+ 0) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7))
 #(#S(VM::CELL :VALUE 5))) 
((VM::+MAKE-CLOSURE+ 2 2) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7))
 #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7)))   0: (VM::MAKE-CLOSURE #(2 0 9 2 1 9 24 0 3 2 12) 10 #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7)) #(+))
  0: VM::MAKE-CLOSURE returned
       #<FUNCTION (LAMBDA (&REST VM::ARGS) :IN VM::MAKE-CLOSURE) {10035F254B}>

((VM::+RETURN+) #(#S(VM::CELL :VALUE 5) #S(VM::CELL :VALUE 7))
 #(#<FUNCTION (LAMBDA (&REST VM::ARGS) :IN VM::MAKE-CLOSURE) {10035F254B}>)) 
#<FUNCTION (LAMBDA (&REST VM::ARGS) :IN VM::MAKE-CLOSURE) {10035F254B}>
|#

(let ((vm::*trace* t)) (funcall *))
|#
((VM::+CLOSURE+ 0) #() #()) 
((VM::+CELL-REF+) #() #(#S(VM::CELL :VALUE 5))) 
((VM::+CLOSURE+ 1) #() #(5)) 
((VM::+CELL-REF+) #() #(5 #S(VM::CELL :VALUE 7))) 
((VM::+FDEFINITION+ 0) #() #(5 7)) 
((VM::+CALL+ 2) #() #(5 7 #<FUNCTION +>)) 
((VM::+RETURN+) #() #()) 
12
#|
```
