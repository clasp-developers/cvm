Prototype for a Common Lisp VM to be used in Clasp and possibly for other purposes.

Specification is at https://github.com/clasp-developers/clasp/wiki/Virtual-machine-design

# Design goals

* Quick compilation of CL code (in more-or-less one pass)
* Reasonably quick execution of the bytecode
* Compatibility with native code, i.e. VM functions can call native functions and vice versa, with practical performance

Possibly also to let function stack frames be of compile-time-known size.

# Implementation status

## VM

Handles all the specified instructions plus an FDEFINITION instruction (which I think is required for sensitivity to redefined functions).

CALL-etc and MAKE-CLOSURE are considered to work with the leftmost argument closer to the bottom of the stack, which is not totally clear to me in the spec. This is so that when compiling a call, the leftmost argument can be compiled first, and its bytecode pushes something; then the second argument is compiled and its bytecode pushes something; etc., and then the call is executed.

## Compiler

Accepts only a small subset of Common Lisp: No macros, no declarations, no lambda lists except for required parameters, and the only available special operators are `progn let function`. Doesn't compute total stack space needed.

In the example in the VM spec, also seen below, the inner lambda has a closure vector of (Y X) instead of (X Y) as in the example. This is because straight-line compilation sees and references Y first.

# Example use

First, compile and load vm.lisp and compile.lisp.

```lisp
(defvar *bytecode-function* (compile-to-vm::compile '(lambda (x) (let ((y 5)) (print y) #'(lambda () (+ y x))))))

(vm::initialize-vm 2048)

(vm::disassemble (vm::bytecode-function-module *bytecode-function*))
#|
((VM::+CLOSURE+ 0) (VM::+CELL-REF+) (VM::+CLOSURE+ 1) (VM::+CELL-REF+)
 (VM::+FDEFINITION+ 2) (VM::+CALL+ 2) (VM::+RETURN+) (VM::+REF+ 0)
 (VM::+MAKE-CELL+) (VM::+SET+ 0) (VM::+CONST+ 0) (VM::+MAKE-CELL+)
 (VM::+BIND+ 1 1) (VM::+REF+ 1) (VM::+CELL-REF+) (VM::+FDEFINITION+ 1)
 (VM::+CALL-RECEIVE-FIXED+ 1 0) (VM::+REF+ 1) (VM::+REF+ 0)
 (VM::+MAKE-CLOSURE+ 3) (VM::+RETURN+))
|#

(let ((vm::*trace* t)) (vm::apply* *bytecode-function* '(7)))
#|
((VM::+REF+ 0) #(7 0) #())
((VM::+MAKE-CELL+) #(7 0) #(7))
((VM::+SET+ 0) #(7 0) #(#S(VM::CELL :VALUE 7)))
((VM::+CONST+ 0) #(#S(VM::CELL :VALUE 7) 0) #())
((VM::+MAKE-CELL+) #(#S(VM::CELL :VALUE 7) 0) #(5))
((VM::+BIND+ 1 1) #(#S(VM::CELL :VALUE 7) 0) #(#S(VM::CELL :VALUE 5)))
((VM::+REF+ 1) #(#S(VM::CELL :VALUE 7) #S(VM::CELL :VALUE 5)) #())
((VM::+CELL-REF+) #(#S(VM::CELL :VALUE 7) #1=#S(VM::CELL :VALUE 5)) #(#1#))
((VM::+FDEFINITION+ 1) #(#S(VM::CELL :VALUE 7) #S(VM::CELL :VALUE 5)) #(5))
((VM::+CALL-RECEIVE-FIXED+ 1 0) #(#S(VM::CELL :VALUE 7) #S(VM::CELL :VALUE 5))
 #(5 #<FUNCTION PRINT>))
5 
((VM::+REF+ 1) #(#S(VM::CELL :VALUE 7) #S(VM::CELL :VALUE 5)) #())
((VM::+REF+ 0) #(#S(VM::CELL :VALUE 7) #1=#S(VM::CELL :VALUE 5)) #(#1#))
((VM::+MAKE-CLOSURE+ 3) #(#1=#S(VM::CELL :VALUE 7) #2=#S(VM::CELL :VALUE 5))
 #(#2# #1#))
((VM::+RETURN+) #(#S(VM::CELL :VALUE 7) #S(VM::CELL :VALUE 5))
 #(#<FUNCTION (LAMBDA (&REST VM::ARGS) :IN VM::MAKE-CLOSURE) {10066812BB}>))
#<FUNCTION (LAMBDA (&REST VM::ARGS) :IN VM::MAKE-CLOSURE) {10066812BB}>
|#

(let ((vm::*trace* t)) (vm::apply* * '()))
#|
((VM::+CLOSURE+ 0) #(0 0) #())
((VM::+CELL-REF+) #(0 0) #(#S(VM::CELL :VALUE 5)))
((VM::+CLOSURE+ 1) #(0 0) #(5))
((VM::+CELL-REF+) #(0 0) #(5 #S(VM::CELL :VALUE 7)))
((VM::+FDEFINITION+ 2) #(0 0) #(5 7))
((VM::+CALL+ 2) #(0 0) #(5 7 #<FUNCTION +>))
((VM::+RETURN+) #(0 0) #())
12
|#
```
