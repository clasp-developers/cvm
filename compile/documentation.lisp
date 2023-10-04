(in-package #:cvm.compile)

(setf (documentation (find-package "CVM.COMPILE") t)
      "Package for Lisp compiler and evaluator. Main entry points are COMPILE and EVAL."
      (documentation 'eval 'function)
      "As CL:EVAL. Evaluate FORM in the compilation ENVIRONMENT and the current dynamic environment."
      (documentation 'eval-progn 'function)
      "Evaluate FORMS as a progn in the compilation ENVIRONMENT. This is provided as a convenience function, because it works regardless of how CL:PROGN is bound in ENVIRONMENT, if at all."
      (documentation 'compile 'function)
      "Compile LAMBDA-EXPRESSION into a function in the compilation ENVIRONMENT.
Returns three values: FUNCTION, WARNINGSP, FAILUREP, which are the same as those of CL:COMPILE. FUNCTION may or may not be considered a COMPILED-FUNCTION by the host - this is out of CVM's control.
Unlike CL:COMPILE, COMPILE does not accept a function name, and does not alter bindings to install the newly compiled function."
      (documentation 'compile-into 'function)
      "Compile LAMBDA-EXPRESSION in the compilation ENVIRONMENT into a new compiler function within MODULE. The compiler function information is returned, but is not loaded into being an actual callable function. COMPILE-INTO underlies all other compilation operations and can be used for lower level purposes, such as getting a compiler function to serialize during COMPILE-FILE.
If FORMS-ONLY is provided, the body of the lambda expression is treated as if it was in a PROGN, so declaration expressions are not permitted (and will be treated as forms). If BLOCK-NAME is provided, a block of that name will be established around the body of the lambda expression. These options are available so that functions can be compiled with implicit blocks or progns regardless of the compilation environment's bindings of CL:PROGN or CL:BLOCK."
      (documentation 'link 'function)
      "Finish compilation of a module by resolving all temporary labels into actual positions. Return the final bytecode for the module."
      (documentation 'with-compilation-results 'function)
      "Evaluate BODY as a progn, and then return values analogous to those returned by COMPILE. Specifically, three values are returned:
* the primary value of BODY
* warningsp, which is true iff evaluation of BODY signaled any unhandled warnings
* failurep, which is true iff evaluation of BODY signaled any unhandled warnings or errors, other than style warnings
COnditions may be handled from outside the body of WITH-COMPILATION-RESULTS. Handling a warning with MUFFLE-WARNING is not sufficient to prevent WITH-COMPILATION-RESULTS from noting it: you would need a different restart."
      (documentation 'with-compilation-unit 'function)
      "As CL:WITH-COMPILATION-UNIT. Evaluate BODY as a progn and return its values. Within the dynamic environment of WITH-COMPILATION-UNIT, actions deferred by the compiler until the end of compilation will be deferred to the end of the outermost WITH-COMPILATION-FORM (or the innermost with :override true).
On exit, this outermost WITH-COMPILATION-FORM will print to *ERROR-OUTPUT* a summary of conditions signaled during evaluation, if there were any.
For now in CVM, these actions are only signaling certain kinds of warnings relating to unknown names."
      (documentation 'run-time-environment 'function)
      "The compiler calls this function to get the run time environment for a given compilation environment. Clients should customize it in accordance with their environments' structures."
      (documentation 'resolve-reference 'function)
      "This function can be called by WITH-COMPILATION-UNIT's expansion when it receives an UNKNOWN-REFERENCE-RESOLUTION signal. The function is called with the resolution condition and any unresolved condition. If the resolution resolves the given condition, T should be returned. Other effects are permissible, such as signaling warnings.")
