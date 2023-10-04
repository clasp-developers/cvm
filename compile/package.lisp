(defpackage #:cvm.compile
  (:use #:cl)
  (:local-nicknames (#:m #:cvm.machine)
                    (#:arg #:cvm.argparse))
  (:shadow #:compile #:eval #:constantp #:macroexpand-1 #:macroexpand
           #:with-compilation-unit)
  (:export #:compile-into #:compile #:eval #:eval-progn)
  (:export #:with-compilation-unit #:with-compilation-results)
  ;; Compiler guts - used in cmpltv
  (:export #:add-specials #:extract-specials #:lexenv-for-macrolet
           #:make-lexical-environment #:make-local-macro #:make-symbol-macro
           #:coerce-to-lexenv #:funs #:vars
           #:compute-macroexpander
           #:macroexpand-1 #:macroexpand)
  (:export #:run-time-environment)
  (:export #:ltv-info #:ltv-info-form #:ltv-info-read-only-p)
  (:export #:fdefinition-info #:fdefinition-info-name)
  (:export #:value-cell-info #:value-cell-info-name)
  (:export #:constant-info #:constant-info-value)
  (:export #:env-info)
  (:export #:cmodule #:make-cmodule #:cmodule-literals #:link)
  (:export #:cfunction #:cfunction-cmodule #:cfunction-nlocals
           #:cfunction-closed #:cfunction-final-entry-point
	   #:cfunction-name
           #:cfunction-lambda-list #:cfunction-doc #:cfunction-final-size)
  ;; Conditions and compilation unit handling
  (:export #:with-compilation-unit #:with-compilation-results)
  (:export #:unknown-reference #:unknown-variable #:unknown-function
           #:name
           #:unknown-reference-resolution #:resolve-reference
           #:resolve-function #:resolve-macro
           #:assumed-function-now-macro))
