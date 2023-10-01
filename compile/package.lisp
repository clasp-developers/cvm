(defpackage #:cvm.compile
  (:use #:cl)
  (:local-nicknames (#:m #:cvm.machine)
                    (#:arg #:cvm.argparse))
  (:shadow #:compile #:eval #:constantp)
  (:export #:compile-into #:compile #:eval)
  ;; Compiler guts - used in cmpltv
  (:export #:add-specials #:extract-specials #:lexenv-for-macrolet
           #:make-lexical-environment #:make-local-macro #:make-symbol-macro
           #:coerce-to-lexenv #:funs #:vars
           #:compute-macroexpander)
  (:export #:run-time-environment)
  (:export #:var-info #:fun-info #:expand #:symbol-macro-expansion)
  (:export #:ltv-info #:ltv-info-form #:ltv-info-read-only-p)
  (:export #:fdefinition-info #:fdefinition-info-name)
  (:export #:value-cell-info #:value-cell-info-name)
  (:export #:constant-info #:constant-info-value)
  (:export #:env-info)
  (:export #:cmodule #:make-cmodule #:cmodule-literals #:link)
  (:export #:cfunction #:cfunction-cmodule #:cfunction-nlocals
           #:cfunction-closed #:cfunction-entry-point #:cfunction-name
           #:cfunction-lambda-list #:cfunction-doc #:cfunction-final-size
           #:annotation-module-position))
