(asdf:defsystem #:cvm
  :description "Reference implementation of the CVM bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.5.0"
  :depends-on (:closer-mop ; machine
               :alexandria :trucler :ecclesia ; compiler
               :ieee-floats) ; compile-file
  :components ((:file "machine")
               (:file "arg-conditions")
               (:file "parse-macro" :depends-on ("arg-conditions"))
               (:file "structures" :depends-on ("machine"))
               (:file "disassemble" :depends-on ("structures" "machine"))
               (:file "compile" :depends-on ("parse-macro"
                                             "structures" "machine"))
               (:file "cmpltv" :depends-on ("compile"))
               (:file "vm" :depends-on ("arg-conditions" "disassemble"
                                        "structures" "machine"))))

(asdf:defsystem #:cvm/test
  :author ("Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm :fiveam)
  :components
  ((:module "test"
    :components ((:file "packages")
                 (:file "suites" :depends-on ("packages"))
                 (:file "rt" :depends-on ("packages"))
                 (:file "native-sham" :depends-on ("rt" "packages"))
                 (:module "ansi"
                  :depends-on ("suites" "rt" "packages")
                  ;; These can be loaded in any order.
                  :components (;; eval-and-compile
                               (:file "compile")
                               (:file "dynamic-extent")
                               (:file "eval")
                               (:file "eval-when")
                               (:file "ignorable")
                               (:file "ignore")
                               (:file "lambda")
                               (:file "locally")
                               (:file "optimize")
                               (:file "special")
                               (:file "symbol-macrolet")
                               (:file "the")
                               (:file "type")
                               ;; data-and-control-flow
                               (:file "block")
                               (:file "catch")
                               (:file "flet")
                               (:file "if")
                               (:file "labels")
                               (:file "let")
                               (:file "letstar")
                               (:file "macrolet")
                               (:file "multiple-value-call")
                               (:file "multiple-value-prog1")
                               (:file "progn")
                               (:file "progv")
                               (:file "return-from")
                               (:file "tagbody")))))))

(asdf:defsystem #:cvm/test/cross
  :author ("Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm/test :cvm-cross :clostrum)
  :components
  ((:module "test"
    :components ((:module "cross"
                  :components ((:file "packages")
                               (:file "sham" :depends-on ("packages"))
                               (:file "rt" :depends-on ("sham"
                                                        "packages"))))))))
