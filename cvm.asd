(asdf:defsystem #:cvm
  :description "Reference implementation of the CVM bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.5.0"
  :depends-on (:cvm/base :cvm/compile :cvm/compile-file :cvm/load
               :cvm/vm-native :cvm/vm-cross))

(asdf:defsystem #:cvm/base
  :description "Basic components of the CVM bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.5.0"
  :depends-on (:closer-mop)
  :components ((:file "machine")
               (:file "arg-conditions")
               (:file "structures" :depends-on ("machine"))
               (:file "link" :depends-on ("machine"))
               (:file "disassemble" :depends-on ("structures" "machine"))))

(asdf:defsystem #:cvm/compile
  :description "Reference implementation compiler for CVM."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm/base :alexandria :trucler :ecclesia)
  :components
  ((:module "compile"
    :components ((:file "package")
                 (:file "parse-macro" :depends-on ("package"))
                 (:file "compile" :depends-on ("parse-macro"
                                               "package"))))))

(asdf:defsystem #:cvm/compile-file
  :description "Reference implementation file compiler for CVM."
  :author ("Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :depends-on (:cvm/compile :eclector :ieee-floats)
  :components
  ((:module "compile-file"
    :components ((:file "package")
                 (:file "preliminaries" :depends-on ("package"))
                 (:file "read" :depends-on ("preliminaries" "package"))
                 (:file "cmpltv" :depends-on ("preliminaries" "package"))
                 (:file "encode" :depends-on ("cmpltv" "preliminaries" "package"))
                 (:file "top-level-forms" :depends-on ("preliminaries" "package"))
                 (:file "compile-file"
                  :depends-on ("read" "top-level-forms" "cmpltv"
                                      "encode" "package"))))))

(asdf:defsystem #:cvm/load
  :description "Reference implementation FASL loader for CVM."
  :author ("Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :depends-on (:cvm/base :ieee-floats)
  :components ((:file "loadltv")))

(asdf:defsystem #:cvm/vm-native
  :description "CVM VM implementation using host environment."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm/base)
  :components ((:file "vm-native")))

(asdf:defsystem #:cvm/vm-cross
  :description "CVM VM implementation using Clostrum environment."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm/base :clostrum)
  :components ((:file "vm-cross")))

(asdf:defsystem #:cvm/test
  :author ("Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:cvm :clostrum-basic :clostrum-trucler :fiveam)
  :components
  ((:module "test"
    :components ((:file "packages")
                 (:file "suites" :depends-on ("packages"))
                 (:file "rt" :depends-on ("packages"))
                 (:file "native-sham" :depends-on ("rt" "packages"))
                 (:module "cross"
                  :depends-on ("rt" "native-sham")
                  :components ((:file "packages")
                               (:file "sham" :depends-on ("packages"))
                               (:file "rt" :depends-on ("sham"
                                                        "packages"))))
                 (:module "fasl"
                  :depends-on ("suites" "rt" "packages")
                  :components ((:file "similarity")
                               (:file "externalize")))
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
                               (:file "tagbody")
                               (:file "unwind-protect")))
                 (:file "run-all"
                  :depends-on ("ansi" "cross" "rt" "packages"))))))
