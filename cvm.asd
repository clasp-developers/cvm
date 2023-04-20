(asdf:defsystem #:cvm
  :description "Reference implementation of the CVM bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.5.0"
  :depends-on (:closer-mop ; machine
               :alexandria :trucler :trivial-cltl2 ; compiler
               :ieee-floats) ; compile-file
  :components ((:file "machine")
               (:file "structures" :depends-on ("machine"))
               (:file "disassemble" :depends-on ("structures" "machine"))
               (:file "compile" :depends-on ("structures" "machine"))
               (:file "cmpltv" :depends-on ("compile"))
               (:file "vm" :depends-on ("disassemble" "structures" "machine"))))
