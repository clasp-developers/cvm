(defsystem "cvm-cross"
  :depends-on (:cvm :clostrum)
  :components ((:file "client")
               (:file "vm" :depends-on ("client"))))
