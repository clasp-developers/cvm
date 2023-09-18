(defsystem "cvm-cross"
  :depends-on (:cvm :clostrum :clostrum-trucler)
  :components ((:file "client")
               (:file "vm" :depends-on ("client"))))
