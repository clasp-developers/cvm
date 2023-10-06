(defpackage #:cvm.compile-file
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm.compile)
		    (#:m #:cvm.machine))
  (:shadow #:compile-file #:compile-file-pathname)
  (:export #:compile-stream #:compile-file #:compile-file-pathname))
