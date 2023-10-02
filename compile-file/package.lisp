(defpackage #:cvm.compile-file
  (:use #:cl)
  (:local-nicknames (#:cmp #:cvm.compile))
  (:shadow #:compile-file)
  (:export #:compile-stream #:compile-file))
