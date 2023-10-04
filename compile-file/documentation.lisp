(in-package #:cvm.compile-file)

(setf (documentation 'compile-file 'function)
      "As CL:COMPILE-FILE. Compile the given file using CVM. All normal COMPILE-FILE semantics should apply, e.g. top level form processing. Outputs a bytecode FASL.
Returns CL:COMPILE-FILE's usual three values of the output file, warningsp, and failurep.
Besides the standard arguments, there are
ENVIRONMENT: The compilation environment that should be used for compilation, as well as compile-time evaluation through EVAL-WHEN, #., etc. If not provided, defaults to NIL, meaning the host's global environment.
READER-CLIENT: The client passed to Eclector to perform read operations."
      (documentation 'compile-stream 'function)
      "Like COMPILE-FILE, but operates on streams. INPUT must be a character stream and OUTPUT an (unsigned-byte 8) stream. The FASL will be written out to the OUTPUT stream. Returns the output stream as its primary value.

See COMPILE-FILE")
