(define-module (yellowsquid packages linux)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define %latest-version "5.17.6")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "035i9i0gg3fxi5ighjrya97592sk0i4xagra6a8m8nxyh21z3k34")))))
