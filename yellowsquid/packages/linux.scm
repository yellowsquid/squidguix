(define-module (yellowsquid packages linux)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define %latest-version "5.19.2")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "0gg63y078k886clgfq4k5n7nh2r0359ksvf8wd06rv01alghmr28")))))
