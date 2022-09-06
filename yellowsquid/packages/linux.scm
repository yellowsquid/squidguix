(define-module (yellowsquid packages linux)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define %latest-version "5.19.7")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "17qr061b617g64s60svw7lf9s5vn5zwd1y96cwckjpr5shcn1fxq")))))
