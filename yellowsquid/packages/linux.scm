(define-module (yellowsquid packages linux)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define %latest-version "5.18.8")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "0dhaj1zcsr5sfg62byzvvkhm9j419px6v9v04ngcy0d0vc2yn8q8")))))
