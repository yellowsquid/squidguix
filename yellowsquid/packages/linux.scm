(define-module (yellowsquid packages linux)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define %latest-version "5.17.4")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "1ifkl1j5dimipqxwki26i4v6gav70g24456y7ynbb71sx1pdag3f")))))

linux-latest
