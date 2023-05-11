(define-module (yellowsquid packages swaylock)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public swaylock-pam
  (package
    (inherit swaylock)
    (inputs (modify-inputs (package-inputs swaylock)
               (append linux-pam)))))

(define-public swaylock-effects-pam
  (package
    (inherit swaylock-effects)
    (version "1.6.11")
    (name "swaylock-effects")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirutka/swaylock-effects")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j7dxn66xqlf6iv2arqzz7mxlh7nf85anvpyf30d2frcidarda9h"))))
    (inputs (modify-inputs (package-inputs swaylock-effects)
               (append linux-pam)))))
