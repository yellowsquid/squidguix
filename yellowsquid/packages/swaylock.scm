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
    (inherit swaylock-pam)
    (name "swaylock-effects")
    (version "1.6.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirutka/swaylock-effects")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d8ri7bzwfr53ybgf23acz57wyhcl2f1nqprcda1v9bzfgsqfk2n"))))
    (arguments `(#:configure-flags '("-Dsse=false")))
    (synopsis "Screen locking utility for Wayland compositors with effects")
    (description "@code{Swaylock-effects} is a fork of swaylock with additional
features, such as the ability to take a screenshot as the background image,
display a clock or apply image manipulation techniques to the background image.")
    (home-page "https://github.com/jirutka/swaylock-effects")))
