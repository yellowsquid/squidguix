(define-module (yellowsquid packages swaylock)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (guix packages))

(define-public swaylock-pam
  (package
    (inherit swaylock)
    (inputs (modify-inputs (package-inputs swaylock)
               (append linux-pam)))))

(define-public swaylock-effects-pam
  (package
    (inherit swaylock-effects)
    (name "swaylock-effects")
    (inputs (modify-inputs (package-inputs swaylock)
               (append linux-pam)))))
