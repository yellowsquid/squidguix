(define-module (yellowsquid packages swaylock)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public swaylock-pam
  (package
    (name "swaylock")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaylock")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r95p4w11dwm5ra614vddz83r8j7z6gd120z2vcchy7m9b0f15kf"))))
    (build-system meson-build-system)
    (inputs (list cairo gdk-pixbuf libxkbcommon linux-pam wayland))
    (native-inputs (list pango pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Screen locking utility for Wayland compositors")
    (description "Swaylock is a screen locking utility for Wayland compositors.")
    (license license:expat)))

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
