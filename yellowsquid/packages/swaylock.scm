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
  ;; Latest release is from November 2020, but doesn't support disabling SSE.
  (let ((commit "5cb9579faaf5662b111f5722311b701eff1c1d00")
        (revision "1"))
    (package
      (inherit swaylock-pam)
      (name "swaylock-effects")
      (version (git-version "1.6-3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mortie/swaylock-effects")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "036dkhfqgk7g9vbr5pxgrs66h5fz0rwdsc67i1w51aa9v01r35ca"))))
      (arguments
       `(#:configure-flags '("-Dsse=false")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-meson
             (lambda _
               (substitute* "meson.build"
                 (("'-mtune=native',") "")))))))
      (synopsis "Screen locking utility for Wayland compositors with effects")
      (description "@code{Swaylock-effects} is a fork of swaylock with additional
features, such as the ability to take a screenshot as the background image,
display a clock or apply image manipulation techniques to the background image.")
      (home-page "https://github.com/mortie/swaylock-effects"))))
