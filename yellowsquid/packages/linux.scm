(define-module (yellowsquid packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (yellowsquid packages))

(define %latest-version "5.19.8")

(define-public linux-latest
  (package
    (inherit linux)
    (name "linux-latest")
    (version %latest-version)
    (source ((@@ (gnu packages linux) %upstream-linux-source)
             %latest-version
             (base32 "1kl7fifsa6vsm34xg3kd2svhx18n771hfj67nhwnlalmb9whhqv1")))))

(define-public linuxconsole-tools
  (package
    (name "linuxconsole-tools")
    (version "1.8.1")
    (source (origin (method url-fetch)
                    (uri (string-append
                          "mirror://sourceforge/linuxconsole/linuxconsoletools-"
                          version ".tar.bz2"))
                    (patches
                     (search-patches "linuxconsole-udev-install.patch"))
                    (sha256
                     (base32
                      "0mi4bkrwp83gcg3jy7knscfi4qziggkljz7kylcdpdw2qx2rg8jd"))))
    (inputs (list sdl2))
    (native-inputs (list pkg-config))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "DESTDIR="
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check))))
    (home-page "https://sourceforge.net/projects/linuxconsole")
    (synopsis "Utilities to test and configure joysticks and the event input layer.")
    (description "Linux Console tools includes utilities to test and configure
joysticks, connect legacy devices to the kernel's input subsystem (providing
support for serial mice, touchscreens etc.), and test the input event layer.

* evdev-joystick - calibrate joystick devices (including dead zones and fuzz)

* ffcfstress, ffmvforce, fftest - test force-feedback devices

* ffset - set force-feedback device parameters

* jscal - calibrate joystick devices, reconfigure the axes and buttons

* jscal-store, jscal-restore - store and retrieve joystick device settings as
  configured using jscal

* jstest - test joystick devices")
    (license license:gpl2+)))
