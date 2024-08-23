(define-module (yellowsquid packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (yellowsquid packages))

(define-public linuxconsole-tools
  (package
    (name "linuxconsole-tools")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/linuxconsole/linuxconsoletools-" version
             ".tar.bz2"))
       (patches (search-patches "linuxconsole-tools-udev-install.patch"))
       (sha256
        (base32 "0mi4bkrwp83gcg3jy7knscfi4qziggkljz7kylcdpdw2qx2rg8jd"))))
    (inputs (list sdl2))
    (native-inputs (list pkg-config))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC="
                                         ,(cc-for-target)) "DESTDIR="
                                         (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check))))
    (home-page "https://sourceforge.net/projects/linuxconsole/")
    (synopsis
     "Utilities to test and configure joysticks and the event input layer")
    (description
     "Linux Console tools includes utilities to test and configure
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

(define-public wihotspot
  (package
    (name "wihotspot")
    (version "4.7.2")
    (home-page "https://github.com/lakinduakash/linux-wifi-hotspot")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "wihotspot-destdir.patch"))
       (sha256
        (base32 "0fd22rppbj3kbgx1qkiaj8g3kagc0941q2xd59z0pj041rcxhqgr"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX="
                                          #$output) "PKGCONFIG=pkg-config")
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'check))))
    (inputs (list gtk+ qrencode))
    (propagated-inputs (list dnsmasq hostapd))
    (native-inputs (list pkg-config))
    (synopsis "Wifi hotspot creator for Linux")
    (description
     "Feature-rich wifi hotspot creator for Linux which provides both GUI and
command-line interface.  It is also able to create a hotspot using the same wifi
card which is connected to an AP already.")
    (license license:bsd-2)))
