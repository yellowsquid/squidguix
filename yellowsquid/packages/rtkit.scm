(define-module (yellowsquid packages rtkit)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages vim)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public rtkit
  (package
    (name "rtkit")
    (version "0.13")
    (home-page "https://github.com/heftig/rtkit")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              ;; XXX: should use [this patch], not a snippet.
              ;; [this patch]: https://github.com/heftig/rtkit/pull/18
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (substitute* "meson.build"
                    (("cc\\.find_library\\('z'\\)")
                     "cc.find_library('rt')"))))
              (sha256
               (base32 "14z09cxahpwvn229vhg1a7fqp65fdjx6rlis2jlb44i10y9fyjyk"))))
    (build-system meson-build-system)
    (inputs
     (list dbus libcap xxd))
    (native-inputs
     (list pkg-config))
    (arguments
     (list
      #:configure-flags
      #~`(,(string-append "-Ddbus_systemservicedir="
                          #$output
                          "/share/dbus-1/system-services")
          ,(string-append "-Ddbus_interfacedir="
                          #$output
                          "/share/dbus-1/interfaces")
          ,(string-append "-Ddbus_rulesdir="
                          #$output
                          "/etc/dbus-1/system.d"))))
    (synopsis "Realtime policy and watchdog daemon")
    (description "RealtimeKit is a D-Bus system service that changes the
scheduling policy of user processes/threads to SCHED_RR (i.e. realtime
scheduling mode) on request.  It is intended to be used as a secure mechanism
to allow real-time scheduling to be used by normal user processes.")
    (license license:gpl3+)))
