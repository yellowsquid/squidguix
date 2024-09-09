(define-module (yellowsquid packages network)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (yellowsquid packages))

(define-public network-manager-l2tp
  (package
    (name "network-manager-l2tp")
    (home-page "https://github.com/nm-l2tp/NetworkManager-l2tp")
    (version "1.20.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nm-l2tp/NetworkManager-l2tp")
             (commit version)))
       (file-name (git-file-name name version))
       (patches (search-patches
                 "network-manager-l2tp-fix-for-autoconf-2.69.patch"))
       (sha256
        (base32 "1f2n4wayf7dw6a04krdaj225pzra9xl72yzna4v5s017fp3wxi7g"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-absolute-paths"
                                "--localstatedir=/var"
                                "--with-gtk4=yes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-path
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (let* ((ipsec (search-input-file inputs "/sbin/ipsec"))
                     (xl2tpd (search-input-file inputs "/sbin/xl2tpd"))
                     (modprobe (search-input-file inputs "/bin/modprobe"))
                     (pretty-xl2tpd (string-append "\"" xl2tpd "\""))
                     (pretty-ipsec (string-append "\"" ipsec "\"")))
                (for-each
                 (lambda (file)
                   (substitute* file
                     (("\"(/usr(/local)?)?/s?bin/xl2tpd\"") pretty-xl2tpd)
                     (("\"(/usr(/local)?)?/s?bin/kl2tpd\"") "NULL")
                     (("\"(/usr(/local)?)?/s?bin/ipsec\"") pretty-ipsec)
                     (("\"(/usr(/local)?)?/s?bin/strongswan\"") "NULL")
                     (("/sbin/modprobe") modprobe)))
                 '("src/nm-l2tp-service.c"
                   "shared/utils.c"))))))))
    (native-inputs
     (list autoconf
           automake
           (list glib "bin")
           gobject-introspection
           (list gtk "bin")
           intltool
           libtool
           libxml2
           pkg-config))
    (inputs
     (list gettext-minimal
           gtk
           gtk+
           kmod
           libnma
           libsecret
           network-manager
           nss
           openssl
           ppp
           strongswan
           xl2tpd))
    (synopsis "L2TP plugin of NetworkManager")
    (description
     "This extension of NetworkManager allows it to take car of connections
to virtual private networks (VPNs) via L2TP and L2TP/IPSec.")
    (license license:gpl2)
    (properties `((upstream-name . "NetworkManager-l2tp")))))
