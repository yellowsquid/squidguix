(define-module (yellowsquid packages network)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vpn)
  #:use-module (guix build-system gnu)
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
    (native-inputs (list autoconf
                         automake
                         gettext-minimal
                         (list glib "bin")
                         gobject-introspection
                         libtool
                         pkg-config))
    (inputs (list gettext-minimal
                  gtk+
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
