(define-module (yellowsquid packages dns)
  :use-module (gnu packages admin)
  :use-module (gnu packages autotools)
  :use-module (gnu packages bash)
  :use-module (gnu packages curl)
  :use-module (gnu packages linux)
  :use-module (gnu packages perl)
  :use-module (gnu packages perl-check)
  :use-module (gnu packages web)
  :use-module (guix build-system gnu)
  :use-module (guix gexp)
  :use-module (guix git-download)
  :use-module (guix packages)
  :use-module ((guix licenses) :prefix license:)
  :use-module (yellowsquid packages)
  )

(define-public ddclient
  (package
    (name "ddclient")
    (version "3.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ddclient/ddclient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14v5vfab43kcdlsp1nn2xd6g979d9zz9hslkr1rmklngf5dqkkrq"))
       (modules '((guix build utils)))
       (snippet
        ;; XXX: should just patch test
        #~(begin
            (substitute* "Makefile.am"
              (("t/get_ip_from_if.pl") ""))))
       #;(patches (search-patches "ddclient-skip-test.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool
           perl-test-warnings perl-test-mockmodule))
    (inputs
     (list inetutils ; logger
           net-tools
           bash-minimal                           ;for 'wrap-program'
           curl
           perl
           perl-digest-sha1
           perl-io-socket-ssl
           ;; perl-io-socket-inet6  ;; XXX: this is likely to be removed in a future ddclient release
           ;;                       ;; https://github.com/ddclient/ddclient/issues/461
           perl-json))
    (arguments
     (list
      #:configure-flags #~(list "--localstatedir=/var")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              ;; XXX: Do not create /var
              (invoke "make" "localstatedir=/tmp/discard" "install")))
          (add-after 'wrap 'wrap-ddclient
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/ddclient")
                `("PERL5LIB" ":" prefix ,(string-split (getenv "PERL5LIB") #\:))
                `("PATH" prefix ,(map (lambda (x)
                                        (string-append (assoc-ref inputs x) "/bin"))
                                      '("inetutils" "net-tools")))))))))
    ;; (native-search-paths (list $SSL_CERT_DIR $SSL_CERT_FILE))
    (home-page "https://ddclient.net/")
    (synopsis "Address updating utility for dynamic DNS services")
    (description "This package provides a client to update dynamic IP
addresses with several dynamic DNS service providers, such as
@uref{https://www.dyndns.com/account/login.html,DynDNS.com}.
This makes it possible to use a fixed hostname (such as myhost.dyndns.org) to
access a machine with a dynamic IP address.
The client supports both dynamic and (near) static services, as well as MX
record and alternative name management.  It caches the address, and only
attempts the update when it has changed.")
    (license license:gpl2+)))
