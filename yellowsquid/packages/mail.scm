(define-module (yellowsquid packages mail)
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages mail) #:prefix gnu:)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libspf2
  (let ((version "1.2.11")
        (commit "8131fe140704eaae695e76b5cd09e39bd1dd220b")
        (revision "1"))
    (package
      (name "libspf2")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/shevek/libspf2")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "1cmddih3hkfk5q1797jlc0r7h64kjpsgby4km3ik4dc3g915k6rw"))))
      (build-system gnu-build-system)
      (synopsis "Library to check SPF records")
      (description "libspf2 implements the Sender Policy Framework, a part of the
SPF/SRS protocol pair. libspf2 is a library which allows email systems such as
Sendmail, Postfix, Exim, Zmailer and MS Exchange to check SPF records and make
sure that the email is authorized by the domain name that it is coming from.
This prevents email forgery, commonly used by spammers, scammers and email
viruses/worms.")
      (home-page "https://www.libspf2.org/index.html")
      (license (list license:lgpl2.1+ license:bsd-2)))))

(define-public opendmarc-1.3
  (package
    (name "opendmarc")
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trusteddomainproject/OpenDMARC")
             (commit "rel-opendmarc-1-3-3")))
       (file-name (git-file-name name version))
       (sha256 (base32 "15vb8ay6r7rqxcbv9063724wq864aw6zhx8lc8nl2m6zabjgq0a9"))))
    (native-inputs (list autoconf automake libtool))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-filter")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'remove-make-docs
           (lambda _
             (substitute* "configure.ac"
               (("[\t]+docs/Makefile") "")))))))
    (synopsis "DMARC record checking and policy enforcement.")
    (description "A package providing DMARC report generation and policy
enforcement services. It includes a library for handling DMARC record parsing, a
database schema and tools for aggregating and processing transaction history to
produce DMARC reports.")
    (home-page "http://www.trusteddomain.org/opendmarc")
    (license license:bsd-3)))

(define-public exim
  (package
    (inherit gnu:exim)
    (inputs (modify-inputs (package-inputs gnu:exim)
              (delete "gnutls" "gnutls-dane")
              (append openssl libspf2 opendmarc-1.3)))
    (arguments
     (substitute-keyword-arguments (package-arguments gnu:exim)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'configure 'configure-more
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (substitute* "Local/Makefile"
                    ;; Needs two paths to prevent automatic installation.
                    ;; /etc/exim.conf has to be first or it is never used.
                    (("(CONFIGURE_FILE=).*" all var)
                     (string-append var "/etc/exim.conf:" out "/etc/exim.conf\n"))
                    ;; Use openssl instead of gnutls
                    (("USE_GNUTLS(|_PC)=.*" all)
                     (string-append "# " all "\n"))
                    (("# (USE_OPENSSL(|_PC)=.*)" all line)
                        (string-append line "\n"))
                    ;; Enable LMTP
                    (("# (TRANSPORT_LMTP=yes)" all line) line)
                    ;; Enable SPF
                    (("# (SUPPORT_SPF=yes)" all line) line)
                    (("# (LDFLAGS.*spf2)" all line) line)
                    ;; Enable DMARC
                    (("# (SUPPORT_DMARC=yes)" all line) line)
                    (("# (LDFLAGS.*opendmarc)" all line) line)))))
            (add-after 'install 'install-config
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir-p (string-append out "/etc"))
                  (copy-file "src/configure.default" (string-append out "/etc/exim.conf"))
                  (substitute* (string-append out "/etc/exim.conf")
                    (("SYSTEM_ALIASES_FILE") "/etc/aliases")))))))))))
