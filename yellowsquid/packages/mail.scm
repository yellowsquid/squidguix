(define-module (yellowsquid packages mail)
  #:use-module ((gnu packages mail) #:prefix gnu:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public exim
  (package
    (inherit gnu:exim)
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
                    (("# (TRANSPORT_LMTP=yes)" all line) line)))))
            (add-after 'install 'install-config
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir-p (string-append out "/etc"))
                  (copy-file "src/configure.default" (string-append out "/etc/exim.conf"))
                  (substitute* (string-append out "/etc/exim.conf")
                    (("SYSTEM_ALIASES_FILE") "/etc/aliases")))))))))))
