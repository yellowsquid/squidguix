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
                (substitute* "Local/Makefile"
                  (("# (TRUSTED_CONFIG_LIST=).*" all var)
                   (string-append var "/etc/exim.conf\n"))
                  (("# (TRANSPORT_LMTP=yes)" all line) line))))))))))
