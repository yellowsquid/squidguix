(define-module (yellowsquid packages filesystem)
  #:use-module (gnu packages man)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public fatsort
  (package
    (name "fatsort")
    (version "1.6.5.640")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/fatsort/fatsort-" version
                           ".tar.xz"))
       (sha256
        (base32
         "0bp4zfw8qxmlryk754clp873cm3qvsmc7bph9995afpbv5bcw3k3"))))
    (native-inputs (list help2man))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "DESTDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-install
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("(MANDIR=).*" all var)
                (string-append var "/share/man/man1\n")))
             (substitute* "src/Makefile"
               (("(SBINDIR=).*" all var)
                (string-append var "/bin\n")))))
         (delete 'configure)
         (delete 'check))))
    (home-page "https://fatsort.sourceforge.net")
    (synopsis "Sorts directory structures of FAT16 and FAT32 file systems")
    (description "FATSort is a C utility that sorts FAT12, FAT16, FAT32 and
exFAT partitions. It even can handle long file name entries.")
    (license license:gpl2)))
