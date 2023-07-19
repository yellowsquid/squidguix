(define-module (yellowsquid packages agda)
  #:use-module (gnu packages agda)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (yellowsquid build-system agda)
  #:use-module (yellowsquid packages))

(define-public agda-stdlib-1.7.2
  (package
    (name "agda-stdlib")
    (version "1.7.2")
    (home-page "https://github.com/agda/agda-stdlib")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "065hf24xjpciwdrvk4isslgcgi01q0k93ql0y1sjqqvy5ryg5xmy"))))
    (build-system agda-build-system)
    (native-inputs (list ghc ghc-filemanip))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-everything
           ;; taken from (gnu build haskell-build-system)
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (srfi srfi-1))
             (define %tmp-db-dir
               (string-append (or (getenv "TMP") "/tmp")
                              "/package.conf.d"))

             (define (run-setuphs command params)
               (let ((setup-file (cond
                                  ((file-exists? "Setup.hs")
                                   "Setup.hs")
                                  ((file-exists? "Setup.lhs")
                                   "Setup.lhs")
                                  (else
                                   #f)))
                     (pkgdb (string-append "-package-db=" %tmp-db-dir)))
                 (if setup-file
                     (begin
                       (format #t
                               "running \"runhaskell Setup.hs\" with command \
~s and parameters ~s~%"
                               command params)
                       (apply invoke
                              "runhaskell"
                              pkgdb
                              setup-file
                              command
                              params))
                     (error "no Setup.hs nor Setup.lhs found"))))

             #;(run-setuphs "run" '("GenerateEverything"))
             (let* ((out (assoc-ref outputs "out"))
                    (name-version (strip-store-file-name out))
                    (ghc-path (getenv "GHC_PACKAGE_PATH"))
                    (conf-dirs (search-path-as-string->list
                                (getenv "GHC_PACKAGE_PATH")))
                    (conf-files (append-map
                                 (lambda (file) (find-files file "\\.conf$"))
                                 conf-dirs))
                    (params `(,(string-append "--prefix=" out)
                              ,(string-append "--libdir=" out "/lib")
                              ,(string-append "--docdir="
                                              out
                                              "/share/doc/"
                                              name-version)
                              "--libsubdir=$compiler/$pkg-$version"
                              ,(string-append "--package-db=" %tmp-db-dir)
                              "--global"
                              "--enable-shared"
                              "--enable-executable-dynamic"
                              "--ghc-option=-fPIC"
                              ,(string-append
                                "--ghc-option=-optl=-Wl,-rpath="
                                out
                                "/lib/$compiler/$pkg-$version"))))
               (mkdir-p %tmp-db-dir)
               (for-each
                (lambda (file)
                  (let ((dest (string-append %tmp-db-dir
                                             "/"
                                             (basename file))))
                    (unless (file-exists? dest)
                      (copy-file file dest))))
                conf-files)
               (invoke "ghc-pkg"
                       (string-append "--package-db=" %tmp-db-dir)
                       "recache")
               (unsetenv "GHC_PACKAGE_PATH")
               (when (file-exists? "configure")
                 (setenv "CONFIG_SHELL" "sh"))
               (run-setuphs "configure" params)
               (setenv "GHC_PACKAGE_PATH" ghc-path)
               (run-setuphs "build" '())
               (invoke
                "dist/build/GenerateEverything/GenerateEverything")))))))
    (synopsis "Standard library for Agda")
    (description "The Agda standard library aims to contain all the tools
needed to write both programs and proofs easily.  Whilst the library tries to
write efficient code, ease of proof is prioritised over type-checking and
normalisation performance.")
    (license license:expat)))

(define-public agda-stdlib agda-stdlib-1.7.2)

(define-public agda-categories
  (package
    (name "agda-categories")
    (version "0.1.7.2")
    (home-page "https://github.com/agda/agda-categories")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xwgm2mfl2pxipsv31bin8p14y1yhd9n27lv3clvsxd4z9yc034m"))
              (patches
               (search-patches "agda-categories-everything.patch"))))
    (build-system agda-build-system)
    (inputs (list agda-stdlib-1.7.2))
    (arguments
     '(#:readme "Everything.agda"))
    (synopsis "Categories library for Agda")
    (description "A proof-relevant category theory library for Agda.  The
library contains definitions for many important parts of category theory.
A major goal is to make the category ready to be incorporated into the
standard library.  Note that the library is currently pre-beta software, and
backwards compatibility is not assured.")
    (license license:expat)))

(define-public cubical
  (package
    (name "cubical")
    (version "0.5")
    (home-page "https://github.com/agda/cubical")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yfg7gr55n08ly1qgzpcp16s15k1abycppbcdi9lzg1hjryqxcg3"))))
    (build-system agda-build-system)
    (inputs (list ghc))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-everything
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "gen-everythings"))))
       #:everything "Cubical/README.agda"
       #:readme "Cubical/README.agda"))
    (synopsis "Standard library for Cubical Agda")
    (description "A standard library for Cubical Agda. ")
    (license license:expat)))
