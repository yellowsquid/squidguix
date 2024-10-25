(define-module (yellowsquid packages idris)
  #:use-module (gnu packages base)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:use-module (yellowsquid build-system idris)
  #:use-module (yellowsquid packages))

(define-record-type <idris-source>
  (idris-source origin version tag guix-version)
  idris-source?
  (origin
    idris-source-origin)
  (version idris-source-version)
  (tag idris-source-tag)
  (guix-version idris-source-guix-version))

;; Make the specified target.
(define* (make-target targets
                      #:key
                      (extra-flags #~'())
                      (tests? #f))
  #~(lambda* (#:key make-flags parallel-build? parallel-tests?
              #:allow-other-keys)
      (apply invoke "make"
             `(#$@targets
               ,@(if (or (and #$(not tests?) parallel-build?)
                         (and #$tests? parallel-tests?))
                     `("-j" ,(number->string (parallel-job-count)))
                     '())
               ,@make-flags
               ,@#$extra-flags))))

(define* (make-idris-support idris-source)
  (package
    (name "idris2-support")
    (version (idris-source-guix-version idris-source))
    (source
     (idris-source-origin idris-source))
    (build-system gnu-build-system)
    (inputs (list gmp))
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output)
                           (string-append "CC=" #$(cc-for-target)))
      #:test-target "test-support"
      #:phases #~(modify-phases %standard-phases
                   (delete 'bootstrap)
                   (delete 'configure)
                   (replace 'build
                     #$(make-target '("support")))
                   (replace 'install
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (apply invoke "make" "install-support" make-flags)))
                   (add-after 'install 'fix-paths
                     (lambda _
                       ;; Split top-level /idris2-${version}
                       ;; into /lib and /share/idris2-support
                       (let* ((old-dir (string-append #$output "/idris2-"
                                                      #$(idris-source-version
                                                         idris-source)))
                              (old-lib (string-append old-dir "/lib"))
                              (new-lib (string-append #$output "/lib"))
                              (old-share (string-append old-dir "/support"))
                              (new-share (string-append #$output
                                                        "/share/idris2-support")))
                         (mkdir-p (dirname new-lib))
                         (mkdir-p (dirname new-share))
                         (rename-file old-lib new-lib)
                         (rename-file old-share new-share)
                         (rmdir old-dir)))))))
    (synopsis "")
    (description "")
    (license license:bsd-3)
    (home-page "https://www.idris-lang.org")))

(define* (make-idris2 idris-source support
                      #:key (bootstrap-idris #f))
  (let* ((support-libs (file-append support "/lib"))
         (support-share (file-append support "/share/idris2-support")))
    (package
      (name "idris2")
      (version (idris-source-guix-version idris-source))
      (source
       (idris-source-origin idris-source))
      (native-inputs `(,chez-scheme ,@(if bootstrap-idris
                                          (list bootstrap-idris)
                                          '())))
      (inputs (list support chez-scheme gmp))
      (native-search-paths
       (list (search-path-specification
              (variable "GUIX_IDRIS2_PACKAGE_PATH")
              (files (list (string-append "lib/idris2-"
                                          (idris-source-version idris-source)))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list (string-append "PREFIX="
                                            #$output)
                             (string-append "CC="
                                            #$(cc-for-target))
                             (string-append "VERSION_TAG="
                                            #$(idris-source-tag idris-source))
                             (string-append "IDRIS2_SUPPORT_DIR="
                                            #$support-libs))
        #:test-target "test"
        #:phases #~(modify-phases %standard-phases
                     (delete 'bootstrap)
                     (delete 'configure)
                     (add-before 'build 'patch-paths
                       ;; Bootstrapping generates the wrong shebangs.
                       ;; We have to patch the sources first.
                       (lambda* (#:key inputs #:allow-other-keys)
                         (let ((sh (string-append "#!" (search-input-file inputs "/bin/sh")))
                               (env (search-input-file inputs "/bin/env"))
                               ;; NOTE: this is overzealous.
                               (files-to-patch
                                (append (find-files "src/Compiler")
                                        (find-files "." "\\.((sh)|(ss)|(rkt))$"))))
                           ;; Derived from patch-/usr/bin/file
                           (for-each
                            (lambda (file)
                              (when (file-exists? file)
                                (substitute* file
                                  ((#$(regexp-quote "#!/bin/sh"))
                                   (begin
                                     (format (current-error-port)
                                             "patch-paths: ~a: changing `~a' to `~a'~%"
                                             file "#!/bin/sh" sh)
                                     sh))
                                  ((#$(regexp-quote "/usr/bin/env"))
                                   (begin
                                     (format (current-error-port)
                                             "patch-paths: ~a: changing `~a' to `~a'~%"
                                             file "/usr/bin/env" env)
                                     env)))))
                            files-to-patch))))
                     (replace 'build
                       #$(make-target (if bootstrap-idris
                                          '("all")
                                          '("bootstrap"))
                                      #:extra-flags
                                      (if bootstrap-idris
                                          #~'()
                                          #~(list (string-append
                                                   "SCHEME="
                                                   #+(file-append chez-scheme "/bin/chez-scheme"))
                                                  (string-append
                                                   "IDRIS2_DATA="
                                                   #$support-share)
                                                  (string-append
                                                   "IDRIS2_LIBS="
                                                   #$support-libs)))))
                     ;; Change target of gnu:check
                     (replace 'check
                       (lambda* (#:key target
                                 make-flags
                                 tests?
                                 test-target
                                 parallel-tests?
                                 test-suite-log-regexp
                                 #:allow-other-keys)
                         (if tests?
                             #$(make-target
                                #~'(test-target)
                                #:extra-flags
                                #~'("INTERACTIVE="
                                    (string-append "IDRIS2_DATA=" #$support-share)
                                    (string-append "IDRIS2_LIBS=" #$support-libs)
                                    (string-append "TEST_IDRIS2_DATA=" #$support-share)
                                    (string-append "TEST_IDRIS2_LIBS=" #$support-libs)
                                    (string-append "TEST_IDRIS2_SUPPORT_DIR=" #$support-libs)))
                             (format #t "test suite not run~%"))))
                     (replace 'install
                       #$(make-target '("install-idris2"
                                        "install-with-src-libs")))
                     (add-after 'install 'wrap
                       (lambda* (#:key inputs #:allow-other-keys)
                         (let* ((lib-exec (string-append #$output "/libexec/idris2.so"))
                                (package-path (format #f "~a/lib/idris2-~a"
                                                      #$output
                                                      #$(idris-source-version idris-source)))
                                (idris (string-append #$output "/bin/idris2")))
                           ;; Remove existing wrapper because LD_LIBRARY_PATH is incorrect
                           (delete-file idris)

                           ;; Keep only the binary
                           (mkdir-p (dirname lib-exec))
                           (rename-file
                            (string-append #$output "/bin/idris2_app/idris2.so")
                            lib-exec)
                           (delete-file-recursively
                            (string-append #$output "/bin/idris2_app"))

                           ;; Write wrapper executable
                           (call-with-output-file idris
                             (lambda (port)
                               (format port
                                       "#!~a~%~a~%exec -a \"$0\" \"~a\" \"$@\"~%"
                                       (search-input-file inputs "/bin/sh")
                                       (string-join
                                        (list
                                         (format #f
                                                 "export ~a=\"${~a:-~a}\""
                                                 "CHEZ" "CHEZ"
                                                 (search-input-file inputs "/bin/chez-scheme"))
                                         (format #f
                                                 "export ~a=\"${~a:-\"~a\"}\""
                                                 "IDRIS2_PREFIX"
                                                 "IDRIS2_PREFIX"
                                                 "$HOME/.idris2")
                                         (format #f
                                                 "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_LIBS"
                                                 "IDRIS2_LIBS"
                                                 "IDRIS2_LIBS"
                                                 #$support-libs)
                                         (format #f
                                                 "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_DATA"
                                                 "IDRIS2_DATA"
                                                 "IDRIS2_DATA"
                                                 #$support-share)
                                         (format #f
                                                 "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_PACKAGE_PATH"
                                                 "IDRIS2_PACKAGE_PATH"
                                                 "IDRIS2_PACKAGE_PATH"
                                                 "$GUIX_IDRIS2_PACKAGE_PATH")
                                         (format #f
                                                 "export ~a=\"${~a}${~a:+:}~a\""
                                                 "LD_LIBRARY_PATH"
                                                 "LD_LIBRARY_PATH"
                                                 "LD_LIBRARY_PATH"
                                                 #$support-libs)
                                         (format #f
                                                 "export ~a=\"${~a}${~a:+:}~a\""
                                                 "DYLD_LIBRARY_PATH"
                                                 "DYLD_LIBRARY_PATH"
                                                 "DYLD_LIBRARY_PATH"
                                                 #$support-libs))
                                        "\n")
                                       lib-exec)))
                           (chmod idris #o755)

                           ;; Move libraries to /share/{name}
                           (mkdir-p (dirname package-path))
                           (rename-file
                            (string-append #$output "/idris2-" #$(idris-source-version idris-source))
                            package-path)))))))
      (synopsis "Dependently typed functional programming language and proof assistant")
      (description "Idris is a programming language designed to encourage Type-Driven Development.  In type-driven development, types are tools for constructing programs.  We treat the type as the plan for a program, and use the compiler and type checker as our assistant, guiding us to a complete program that satisfies the type.  The more expressive the type is that we give up front, the more confidence we can have that the resulting program will be correct.")
      (license license:bsd-3)
      (home-page "https://www.idris-lang.org"))))

(define (idris-git-source version commit tag hash)
  (let ((guix-version (string-append version "-" tag)))
    (idris-source (origin
                    (method git-fetch)
                    (uri (git-reference (url
                                         "https://github.com/idris-lang/Idris2")
                                        (commit commit)))
                    (file-name (git-file-name "idris2" guix-version))
                    (sha256 (base32 hash))
                    (patches (search-patches "idris2-no-support.patch")))
                  version
                  (substring tag (1+ (string-index tag #\g)))
                  (string-append version "-" tag))))

;; The earliest idris we can build

(define %idris-source-root
  (idris-git-source "0.7.0" "034f1e89c4c58cdd59aabe2b0d0fe4e9ff3411f6"
                    "50-g034f1e89c"
                    "1b6yvarydyk2m1q82hg96f2rfywda42i4cw66jzbm71fvg84ya2k"))

(define idris2-support-root
  (make-idris-support %idris-source-root))

(define idris2-bootstrap-root
  (package
    (inherit (make-idris2 %idris-source-root idris2-support-root))
    (name "idris2-bootstrap")))

(define idris2-root
  (make-idris2 %idris-source-root idris2-support-root
               #:bootstrap-idris idris2-bootstrap-root))

;; Latest "stable" version

(define-public idris2
  idris2-root)

;; Latest git commit

(define %idris-source-git
  (idris-git-source "0.7.0" "0659bcc2c0c8b437affd9d33b62668f0b08c74f7"
                    "268-g0659bcc2c"
                    "1chjy0gnrsgb205rhxridbpp7nf72922mg1cx6mg1gkl9z97ad1j"))

(define-public idris2-support-git
  (package
    (inherit (make-idris-support %idris-source-git))
    (name "idris2-support-git")
    (properties '((hidden . #t)))))

(define-public idris2-git
  (package
    (inherit (make-idris2 %idris-source-git idris2-support-git
                          #:bootstrap-idris idris2-root))
    (name "idris2-git")
    (properties '((hidden . #t)))))

;; Idris packages

(define-public idris2-api
  (let ((idris-source %idris-source-root))
    (package
      (name "idris2-api")
      (version (idris-source-guix-version idris-source))
      (source
       (idris-source-origin idris-source))
      (build-system idris2-build-system)
      (native-inputs (list gnu-make))
      (arguments
       (list
        #:ipkg-name "idris2api"
        #:phases #~(modify-phases %standard-phases
                     (add-before 'build 'make-idris-paths
                       (lambda* _
                         (invoke "make"
                                 (string-append "VERSION_TAG="
                                                #$(idris-source-tag idris-source))
                                 "src/IdrisPaths.idr"))))))
      (synopsis "API for the Idris2 compiler")
      (description "API for the Idris2 compiler.")
      (license license:bsd-3)
      (home-page "https://www.idris-lang.org"))))

(define-public idris2-collie
  (package
    (name "idris2-collie")
    (version "0.0.0-1")
    (source
     (let ((commit "e496277f6dd557d31668b2430cfb07c47e841e2f"))
       (origin
         (method git-fetch)
         (uri (git-reference (url "https://github.com/ohad/collie")
                             (commit commit)))
         (file-name (git-file-name name commit))
         (sha256 (base32
                  "0ka0yj3f1da0hdm5iwii4y23v32y8cfm0mnrm8f6vvy30xazlj3n")))))
    (build-system idris2-build-system)
    (arguments
     '(#:ipkg-name "collie"))
    (synopsis "Command line interfaces for Idris2 applications")
    (description "Command line interfaces for Idris2 applications.")
    (license (license:non-copyleft "file://LICENSE"))
    (home-page "https://github.com/ohad/collie")))

(define-public idris2-elab-util
  (package
    (name "idris2-elab-util")
    (home-page "https://github.com/stefan-hoeck/idris2-elab-util")
    (version "0.6.0")
    (source
     (let ((commit "2fc2d188640ce6822b5e250db73b62f5a952ca4d"))
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page)
                             (commit commit)))
         (file-name (git-file-name name commit))
         (sha256 (base32
                  "0s956hm3njm7d3kccrppqxnk4az67scp57k8v4j27q3j3y2fp0q9")))))
    (build-system idris2-build-system)
    (arguments
     '(#:ipkg-name "elab-util"))
    (synopsis "Idris2 elaborator reflection library")
    (description "Utilities and documentation for exploring Idris2 elaborator reflection.")
    (license license:bsd-2)))
