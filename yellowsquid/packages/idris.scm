(define-module (yellowsquid packages idris)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9))

(define-record-type <idris-source>
  (idris-source origin version tag guix-version)
  idris-source?
  (origin
    idris-source-origin)
  (version idris-source-version)
  (tag idris-source-tag)
  (guix-version idris-source-guix-version))

;; Make the specified target.
(define* (make-target targets #:key (extra-flags #~'()) (tests? #f))
  #~(lambda* (#:key make-flags parallel-build? parallel-tests? #:allow-other-keys)
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
                   (replace 'build #$(make-target '("support")))
                   (replace 'install
                     (lambda* (#:key make-flags #:allow-other-keys)
                       (apply invoke "make" "install-support" make-flags)))
                   (add-after 'install 'fix-paths
                     (lambda _
                       ;; Split top-level /idris2-${version} into /lib and /share/idris2-support
                       (let* ((old-dir (string-append #$output
                                                      "/idris2-"
                                                      #$(idris-source-version idris-source)))
                              (old-lib (string-append old-dir "/lib"))
                              (new-lib (string-append #$output "/lib"))
                              (old-share (string-append old-dir "/support"))
                              (new-share (string-append #$output "/share/idris2-support")))
                         (mkdir-p (dirname new-lib))
                         (mkdir-p (dirname new-share))
                         (rename-file old-lib new-lib)
                         (rename-file old-share new-share)
                         (rmdir old-dir)))))))
    (synopsis "")
    (description "")
    (license license:bsd-3)
    (home-page "https://www.idris-lang.org")))

(define* (make-idris2 idris-source support #:key (bootstrap-idris #f))
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
              (files (list (string-append "lib/idris2-" (idris-source-version idris-source)))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list (string-append "PREFIX=" #$output)
                             (string-append "CC=" #$(cc-for-target))
                             (string-append "VERSION_TAG=" #$(idris-source-tag idris-source))
                             (string-append "IDRIS2_SUPPORT_DIR=" #$support-libs))
        #:test-target "test"
        #:phases #~(modify-phases %standard-phases
                     (delete 'bootstrap)
                     (delete 'configure)
                     #$(if bootstrap-idris
                           #~(add-before 'build 'patch-paths
                               (lambda* _
                                 (format (current-error-port)
                                         "patch-paths: no patches as not bootstrapping~%")))
                           #~(add-before 'build 'patch-paths
                               ;; Bootstrapping generates the wrong shebangs.
                               ;; We have to patch the sources first.
                               (lambda* (#:key inputs #:allow-other-keys)
                                 (let ((sh (string-append "#!" (search-input-file inputs "/bin/sh")))
                                       (env (search-input-file inputs "/bin/env"))
                                       ;; NOTE: this is overzealous.
                                       (files-to-patch
                                        (append
                                         (find-files "src/Compiler")
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
                                    files-to-patch)))))
                     (replace 'build
                       #$(make-target
                          (if bootstrap-idris '("all") '("bootstrap"))
                          #:extra-flags
                          (if bootstrap-idris
                              #~'()
                              #~(list
                                 (string-append "SCHEME=" #+(file-append chez-scheme "/bin/chez-scheme"))
                                 (string-append "IDRIS2_DATA=" #$support-share)
                                 (string-append "IDRIS2_LIBS=" #$support-libs)))))
                     ;; Change target of gnu:check
                     (replace 'check
                       (lambda* (#:key target make-flags tests? test-target
                                 parallel-tests? test-suite-log-regexp
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
                       #$(make-target '("install-idris2" "install-with-src-libs")))
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
                           (delete-file-recursively (string-append #$output "/bin/idris2_app"))

                           ;; Write wrapper executable
                           (call-with-output-file idris
                             (lambda (port)
                               (format port
                                       "#!~a~%~a~%exec -a \"$0\" \"~a\" \"$@\"~%"
                                       (search-input-file inputs "/bin/sh")
                                       (string-join
                                        (list
                                         (format #f "export ~a=\"${~a:-~a}\""
                                                 "CHEZ" "CHEZ"
                                                 (search-input-file inputs "/bin/chez-scheme"))
                                         (format #f "export ~a=\"${~a:-\"~a\"}\""
                                                 "IDRIS2_PREFIX" "IDRIS2_PREFIX"
                                                 "$HOME/.idris2")
                                         (format #f "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_LIBS" "IDRIS2_LIBS" "IDRIS2_LIBS"
                                                 #$support-libs)
                                         (format #f "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_DATA" "IDRIS2_DATA" "IDRIS2_DATA"
                                                 #$support-share)
                                         (format #f "export ~a=\"${~a}${~a:+:}~a\""
                                                 "IDRIS2_PACKAGE_PATH" "IDRIS2_PACKAGE_PATH" "IDRIS2_PACKAGE_PATH"
                                                 "$GUIX_IDRIS2_PACKAGE_PATH")
                                         (format #f "export ~a=\"${~a}${~a:+:}~a\""
                                                 "LD_LIBRARY_PATH" "LD_LIBRARY_PATH" "LD_LIBRARY_PATH"
                                                 #$support-libs)
                                         (format #f "export ~a=\"${~a}${~a:+:}~a\""
                                                 "DYLD_LIBRARY_PATH" "DYLD_LIBRARY_PATH" "DYLD_LIBRARY_PATH"
                                                 #$support-libs))
                                        "\n")
                                       lib-exec)))
                           (chmod idris #o755)

                           ;; Move libraries to /share/{name}
                           (mkdir-p (dirname package-path))
                           (rename-file
                            (string-append #$output "/idris2-" #$(idris-source-version idris-source))
                            package-path)))))))
      (synopsis "")
      (description "")
      (license license:bsd-3)
      (home-page "https://www.idris-lang.org"))))

(define (idris-git-source version commit tag hash)
  (let ((guix-version (string-append version "-" tag)))
    (idris-source
     (origin
       (method git-fetch)
       (uri (git-reference (url
                            "https://github.com/idris-lang/Idris2.git")
                           (commit commit)))
       (file-name (git-file-name "idris2" guix-version))
       (sha256 (base32 hash)))
     version
     (substring tag 4)
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

(define-public idris2 idris2-root)

;; Latest git commit

(define %idris-source-git
  (idris-git-source "0.7.0" "034f1e89c4c58cdd59aabe2b0d0fe4e9ff3411f6"
                    "50-g034f1e89c"
                    "1b6yvarydyk2m1q82hg96f2rfywda42i4cw66jzbm71fvg84ya2k"))

(define-public idris2-support-git
  (package
    (inherit (make-idris-support %idris-source-git))
    (properties '((hidden . #t)))))

(define-public idris2-git
  (package
    (inherit (make-idris2 %idris-source-git idris2-support-git
                          #:bootstrap-idris idris2-root))
    (properties '((hidden . #t)))))
