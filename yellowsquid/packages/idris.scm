(define-module (yellowsquid packages idris)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages node)
  #:use-module (gnu packages racket)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

;;;
;;; Idris 2
;;;
(define* (make-idris-package source idris-version
                             #:key bootstrap-idris
                             (idris-version-tag #false)
                             (guix-version (string-append
                                            idris-version
                                            (if idris-version-tag
                                                (string-append
                                                 "-" idris-version-tag)
                                                "")))
                             (ignore-test-failures? #false)
                             (unwrap 1)
                             (tests? #true)
                             (historical? #false)
                             (hidden? #false) ; or (hidden? historical?)
                             (substitutable? (not historical?))
                             (files-to-patch-for-shell
                              '("src/Compiler/Scheme/Chez.idr"
                                "src/Compiler/Scheme/Racket.idr"
                                "src/Compiler/Scheme/Gambit.idr"
                                "src/Compiler/ES/Node.idr"
                                "bootstrap/idris2_app/idris2.rkt"
                                "bootstrap/idris2_app/idris2.ss"
                                "build/stage1/idris2_app/idris2.ss"
                                "build/stage1/idris2_app/idris2.rkt"
                                ))
                             (with-bootstrap-shortcut? (not historical?)))
  "HISTORICAL? means that it's only interesting for historical reasons, e.g. to be
used as a bootsrapping stage.

WITH-BOOTSTRAP-SHORTCUT? controls whether to use a previous version of Idris to
build us (which is potentially recursive), or use the captured compiler output
(Scheme code)."
  (package
    (name "idris2")
    (version guix-version)
    (source (match source
              ((commit hash . url)
               (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url (if (null? url)
                                "https://github.com/idris-lang/Idris2.git"
                                (car url)))
                       (commit commit)))
                 (sha256 (base32 hash))
                 (file-name (git-file-name name version))))
              ((or (? git-checkout?)
                   (? local-file?))
               source)))
    (build-system gnu-build-system)
    (native-inputs
     (list (if with-bootstrap-shortcut?
               chez-scheme
               bootstrap-idris)
           coreutils which git
           node-lts                     ; only for the tests
           #;racket                       ; FIXME: tests are failing
           sed))
    (inputs
     (list bash-minimal chez-scheme gmp))
    (arguments
     (list
      #:tests? tests?
      #:substitutable? substitutable?
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              #$(string-append "IDRIS_VERSION=" idris-version)
              #$(string-append "IDRIS_VERSION_TAG=" (or idris-version-tag ""))
              #$(if with-bootstrap-shortcut?
                    #~(string-append "SCHEME="
                                     #$(this-package-input "chez-scheme")
                                     "/bin/scheme")
                    #~(string-append "BOOTSTRAP_IDRIS="
                                     #$bootstrap-idris
                                     "/bin/" #$(package-name bootstrap-idris)))
              (string-append "PREFIX=" (assoc-ref %outputs "out"))
              "-j1")
      #:phases
      `(modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (delete 'check)    ; check must happen after install and wrap-program
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((files-to-patch (filter file-exists?
                                           ',files-to-patch-for-shell)))
               (substitute* files-to-patch
                 ((,(regexp-quote "#!/bin/sh"))
                  (string-append "#!" (assoc-ref inputs "bash") "/bin/sh"))
                 (("/usr/bin/env")
                  (string-append (assoc-ref inputs "coreutils") "/bin/env"))))))
         ,@(case unwrap
               ((1) `((add-after 'install 'unwrap
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; The bin/idris2 calls bin/idris2_app/idris2.so which is
                     ;; the real executable, but it sets LD_LIBRARY_PATH
                     ;; incorrectly.  Remove bin/idris2 and replace it with
                     ;; bin/idris2_app/idris2.so instead.
                     (let* ((out (assoc-ref outputs "out"))
                            (image-base (string-append
                                         out "/bin/idris2_app/idris2"))
                            (image (if (file-exists? image-base)
                                       image-base
                                       ;; For v0.5.1 and older.
                                       (string-append image-base ".so"))))
                       (delete-file (string-append out "/bin/idris2"))
                       (rename-file image (string-append out "/bin/idris2"))
                       (delete-file-recursively (string-append out "/bin/idris2_app"))
                       (delete-file-recursively (string-append out "/lib")))))))
               ((2) `((add-after 'install 'unwrap
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Same as previous, except idris no longer creates a lib directory
                     (let* ((out (assoc-ref outputs "out"))
                            (image-base (string-append
                                         out "/bin/idris2_app/idris2"))
                            (image (if (file-exists? image-base)
                                       image-base
                                       ;; For v0.5.1 and older.
                                       (string-append image-base ".so"))))
                       (delete-file (string-append out "/bin/idris2"))
                       (rename-file image (string-append out "/bin/idris2"))
                       (delete-file-recursively (string-append out "/bin/idris2_app")))))))
               (else '()))
         ,@(if with-bootstrap-shortcut?
               `((replace 'build
                   (lambda* (#:key make-flags #:allow-other-keys)
                     ;; i.e. do not build it using the previous version of
                     ;; Idris, but rather compile the comitted compiler
                     ;; output.
                     (apply invoke "make" "bootstrap" make-flags))))
               '())
         (add-after 'unwrap 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((chez (string-append (assoc-ref inputs "chez-scheme")
                                         "/bin/scheme"))
                    (out (assoc-ref outputs "out"))
                    (exe (string-append out "/bin/" ,name))
                    (version ,idris-version))
               (wrap-program exe
                 `("IDRIS2_PREFIX" = (,out))
                 `("LD_LIBRARY_PATH" prefix (,(string-append
                                               out "/idris2-" version "/lib")))
                 `("CC" = (,',(cc-for-target)))
                 `("CHEZ" = (,chez)))
               (with-directory-excursion (string-append out "/bin/")
                 (let ((versioned-name ,(string-append name "-" version)))
                   (rename-file ,name versioned-name)
                   (symlink versioned-name ,name))))))
         (add-after 'wrap-program 'check
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (let ((invoke-make
                    (lambda (target)
                      (apply invoke "make"
                             "INTERACTIVE="
                             ;; "THREADS=1" ; for reproducible test output
                             (string-append "IDRIS2="
                                            (assoc-ref outputs "out")
                                            "/bin/" ,name)
                             target make-flags))))
               ;; TODO This is something like how it should be handled, but
               ;; the Makefile unconditionally invokes the `testenv` target,
               ;; and thus overwrites the `runtest` script when `make test` is
               ;; invoked.  For now this situation is resolved in the Idris
               ;; Makefile, by explicitly invoking the Idris `runtest` wrapper
               ;; script with an sh prefix.
               ;;
               ;;(invoke-make "testenv")
               ;;(patch-shebang "build/stage2/runtests")
               ,(if ignore-test-failures?
                    '(begin
                       (false-if-exception (invoke-make "test"))
                       #true)
                    '(invoke-make "test"))))))))
    (properties `((hidden? . ,hidden?)))
    (home-page "https://www.idris-lang.org")
    (synopsis "General purpose language with full dependent types")
    (description "Idris is a general purpose language with full dependent
types.  It is compiled, with eager evaluation.  Dependent types allow types to
be predicated on values, meaning that some aspects of a program's behaviour
can be specified precisely in the type.  The language is closely related to
Epigram and Agda.")
    (license license:bsd-3)))

(define-public idris2-0.6.0
  (make-idris-package
   '("v0.6.0"
     "0zphckjnq8j177y09nma66pd30rgqf3hjnhyyqsd44j8rlc00hzk")
   "0.6.0"
   #:ignore-test-failures? #true))

(define-public idris2-git
  (make-idris-package
   '("23694c7e5af11c263d4dba7f40c550d9bc37509c"
     "001gh911ldadw68ymgy87xc4zzqp7l573cf35w7n5lfv4fsggwxh")
   "0.6.0"
   #:bootstrap-idris idris2-0.6.0
   #:idris-version-tag "251-g23694c7e5"
   #:with-bootstrap-shortcut? #false
   #:ignore-test-failures? #true
   #:unwrap 2))
