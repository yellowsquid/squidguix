(define-module (yellowsquid build-system agda)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (default-agda
            lower
            agda-build
            agda-build-system))

(define %agda-build-system-modules
  (source-module-closure
   `((yellowsquid build agda-build-system)
     ,@%gnu-build-system-modules)
   #:select?
   (lambda (name)
     (or (guix-module-name? name)
         (eq? (car name) 'yellowsquid)))))

(define (default-agda)
  "Return the default agda package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages agda))))
    (module-ref module 'agda)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (agda (default-agda))
                (agda-inputs '())
                #:allow-other-keys
                #:rest arguments)
  (define private-keywords
    '(#:target #:agda #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,@(standard-packages)))
    (build-inputs `(("agda" ,agda)
                    ,@native-inputs))
    (outputs outputs)
    (build agda-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (agda-build name inputs
                     #:key
                     guile source
                     (outputs '("out"))
                     (search-paths '())
                     (out-of-source? #t)
                     (validate-runpath? #t)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec" "bin" "sbin"))
                     (phases '(@ (yellowsquid build agda-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (target #f)
                     (imported-modules %agda-build-system-modules)
                     (modules '((guix build utils)
                                (yellowsquid build agda-build-system))))
  "Build SOURCE using AGDA, and with INPUTS."

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          #$(with-build-variables inputs outputs
              #~(agda-build #:source #+source
                            #:system #$system
                            #:outputs %outputs
                            #:inputs %build-inputs
                            #:search-paths
                            '#$(sexp->gexp
                                (map search-path-specification->sexp
                                     search-paths))
                            #:phases #$(if (pair? phases)
                                           (sexp->gexp phases)
                                           phases)
                            #:out-of-source? #$out-of-source?
                            #:validate-runpath? #$validate-runpath?
                            #:patch-shebangs? #$patch-shebangs?
                            #:strip-binaries? #$strip-binaries?
                            #:strip-flags #$(sexp->gexp strip-flags)
                            #:strip-directories
                            #$(sexp->gexp strip-directories))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:guile-for-build guile)))

(define agda-build-system
  (build-system
    (name 'agda)
    (description "Build system for Agda libraries.")
    (lower lower)))
