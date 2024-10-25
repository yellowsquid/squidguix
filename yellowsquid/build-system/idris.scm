(define-module (yellowsquid build-system idris)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%idris-build-system-modules
            %default-modules
            idris2-build-system))

(define (default-idris)
  ;; Lazily resolve the binding to avoid a circular dependency
  (let ((idris (resolve-interface '(yellowsquid packages idris))))
    (module-ref idris 'idris2)))

(define %idris-build-system-modules
  `((yellowsquid build idris-build-system)
    ,@%default-gnu-imported-modules))

(define %default-modules
  '((yellowsquid build idris-build-system)
    (guix build utils)))

(define* (idris2-build name inputs
                       #:key
                       source
                       (phases '%standard-phases)
                       ipkg-name
                       (install-source? #t)
                       (outputs '("out"))
                       (search-paths '())
                       (system (%current-system))
                       (guile #f)
                       (imported-modules %idris-build-system-modules)
                       (modules %default-modules))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (idris2-build #:name #$name
                        #:source #+source
                        #:system #$system
                        #:ipkg-name #$ipkg-name
                        #:install-source? #$install-source?
                        #:phases #$(if (pair? phases)
                                       (sexp->gexp phases)
                                       phases)
                        #:outputs #$(outputs->gexp outputs)
                        #:inputs #$(input-tuples->gexp inputs)
                        #:search-paths '#$(sexp->gexp
                                           (map search-path-specification->sexp
                                                search-paths))))))


  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      ;; #:target #f
                      #:guile-for-build guile)))

;; TODO: transitive dependencies
(define (expand-idris-inputs inputs)
  (filter-map
   (match-lambda
     ((label (? package? p))
      (list label (package-source p)))
     ((label input)
      (list label input)))
   inputs))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (idris (default-idris))
                (idris-inputs '())
                #:allow-other-keys
                #:rest arguments)
  ;; NOTE: derived from (gnu build-system cargo)
  ;;
  (define private-keywords
    '(#:inputs #:native-inputs #:outputs
      #:idris #:idris-inputs
      #:target
      ;; ,@(if target '() '(#:target))
      ))

  ;; TODO: cross-compilation support
  (and (not target)
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@(expand-idris-inputs idris-inputs)
                        ,@(if target inputs '())))
         (build-inputs `(("idris2" ,idris)
                         ,@native-inputs
                         ,@(if target '() inputs)
                         ,@(if target
                               (standard-cross-packages target 'host)
                               '())))
         (target-inputs `(,@(if target
                                (standard-cross-packages target 'target)
                                '())))
         (outputs outputs)
         (build idris2-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define idris2-build-system
  (build-system
    (name 'idris2)
    (description "Idris2 build system")
    (lower lower)))
