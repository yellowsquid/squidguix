(define-module (yellowsquid build idris-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases idris2-build))

(define (invoke-idris command ipkg-name)
  (invoke "idris2" command (string-append ipkg-name ".ipkg")))

(define* (set-idris-prefix #:key outputs #:allow-other-keys)
  (let ((prefix-dir (string-append (assoc-ref outputs "out") "/lib")))
    (mkdir-p prefix-dir)
    (setenv "IDRIS2_PREFIX" prefix-dir)))

(define* (build #:key ipkg-name #:allow-other-keys)
  (invoke-idris "--build" ipkg-name))

(define* (install #:key ipkg-name install-source? #:allow-other-keys)
  (invoke-idris (if install-source? "--install-with-src" "--install") ipkg-name)
  ;; TODO: install executables correctly
  )

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'set-idris-prefix set-idris-prefix)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)))

(define* (idris2-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Idris2 package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
