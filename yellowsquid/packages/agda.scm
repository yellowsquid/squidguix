(define-module (yellowsquid packages agda)
  #:use-module (gnu packages agda)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public agda-stdlib
  (package
    (name "agda-stdlib")
    (version "1.7.1")
    (home-page "https://github.com/agda/agda-stdlib")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0khl12jvknsvjsq3l5cbp2b5qlw983qbymi1dcgfz9z0b92si3r0"))))
    (build-system haskell-build-system)
    (inputs (list agda))
    (native-inputs (list ghc-filemanip))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-everything
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "dist/build/GenerateEverything/GenerateEverything")))
         (add-after 'generate-everything 'build-agdai
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "agda" "-i." "-isrc" "Everything.agda")))
         (add-after 'build-agdai 'build-doc
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "agda" "-i." "-isrc" "--html" "README.agda")))
         (delete 'haddock)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (install-file file target)
               (let ((dest (string-append target
                                          (if (string-suffix? "/" target)
                                              file
                                              (string-append "/" file)))))
                 (format (current-output-port) "`~a' -> `~a'~%" file dest)
                 (mkdir-p (dirname dest))
                 (let ((stat (lstat file)))
                   (case (stat:type stat)
                     ((symlink)
                      (let ((target (readlink file)))
                        (symlink target dest)))
                     (else
                      (copy-file file dest))))))

             (let ((lib (string-append (assoc-ref outputs "out")
                                       "/share/agda/lib"))
                   (doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/agda-stdlib-1.7.1")))
               (mkdir-p lib)
               (mkdir-p doc)
               (call-with-output-file
                   (string-append lib "/standard-library.agda-lib")
                 (lambda (port)
                   (display "
name: standard-library-1.7.1
include: stdlib\n"
                            port)))
               (copy-recursively "html" (string-append lib "/"))
               (with-directory-excursion "src"
                 (map (lambda (file)
                        (install-file file (string-append lib "/stdlib/")))
                      (find-files "." "\\.agdai?"))))))
         (delete 'register))))
    (synopsis "Standard library for Agda")
    (description "The Agda standard library aims to contain all the tools needed
to write both programs and proofs easily. Whilst the library tries to write
efficient code, ease of proof is prioritised over type-checking and
normalisation performance.")
    (license license:expat)))
