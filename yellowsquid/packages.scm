(define-module (yellowsquid packages)
  #:use-module ((gnu packages) :prefix gnu:)
  #:use-module (srfi srfi-1)
  #:export (search-patches))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/yellowsquid/packages.scm")))
        %load-path))

(define-syntax-rule (search-patches file-name ...)
  (parameterize
      ((gnu:%patch-path
        (cons (string-append %channel-root "/yellowsquid/packages/patches")
              (gnu:%patch-path))))
    (list (gnu:search-patch file-name) ...)))
