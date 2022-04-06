(define-module (yellowsquid build agda-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:export (%standard-phases
            agda-build))

(define-record-type* <agda-lib>
  agda-lib make-agda-lib
  agda-lib?
  (name agda-lib-name (default #f))
  (depends agda-lib-depends (default '()))
  (includes agda-lib-includes (default '()))
  (flags agda-lib-flags (default '())))

(define (serialize-agda-lib agda-lib)
  (match-record agda-lib <agda-lib>
    (name depends includes flags)
    (call-with-output-string
      (lambda (port)
        (if name
            (format port "name: ~a~%" name))
        (unless (null? depends)
          (format port "depend:~%")
          (for-each (cut format port "  ~a~%" <>) depends))
        (unless (null? includes)
          (format port "include:~%")
          (for-each (cut format port "  ~a~%" <>) includes))
        (unless (null? flags)
          (format port "flags:~%")
          (for-each (cut format port "  ~a~%" <>) flags))))))

(define (parse-agda-lib filename)
  "Parse an agda-lib file."

  (define (strip-comments line)
    (define (helper chars)
      (match chars
        (() '())
        ((#\- #\- (? char-whitespace?) _ ...) '())
        (((and c (? char-whitespace?)) rest ...)
         (let ((rec (helper rest)))
           (if (null? rec)
               '()
               (cons c rec))))
        ((c rest ...) (cons c (helper rest)))))
    (list->string (helper (string->list line))))

  (define (parse-line line)
    "Parse a line into header and content components."
    (cond
     ((null? line) '())
     ((char-whitespace? (string-ref line 0))
      ;; indented lines are content
      `((content ,(string-trim line))))
     (else (match (string-split line #\:)
             ((head rest)
              (if (string-any char-set:whitespace (string-trim-right head))
                  (throw 'agda-lib-parse 'invalid-head head)
                  `((header ,(string-trim-right head))
                    ,@(if (string-null? (string-trim rest))
                          '()
                          `((content ,(string-trim rest)))))))))))

  (define (group-lines lines)
    "Collect headers and contents into lists."
    (match lines
      (() '())
      ((('header head) rest ...)
       (receive (content rest)
           (span (lambda (line) (eq? 'content (car line))) rest)
         (cons (cons head (map cadr content))
               (group-lines rest))))
      (_ (throw 'agda-lib-parse 'invalid-lines lines))))

  (define (parse-generic lines)
    (group-lines
     (append-map
      (lambda (line) (parse-line (strip-comments line)))
      lines)))

  (define (check-fields! headers)
    (define* (duplicates xs #:optional (= equal?))
      (match xs
        (() #f)
        ((_) #f)
        ((x y rest ...) (or (= x y) (duplicates (cons y rest) =)))))
    (let* ((sorted (sort headers string<)))
      (if (duplicates sorted)
          (throw 'agda-lib-parse 'duplicate-headers headers))))

  (define (parse-name content)
    (match content
      (((and name (? (compose not (cut string-any char-set:whitespace <>)))))
       name)
      (_ (throw 'agda-lib-parse 'invalid-name content))))

  (define (parse-includes content)
    (define (fixup acc)
      (let ((fp (acc '())))
        (if (null? fp)
            '()
            (list (list->string fp)))))

    (define (helper acc chars)
      (match chars
        (() (fixup acc))
        ((#\\ #\  rest ...) (helper (compose acc (cut cons #\  <>)) rest))
        ((#\\ #\\ rest ...) (helper (compose acc (cut cons #\\ <>)) rest))
        ((#\  rest ...) (append (fixup acc) (helper identity rest)))
        ((c rest ...) (helper (compose acc (cut cons c <>)) rest))))

    (append-map
     (compose (cut helper identity <>) string->list)
     content))

  (define (parse-flags content)
    (append-map
     (lambda (content)
       (filter
        (compose not string-null?)
        (string-split content char-set:whitespace)))
     content))

  (define (parse-depends content)
    (append-map
     (lambda (content)
       (filter
        (compose not string-null?)
        (string-split content
                      (char-set-union char-set:whitespace
                                      (char-set #\,)))))
     content))

  (define (build-lib fields)
    (match fields
      (() (agda-lib))
      ((("name" content ...) rest ...)
       (agda-lib
        (inherit (build-lib rest))
        (name (parse-name content))))
      ((("include" content ...) rest ...)
       (agda-lib
        (inherit (build-lib rest))
        (includes (parse-includes content))))
      ((("depend" content ...) rest ...)
       (agda-lib
        (inherit (build-lib rest))
        (depends (parse-depends content))))
      ((("flags" content ...) rest ...)
       (agda-lib
        (inherit (build-lib rest))
        (flags (parse-flags content))))))

  (define (file->line-stream filename)
    (let ((p (open-input-file filename)))
      (stream-let loop ((line (get-line p)))
         (if (eof-object? line)
             (begin (close-input-port p)
                    stream-null)
             (stream-cons line (loop (get-line p)))))))

  (let* ((lines (stream->list (file->line-stream filename)))
         (fields (parse-generic lines)))
    (check-fields! (map car fields))
    (build-lib fields)))

(define (find+parse-agda-lib)
  (match (scandir "." (cut string-suffix? ".agda-lib" <>))
    ((original) (parse-agda-lib original))))

(define* (generate-libraries #:key inputs #:allow-other-keys)
  "Generate a libraries file for a given Agda library."
  (let ((agda-lib (find+parse-agda-lib)))
    (call-with-output-file "libraries"
      (lambda (port)
        (for-each
         (lambda (lib)
           (receive (name _)
               (package-name->name+version lib)
             (format port
                     "~a~%"
                     (search-input-file
                      inputs
                      (format #f "/share/agda/lib/~a.agda-lib" name)))))
         (agda-lib-depends agda-lib))))))

(define* (build #:key everything #:allow-other-keys)
  "Build a given Agda library."
  (invoke "agda"
          "--include-path=."
          "--library-file=libraries"
          everything))

(define* (build-docs #:key outputs readme #:allow-other-keys)
  "Build documentation for a given Agda library."
  (let ((out (assoc-ref outputs "out")))
    (invoke "agda"
            "--include-path=."
            "--library-file=libraries"
            "--html"
            (format #f
                    "--html-dir=~a/share/doc/~a/html"
                    out
                    (strip-store-file-name out))
            readme)))

(define* (install #:key inputs outputs #:allow-other-keys)
  "Install a given Agda library."

  ;;; Taken from (gnu build copy-build-system)
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

  (let* ((my-agda-lib (find+parse-agda-lib))
         (out (assoc-ref outputs "out"))
         (name (or (agda-lib-name my-agda-lib) (strip-store-file-name out)))
         (libdir (string-append out "/share/agda/lib/"))
         (my-agda-lib* (agda-lib
                        (inherit my-agda-lib)
                        (includes
                         (map (cut string-append name "/" <>)
                              (agda-lib-includes my-agda-lib))))))
    (mkdir-p libdir)
    (receive (name* _)
        (package-name->name+version name)
      (call-with-output-file (string-append libdir name* ".agda-lib")
        (cut display
             (serialize-agda-lib my-agda-lib*)
             <>)))
    (receive (_ agda-ver)
        (package-name->name+version
         (strip-store-file-name (assoc-ref inputs "agda")))
      (for-each
       (lambda (src-dir dest-dir)
         (for-each
          (match-lambda
            ((base extension)
             (with-directory-excursion (string-append base src-dir)
               (map
                (cut install-file <> (string-append libdir base dest-dir))
                (find-files "." extension)))))
          `(("" "\\.agda")
            (,(string-append "_build/" agda-ver "/agda/") "\\.agdai"))))
       (agda-lib-includes my-agda-lib)
       (agda-lib-includes my-agda-lib*)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'generate-libraries generate-libraries)
    (replace 'build build)
    (add-after 'build 'build-docs build-docs)
    (delete 'check)
    (replace 'install install)))

(define* (agda-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
