(define-module (yellowsquid packages vosk)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-26))

(define-public openfst-1.7.2
  (package
    (inherit openfst)
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                                  "FstDownload/openfst-" version ".tar.gz"))
              (sha256
               (base32
                "0fqgk8195kz21is09gwzwnrg7fr9526bi9mh4apyskapz27pbhr1"))))))

(define-public openfst-1.7.2+far+lookahead+ngram
  (package
    (inherit openfst-1.7.2)
    (version "1.7.2+far+lookahead+ngram")
    (arguments
     (cons* #:configure-flags
            ''("--enable-far"
               "--enable-ngram-fsts"
               "--enable-lookahead-fsts")
            (package-arguments openfst-1.7.2)))))

(define-public kaldi-501de70
  (let ((commit "501de7066f24375fce5f9558a931381886a294e1")
        (revision "3"))
    (package
      (inherit kaldi)
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kaldi-asr/kaldi")
                      (commit commit)))
                (file-name (git-file-name "kaldi" version))
                (sha256
                 (base32
                  "055znh22c0ppvnpb3av8b3brqff6rcbnyx9l414zn989c8a6h55k"))))
      (inputs (modify-inputs (package-inputs kaldi)
                             (replace "openfst" openfst-1.7.2))))))

(define-public python-vosk
  (package
    (name "python-vosk")
    (version "0.3.32")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alphacep/vosk-api")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1inmw0wd0zvbz83cwk6x8svwycgxzzd9vd0n12xgjc4ypqlk8fk9"))
              (patches
               (parameterize ((%patch-path (map (cut string-append <> "/yellowsquid/packages/patches") %load-path)))
                 (search-patches "vosk-makefile.patch")))))
    (build-system python-build-system)
    (inputs (list openfst-1.7.2+far+lookahead+ngram
                  kaldi-501de70
                  openblas
                  clapack
                  python-cffi))
    (native-inputs (list python-wheel))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-shared-library
           (lambda* (#:key (make-flags '()) (parallel-build? #t) #:allow-other-keys)
             (with-directory-excursion "src"
               (apply invoke "make"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))
         (add-after 'build-shared-library 'goto-python-dir
           (lambda _ (chdir "python"))))))
    (home-page "https://alphacephei.com/vosk/install")
    (synopsis "Speech recognition toolkit")
    (description "Vosk is a speech recognition toolkit with bindings in various
programming languages.")
    (license license:asl2.0)))

(define-public nerd-dictation
  (let ((commit "a574dddb422010389c2f3a5cd84ed685738b7a4c")
        (revision "1"))
    (package
      (name "nerd-dictation")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ideasman42/nerd-dictation")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lx8lgsqgs8qrss583wcyk7n0hv3xyr7wrwnvxhfjvpjx6jflmmx"))
                (patches (parameterize ((%patch-path (map (cut string-append <> "/yellowsquid/packages/patches") %load-path)))
                 (search-patches "nerd-dictation-setup.patch")) )
                (snippet #~(rename-file "nerd-dictation" "nerd_dictation.py"))))
      (build-system python-build-system)
      (arguments
       (list
        #:phases
        #~(begin
            (use-modules (ice-9 format))
            (modify-phases %standard-phases
              (add-before 'build 'patch-xdotool
                (lambda _
                  (substitute* "nerd_dictation.py"
                    (("cmd_base = \"xdotool\"")
                     (format #f "cmd_base = \"~a/bin/xdotool\"" #$(this-package-input "xdotool"))))))))))
      (inputs (list python-cffi python-vosk xdotool))
      (home-page "https://github.com/ideasman42/nerd-dictation")
      (synopsis "Offline text to speech")
      (description "This is a utility that provides simple access speech to text
for using in Linux without being tied to a desktop environment.")
      (license license:gpl3))))
