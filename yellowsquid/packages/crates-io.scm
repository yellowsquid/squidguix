(define-module (yellowsquid packages crates-io)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust-apps)
  #:use-module (nongnu packages mozilla)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public rust-abort-on-panic-1
  (package
    (name "rust-abort-on-panic")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "abort-on-panic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06gr9ryabrcg59b4496c6gwlwxy51b84zgpgap4mq2czxkwliacz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/emk/abort_on_panic-rs")
    (synopsis "Intercept panic! from unsafe locations and abort the process")
    (description
     "Intercept panic! from unsafe locations and abort the process")
    (license license:unlicense)))

(define-public rust-aes-ctr-0.6
  (package
    (name "rust-aes-ctr")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes-ctr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qspjxzrclnb83501595y01dhc0km1ssrbjnwlxhcrsdwp6w6abp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-soft" ,rust-aes-soft-0.6)
                       ("rust-aesni" ,rust-aesni-0.10)
                       ("rust-cipher" ,rust-cipher-0.2)
                       ("rust-ctr" ,rust-ctr-0.6))))
    (home-page "https://github.com/RustCrypto/block-ciphers/tree/master/aes")
    (synopsis "DEPRECATED: replaced by the `aes` crate")
    (description "DEPRECATED: replaced by the `aes` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-afl-0.1
  (package
    (name "rust-afl")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "afl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "033gvl4pnyry34vh5pbr01h78c4zpycn997nc0mmha0ah356271z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-afl-sys" ,rust-afl-sys-0.1)
                       ("rust-gcc" ,rust-gcc-0.3))
       #:cargo-development-inputs (("rust-afl-plugin" ,rust-afl-plugin-0.1)
                                   ("rust-byteorder" ,rust-byteorder-0.3)
                                   ("rust-libc" ,rust-libc-0.2)
                                   ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis "Fuzzing Rust code with american-fuzzy-lop")
    (description "Fuzzing Rust code with american-fuzzy-lop")
    (license license:asl2.0)))

(define-public rust-afl-plugin-0.1
  (package
    (name "rust-afl-plugin")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "afl-plugin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "103ba3r1y285w7vmqfnsya5wdq2v8jlsc9wbrl6hbxsp8z9spkab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-quale" ,rust-quale-1))))
    (home-page "https://github.com/frewsxcv/afl.rs")
    (synopsis "LLVM instrumentation compiler plugin for afl.rs")
    (description "LLVM instrumentation compiler plugin for afl.rs")
    (license license:asl2.0)))

(define-public rust-afl-sys-0.1
  (package
    (name "rust-afl-sys")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "afl-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kipfjhbypflv0bbrvrccm0jan0jampl13d2f68pjxj1ymhcwpid"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/frewsxcv/afl.rs")
    (synopsis "Wrapper around AFL source")
    (description "Wrapper around AFL source")
    (license license:asl2.0)))

(define-public rust-al-sys-0.6
  (package
    (name "rust-al-sys")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "al-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08whlcfrhn4gqi4nbglkdqv5ysdpnvnlsqg51q34q9hh9l7rp3gz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cmake" ,rust-cmake-0.1)
                       ("rust-libloading" ,rust-libloading-0.5)
                       ("rust-rental" ,rust-rental-0.5))))
    (home-page "https://github.com/jpernst/alto.git")
    (synopsis "Raw bindings for OpenAL 1.1")
    (description "Raw bindings for OpenAL 1.1")
    (license (list license:expat license:asl2.0))))

(define-public rust-alsa-0.5
  (package
    (name "rust-alsa")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03nmld6vbpxqg22fy07p51x2rmwl7bzsc7rszhd03gyknd5ldaqb"))))
    (build-system cargo-build-system)
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.21))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-alsa-0.6
  (package
    (name "rust-alsa")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0szx8finhqbffh08fp3bgh4ywz0b572vcdyh4hwyhrfgw8pza5ar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.23))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-alsa-sys-0.3
  (package
    (name "rust-alsa-sys")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))))
    (build-system cargo-build-system)
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/diwic/alsa-sys")
    (synopsis
     "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (description
     "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (license license:expat)))

(define-public rust-alto-3
  (package
    (name "rust-alto")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alto" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rgsdmh346s3rwhzqacjc6nz7jap4dd72c1gfmkaq9sgzh9fhnyp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-al-sys" ,rust-al-sys-0.6)
                       ("rust-lazy-static" ,rust-lazy-static-0.2)
                       ("rust-parking-lot" ,rust-parking-lot-0.4))))
    (home-page "https://github.com/jpernst/alto.git")
    (synopsis
     "Idiomatic interface for OpenAL 1.1 and extensions (including EFX)")
    (description
     "Idiomatic interface for OpenAL 1.1 and extensions (including EFX)")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-logger-0.9
  (package
    (name "rust-android-logger")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "android_logger" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wspwzkn3fakpyz3ka0lh6h4pa66zk9kkvic2q9n70jx30y37hif"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-log-sys" ,rust-android-log-sys-0.2)
                       ("rust-env-logger" ,rust-env-logger-0.7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/Nercury/android_logger-rs")
    (synopsis
     "A logging implementation for `log` which hooks to android log output.
")
    (description
     "This package provides a logging implementation for `log` which hooks to android
log output.")
    (license (list license:expat license:asl2.0))))

(define-public rust-asio-sys-0.2
  (package
    (name "rust-asio-sys")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m8v2jsa4n57s7a7142vs23dkz63dhjxgcjxykd17kvq66v9qqj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.54)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/RustAudio/cpal/")
    (synopsis
     "Low-level interface and binding generation for the steinberg ASIO SDK.")
    (description
     "Low-level interface and binding generation for the steinberg ASIO SDK.")
    (license license:asl2.0)))

(define-public rust-bitflags-0.3
  (package
    (name "rust-bitflags")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bitflags" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07rvj36f3ifi4vlgdm8h2ddv6s1aiglip0l6gn6ln49bjcy29ljg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.
")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-byteorder-0.3
  (package
    (name "rust-byteorder")
    (version "0.3.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "byteorder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xd1vzp1yzw9f9qpm7w3mp9kqxdxwrwzqs4d620n6m4g194smci9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.2)
                                   ("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis
     "Library for reading/writing numbers in big-endian and little-endian.")
    (description
     "Library for reading/writing numbers in big-endian and little-endian.")
    (license (list license:unlicense license:expat))))

(define-public rust-c-vec-2
  (package
    (name "rust-c-vec")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "c_vec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s765fviy10q27b0wmkyk4q728z9v8v5pdlxv5k564y0mlks9mzx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/GuillaumeGomez/c_vec-rs.git")
    (synopsis "Structures to wrap C arrays")
    (description "Structures to wrap C arrays")
    (license (list license:asl2.0 license:expat))))

(define-public rust-cala-core-0.1
  (package
    (name "rust-cala-core")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cala_core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17939zm80lxi0mqsvi98wv2hjasbbh132j5i2m201x30j8dkx4wx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pasts" ,rust-pasts-0.4)
                       ("rust-stdweb" ,rust-stdweb-0.4)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/libcala/cala_core/blob/master/CHANGELOG.md")
    (synopsis "Low-level platform glue for Cala")
    (description "Low-level platform glue for Cala")
    (license (list license:asl2.0 license:zlib))))

(define-public rust-claxon-0.4
  (package
    (name "rust-claxon")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "claxon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1206mxvw833ysg10029apcsjjwly8zmsvksgza5cm7ma4ikzbysb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Tests require downloading FLAC files.
       #:cargo-development-inputs (("rust-hound" ,rust-hound-3)
                                   ("rust-mp4parse" ,rust-mp4parse-0.8)
                                   ("rust-ogg" ,rust-ogg-0.5)
                                   ("rust-time" ,rust-time-0.1)
                                   ("rust-walkdir" ,rust-walkdir-1))))
    (home-page "https://github.com/ruuda/claxon#readme")
    (synopsis "A FLAC decoding library")
    (description "This package provides a FLAC decoding library")
    (license license:asl2.0)))

(define-public rust-color-eyre-0.5
  (package
    (name "rust-color-eyre")
    (version "0.5.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "color-eyre" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dspj58bk57f9hiqlvbz25rik92i4a95iwa2dl4pg8g8grlqa60z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Includes some very brittle tests.
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-color-spantrace" ,rust-color-spantrace-0.1)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-indenter" ,rust-indenter-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-owo-colors" ,rust-owo-colors-1)
                       ("rust-tracing-error" ,rust-tracing-error-0.1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-ansi-parser" ,rust-ansi-parser-0.6)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
                                   ("rust-thiserror" ,rust-thiserror-1)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/yaahc/color-eyre")
    (synopsis
     "An error report handler for panics and eyre::Reports for colorful, consistent, and well formatted error reports for all kinds of errors.")
    (description
     "An error report handler for panics and eyre::Reports for colorful, consistent,
and well formatted error reports for all kinds of errors.")
    (license (list license:expat license:asl2.0))))

(define-public rust-color-spantrace-0.1
  (package
    (name "rust-color-spantrace")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "color-spantrace" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lb2li71zvpxp80nck98gcqbqm3dnmp43pnlvm52z9x8livy9vmn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-owo-colors" ,rust-owo-colors-1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-error" ,rust-tracing-error-0.1))
       #:cargo-development-inputs (("rust-ansi-parser" ,rust-ansi-parser-0.6)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))
    (home-page "https://github.com/yaahc/color-spantrace")
    (synopsis
     "A pretty printer for tracing_error::SpanTrace based on color-backtrace")
    (description
     "This package provides a pretty printer for tracing_error::SpanTrace based on
color-backtrace")
    (license (list license:expat license:asl2.0))))

(define-public rust-cookie-store-0.15
  (package
    (name "rust-cookie-store")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cookie_store" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z0navy9k0ivrdvz492q8c4nhd3iv5l77hwfppskdp1j15607xxk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.15)
                       ("rust-idna" ,rust-idna-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-publicsuffix" ,rust-publicsuffix-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.2)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/pfernie/cookie_store")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description "Implementation of Cookie storage and retrieval")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-0.9.3
  (package
    (name "rust-core-foundation")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-uuid" ,rust-uuid-0.5))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-sys-0.8.3
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p5r2wckarkpkyc4z83q08dwpvcafrb1h6fxfa3qnikh8szww9sq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-coreaudio-rs-0.5
  (package
    (name "rust-coreaudio-rs")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1njbj52xr8l96b9iy1g7xxmr6k2wjv766b0gm39w27rqnhfdv5vz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-0.3)
                       ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "A friendly rust interface for Apple's CoreAudio API.")
    (description
     "This package provides a friendly rust interface for Apple's CoreAudio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-coreaudio-rs-0.10
  (package
    (name "rust-coreaudio-rs")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "125d4zr3n363ybga4629p41ym7iqjfb2alnwrc1zj7zyxch4p28i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "A friendly rust interface for Apple's CoreAudio API.")
    (description
     "This package provides a friendly rust interface for Apple's CoreAudio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-coreaudio-sys-0.1
  (package
    (name "rust-coreaudio-sys")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i6dh6sz1f4xrs3caq7sg20cgw7wsqk6zbqin96d2k1acabih8ri"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (license license:expat)))

(define-public rust-coreaudio-sys-0.2
  (package
    (name "rust-coreaudio-sys")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mx89ynkf2ds1n43hdd6radg2660gp27dw2l90vkqk4zybq3vxqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.53))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (license license:expat)))

(define-public rust-cpal-0.2
  (package
    (name "rust-cpal")
    (version "0.2.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cpal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04xplav35z7n0jpv2xlyg3n9jilna0z6qhs6c1vihpy6b56zd7cq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ole32-sys" ,rust-ole32-sys-0.2)
                       ("rust-winapi" ,rust-winapi-0.2))
       #:cargo-development-inputs (("rust-vorbis" ,rust-vorbis-0.0.14))))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust.")
    (description "Low-level cross-platform audio I/O library in pure Rust.")
    (license license:asl2.0)))

(define-public rust-cpal-0.13
  (package
    (name "rust-cpal")
    (version "0.13.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cpal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05j11vz8rw19gqqvpd48i7wvm6j77v8fwx5lwhlkckqjllv7h4bl"))))
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa" ,rust-alsa-0.6)
                       ("rust-asio-sys" ,rust-asio-sys-0.2)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8.3)
                       ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.10)
                       ("rust-jack" ,rust-jack-0.8)
                       ("rust-jni" ,rust-jni-0.19)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach" ,rust-mach-0.3)
                       ("rust-ndk" ,rust-ndk-0.6)
                       ("rust-ndk-glue" ,rust-ndk-glue-0.6)
                       ("rust-nix" ,rust-nix-0.23)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oboe" ,rust-oboe-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-stdweb" ,rust-stdweb-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-clap" ,rust-clap-3.2.16)
                                   ("rust-hound" ,rust-hound-3)
                                   ("rust-ringbuf" ,rust-ringbuf-0.2))))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust.")
    (description "Low-level cross-platform audio I/O library in pure Rust.")
    (license license:asl2.0)))

(define-public rust-dbus-0.2
  (package
    (name "rust-dbus")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b85dl7y396g8xh1xh89wxnb1fvvf840dar9axavfhhhlq7c385l"))))
    (inputs (list dbus))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Fails to create dbus instance.
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-0.9.6
  (package
    (name "rust-dbus")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dlf2jzf7sjqz437aj9ksj885nzm5685m72jdb94wp1fdpawv2vg"))))
    (inputs (list dbus))
    (native-inputs (list pkg-config))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Fails to create dbus instance.
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libdbus-sys" ,rust-libdbus-sys-0.2.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-crossroads-0.4
  (package
    (name "rust-dbus-crossroads")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-crossroads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18wcwqm1qp8yisxj62vn81fdbh07kq4vxba9823fcd4zv6zinn64"))))
    (build-system cargo-build-system)
    (inputs (list dbus))
    (native-inputs (list pkg-config))
    (arguments
     `(#:tests? #f ;Fails to create dbus instance.
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9.6))))
    (home-page "https://github.com/diwic/dbus-rs/")
    (synopsis "Framework for writing D-Bus method handlers")
    (description "Framework for writing D-Bus method handlers")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-crossroads-0.5
  (package
    (name "rust-dbus-crossroads")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-crossroads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c3f80dychnx1mi8p2rlr7276pywnqyp6ainmzyk6aq1dlli8ham"))))
    (build-system cargo-build-system)
    (inputs (list dbus))
    (native-inputs (list pkg-config))
    (arguments
     `(#:tests? #f ;FIXME: tests fail to compile
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9.6))
       #:cargo-development-inputs (("rust-tokio" ,rust-tokio-1)
                                   ("rust-dbus-tokio" ,rust-dbus-tokio-0.7))))
    (home-page "https://github.com/diwic/dbus-rs/")
    (synopsis "Framework for writing D-Bus method handlers")
    (description "Framework for writing D-Bus method handlers")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-tokio-0.7
  (package
    (name "rust-dbus-tokio")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10112g227iasjiid7y9wrvnmxypfrczcymj2k5yjvcjk1i5ag88j"))))
    (inputs (list dbus))
    (native-inputs (list pkg-config))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Fails to create dbus instance
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-crossroads" ,rust-dbus-crossroads-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-dbus-tree" ,rust-dbus-tree-0.9)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux
for inter-process communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dns-sd-0.1
  (package
    (name "rust-dns-sd")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dns-sd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11r0jymjshfnn3sh2nqjhrikk4r5rr1g36sip9iqy8i0xafm0j6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/plietar/rust-dns-sd")
    (synopsis "Rust binding for dns-sd")
    (description "Rust binding for dns-sd")
    (license license:expat)))

(define-public rust-fetch-unroll-0.3
  (package
    (name "rust-fetch-unroll")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fetch_unroll" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l3cf8fhcrw354hdmjf03f5v4bxgn2wkjna8n0fn8bgplh8b3666"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libflate" ,rust-libflate-1)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-ureq" ,rust-ureq-2))))
    (home-page "https://github.com/katyo/fetch_unroll")
    (synopsis "Simple utilities for fetching and unrolling .tar.gz archives")
    (description
     "Simple utilities for fetching and unrolling .tar.gz archives")
    (license license:asl2.0)))

;; maybe downgrade
(define-public rust-futures-0.3.21
  (package
    (name "rust-futures")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17id2zvn2acny759indn6yj2acfa6lhkwzaidxr2pqfiaigycgzp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--all-features")
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3.21)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-executor" ,rust-futures-executor-0.3.21)
                       ("rust-futures-io" ,rust-futures-io-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21)
                       ("rust-futures-task" ,rust-futures-task-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21))
       #:cargo-development-inputs (("rust-assert-matches" ,rust-assert-matches-1)
                                   ("rust-futures-test" ,rust-futures-test-0.3.21)
                                   ("rust-pin-project" ,rust-pin-project-1)
                                   ("rust-pin-utils" ,rust-pin-utils-0.1)
                                   ("rust-static-assertions" ,rust-static-assertions-1)
                                   ("rust-tokio" ,rust-tokio-0.1))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack-rust-crates 'patch-manifest
                    (lambda _
                      (let ((output-port (open-file "Cargo.toml" "a")))
                        (display "
[dev-dependencies.futures-test]
version = \"0.3.21\"
" output-port)
                        (close output-port))))
                  (replace 'check
                    (lambda* (#:key tests?
                              (cargo-test-flags '("--release"))
                              #:allow-other-keys)
                      (if tests?
                          (begin
                            ;; Abuse RUSTC_BOOTSTRAP to enable nightly features in stable
                            (setenv "RUSTC_BOOTSTRAP" "1")
                            (apply invoke "cargo" "test" cargo-test-flags)
                            (unsetenv "RUSTC_BOOTSTRAP"))))))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.
")
    (description
     "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-channel-0.3.21
  (package
    (name "rust-futures-channel")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-channel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0420lz2fmxa356ax1rp2sqi7b27ykfhvq4w9f1sla4hlp7j3q263"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.
")
    (description "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-core-0.3.21
  (package
    (name "rust-futures-core")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lqhc6mqklh5bmkpr77p42lqwjj8gaskk5ba2p3kl1z4nw2gs28c"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-executor-0.3.21
  (package
    (name "rust-futures-executor")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-executor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19mq96kwgf06axgdc2fbrjhqzdnxww9vw6cz8b82gqr9z86bj84l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-task" ,rust-futures-task-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Executors for asynchronous tasks based on the futures-rs library.
")
    (description
     "Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-io-0.3.21
  (package
    (name "rust-futures-io")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-io" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0swn29fysas36ikk5aw55104fi98117amvgxw9g96pjs5ab4ah7w"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.
")
    (description
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the
futures-rs library.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-macro-0.3.21
  (package
    (name "rust-futures-macro")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04pmj5xfk5rdhlj69wc7w3zvdg3xardg8srig96lszrk00wf3h9k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.
")
    (description "The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-sink-0.3.21
  (package
    (name "rust-futures-sink")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-sink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s58gx5yw1a21xviw2qgc0wzk225vgn4kbzddrp141m3kw9kw5i1"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-task-0.3.21
  (package
    (name "rust-futures-task")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-task" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0skpiz2ljisywajv79p70yapfwhkqhb39wxy3f09v47mdfbnmijp"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-test-0.3.21
  (package
    (name "rust-futures-test")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-test" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "167fm0bz93w57f2yns7y4y45ax8g7kbrbs3rvzb5vcxzvdwr6glc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-executor" ,rust-futures-executor-0.3.21)
                       ("rust-futures-io" ,rust-futures-io-0.3.21)
                       ("rust-futures-macro" ,rust-futures-macro-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21)
                       ("rust-futures-task" ,rust-futures-task-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Common utilities for testing components built off futures-rs.
")
    (description
     "Common utilities for testing components built off futures-rs.")
    (license (list license:expat license:asl2.0))))

;; maybe downgrade
(define-public rust-futures-util-0.3.21
  (package
    (name "rust-futures-util")
    (version "0.3.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sh3wqi8p36csjffy0irq8nlx9shqxp7z4dsih6bknarsvaspdyq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3.21)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-io" ,rust-futures-io-0.3.21)
                       ("rust-futures-macro" ,rust-futures-macro-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21)
                       ("rust-futures-task" ,rust-futures-task-0.3.21)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Common utilities and extension traits for the futures-rs library.
")
    (description
     "Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-0.16
  (package
    (name "rust-gstreamer")
    (version "0.16.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0crghv0qh0lys26712j3dshdwnvq2znnsyxldrzf72ihzzvx1xcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3.21)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-glib" ,rust-glib-0.10)
                       ("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.10)
                       ("rust-gstreamer-rs-lgpl-docs" ,rust-gstreamer-rs-lgpl-docs-0.16)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-muldiv" ,rust-muldiv-0.2)
                       ("rust-num-rational" ,rust-num-rational-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pretty-hex" ,rust-pretty-hex-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer")
    (description "Rust bindings for GStreamer")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-app-0.16
  (package
    (name "rust-gstreamer-app")
    (version "0.16.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-app" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03mg4ywgba0q02zdqfamzxv7887bab2az32xhzg3x31kf618i06c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21)
                       ("rust-glib" ,rust-glib-0.10)
                       ("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.10)
                       ("rust-gstreamer" ,rust-gstreamer-0.16)
                       ("rust-gstreamer-app-sys" ,rust-gstreamer-app-sys-0.9)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.16)
                       ("rust-gstreamer-rs-lgpl-docs" ,rust-gstreamer-rs-lgpl-docs-0.16)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer App library")
    (description "Rust bindings for GStreamer App library")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-app-sys-0.9
  (package
    (name "rust-gstreamer-app-sys")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-app-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gbyp1jsqqs3x2bfjkbdfsb5kfb4zafwzvxr52w36ywybhkn8gw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.9)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstapp-1.0")
    (description "FFI bindings to libgstapp-1.0")
    (license license:expat)))

(define-public rust-gstreamer-base-0.16
  (package
    (name "rust-gstreamer-base")
    (version "0.16.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-base" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kxgwvv0qh1dgvd08nxfhhkjp36z9fxrf3x1nps11jsrdz2h3zds"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-glib" ,rust-glib-0.10)
                       ("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.10)
                       ("rust-gstreamer" ,rust-gstreamer-0.16)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.9)
                       ("rust-gstreamer-rs-lgpl-docs" ,rust-gstreamer-rs-lgpl-docs-0.16)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base library")
    (description "Rust bindings for GStreamer Base library")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-base-sys-0.9
  (package
    (name "rust-gstreamer-base-sys")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-base-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xgf5dl7507hn9mvz46ffjj3y2shpl1gc4l6w8d0l5kf5pfbddx4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.10)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstbase-1.0")
    (description "FFI bindings to libgstbase-1.0")
    (license license:expat)))

(define-public rust-gstreamer-rs-lgpl-docs-0.16
  (package
    (name "rust-gstreamer-rs-lgpl-docs")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-rs-lgpl-docs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06k4mr6478463q7hhsl4a252nhzf0b2qjqla3xhlh20ma0hz8912"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "LGPL-licensed docs for gstreamer-rs crates")
    (description "LGPL-licensed docs for gstreamer-rs crates")
    (license license:lgpl2.0)))

(define-public rust-gstreamer-sys-0.9
  (package
    (name "rust-gstreamer-sys")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07b09f2acaiczjl3725dhraym935yns8x2jziiqza6nhh901a7zw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.10)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstreamer-1.0")
    (description "FFI bindings to libgstreamer-1.0")
    (license license:expat)))

(define-public rust-hound-3
  (package
    (name "rust-hound")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hound" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cadkxzdsb3bxwzri6r6l78a1jy9j0jxrfwmh34gjadvbnyws4sd"))))
    (build-system cargo-build-system)
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (arguments
     `(#:cargo-development-inputs (("rust-cpal" ,rust-cpal-0.2))))
    (home-page "https://github.com/ruuda/hound")
    (synopsis "A wav encoding and decoding library")
    (description "This package provides a wav encoding and decoding library")
    (license license:asl2.0)))

(define-public rust-hyper-proxy-0.9
  (package
    (name "rust-hyper-proxy")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper-proxy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k3mpq6d4rhz58dam1757sav14j32n39q8x37wjgpz943f4mm0fa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.22)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.5)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-webpki" ,rust-webpki-0.21)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.21))))
    (home-page "https://github.com/tafia/hyper-proxy")
    (synopsis "A proxy connector for Hyper-based applications")
    (description
     "This package provides a proxy connector for Hyper-based applications")
    (license license:expat)))

(define-public rust-if-addrs-0.6
  (package
    (name "rust-if-addrs")
    (version "0.6.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "if-addrs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pkkkwm9znn07xq9s6glf8lxzn2rdxvy8kwkw6czrw64ywhy8wr2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-if-addrs-sys" ,rust-if-addrs-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "Return interface IP addresses on Posix and windows systems")
    (description "Return interface IP addresses on Posix and windows systems")
    (license (list license:expat license:bsd-3))))

(define-public rust-if-addrs-sys-0.3
  (package
    (name "rust-if-addrs-sys")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "if-addrs-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1skrzs79rafv185064p44r0k1va9ig4bfnpbwlvyhxh4g3fvjx6y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "if_addrs sys crate")
    (description "if_addrs sys crate")
    (license (list license:expat license:bsd-3))))

(define-public rust-jack-0.6
  (package
    (name "rust-jack")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sk8rxiq3h2y33hdq15hnf915l8rv09zl9sgg2vjysvypms4ksrd"))))
    (inputs (list jack-1))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-jack-sys" ,rust-jack-sys-0.2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests?
                              (cargo-test-flags '("--release"))
                              #:allow-other-keys)
                      (if tests?
                          (begin
                            (let ((pid (primitive-fork)))
                              (if (= pid 0)
                                  (execlp "sh" "sh" "./dummy_jack_server.sh")
                                  (begin
                                    (setenv "RUST_TEST_THREADS" "1")
                                    (apply invoke "cargo" "test"
                                           cargo-test-flags)
                                    (unsetenv "RUST_TEST_THREADS")
                                    (kill pid SIGINT)))))))))))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK.")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-jack-0.7
  (package
    (inherit rust-jack-0.6)
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append (package-name rust-jack-0.6) "-"
                                        version ".tar.gz"))
              (sha256
               (base32
                "1r7bgfpbph3fl9xyp4i9qffcc4h923dcs7d967mpir13lxg216yp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments rust-jack-0.6)
       ((#:cargo-inputs inputs)
        (cons `("rust-log" ,rust-log-0.4) inputs))))))

(define-public rust-jack-0.8
  (package
    (inherit rust-jack-0.7)
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append (package-name rust-jack-0.6) "-"
                                        version ".tar.gz"))
              (sha256
               (base32
                "0lz10s0n2gy128m65pf96is9ip00vfgvnkfja0y9ydmv24pw2ajx"))))))

(define-public rust-jack-sys-0.2
  (package
    (name "rust-jack-sys")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h9c9za19nyr1prx77gkia18ia93f73lpyjdiyrvmhhbs79g54bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.6)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API.")
    (description "Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jni-0.19
  (package
    (name "rust-jni")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jni" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v0pn0i1wb8zp4wns4l8hz9689hqsplv7iba7hylaznvwg11ipy6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cesu8" ,rust-cesu8-1)
                       ("rust-combine" ,rust-combine-4)
                       ("rust-jni-sys" ,rust-jni-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/jni-rs/jni-rs")
    (synopsis "Rust bindings to the JNI")
    (description "Rust bindings to the JNI")
    (license (list license:expat license:asl2.0))))

(define-public rust-keychain-services-0.1
  (package
    (name "rust-keychain-services")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "keychain-services" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a5hw0nd49wxrgfmxd1scq80q9zw8lsd7ddcl7grgzri7filnvbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation" ,rust-core-foundation-0.6)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-failure-derive" ,rust-failure-derive-0.1)
                       ("rust-zeroize" ,rust-zeroize-0.4))))
    (home-page "https://keychain-services.rs/")
    (synopsis
     "                Rust access to macOS Keychain Services, including TouchID-guarded
                access to cryptographic keys stored in the Secure Enclave
                Processor (SEP).
                ")
    (description
     "                Rust access to macOS Keychain Services, including
TouchID-guarded                 access to cryptographic keys stored in the
Secure Enclave                 Processor (SEP).                  ")
    (license license:asl2.0)))

(define-public rust-keyring-0.10
  (package
    (name "rust-keyring")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "keyring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xs73nygvd6gb5mnisdxngqdh0i5vmbg0id8k1l0nfv6d8aqp6m4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Requires a secret service instance
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-secret-service" ,rust-secret-service-2)
                       ("rust-security-framework" ,rust-security-framework-2.6)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-2)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rpassword" ,rust-rpassword-5)
                                   ("rust-serial-test" ,rust-serial-test-0.5)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/hwchen/keyring-rs")
    (synopsis "Cross-platform library for managing passwords/credentials")
    (description "Cross-platform library for managing passwords/credentials")
    (license (list license:expat license:asl2.0))))

(define-public rust-lewton-0.10
  (package
    (name "rust-lewton")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lewton" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c60fn004awg5c3cvx82d6na2pirf0qdz9w3b93mbcdakbglhyvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-ogg" ,rust-ogg-0.8)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs (("rust-alto" ,rust-alto-3)
                                   ("rust-ogg" ,rust-ogg-0.8))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust vorbis decoder")
    (description "Pure Rust vorbis decoder")
    (license (list license:expat license:asl2.0))))

(define-public rust-libdbus-sys-0.2.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdbus-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libmdns-0.6
  (package
    (name "rust-libmdns")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libmdns" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0siaarjhds4dxrxn7qgz4gnfb2hzcmsdcndd3rnkr1rfs2j8bhgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-hostname" ,rust-hostname-0.3)
                       ("rust-if-addrs" ,rust-if-addrs-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.8))))
    (home-page "https://github.com/librespot-org/libmdns")
    (synopsis
     "mDNS Responder library for building discoverable LAN services in Rust")
    (description
     "mDNS Responder library for building discoverable LAN services in Rust")
    (license license:expat)))

(define-public rust-libpulse-simple-binding-2
  (package
    (name "rust-libpulse-simple-binding")
    (version "2.25.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-simple-binding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mdws3gr5rvvb64hf77mkq17a6vzs6hryf8616v8r939zlfimgvw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libpulse-binding" ,rust-libpulse-binding-2)
                       ("rust-libpulse-simple-sys" ,rust-libpulse-simple-sys-1)
                       ("rust-libpulse-sys" ,rust-libpulse-sys-1.19))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis
     "A Rust language binding for the PulseAudio libpulse-simple library.")
    (description
     "This package provides a Rust language binding for the PulseAudio libpulse-simple
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-simple-sys-1
  (package
    (name "rust-libpulse-simple-sys")
    (version "1.19.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-simple-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hbws8gj45lanvd0xr8d02m60n1247jcyq275ilhjj53kipzjwvw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libpulse-sys" ,rust-libpulse-sys-1.19)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis
     "FFI bindings for the PulseAudio libpulse-simple system library.")
    (description
     "FFI bindings for the PulseAudio libpulse-simple system library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-sys-1.19
  (package
    (name "rust-libpulse-sys")
    (version "1.19.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c3wybzyyarzagz0fy2vwflc3r15d6cyfdp16ijnx8z2xz86n7lr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "FFI bindings for the PulseAudio libpulse system library.")
    (description "FFI bindings for the PulseAudio libpulse system library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-audio-0.2
  (package
    (name "rust-librespot-audio")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-audio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h25cinnyfw15farjv7lwcyqppd6gv0h5fkyac00dn06w79qj3ps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aes-ctr" ,rust-aes-ctr-0.6)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-lewton" ,rust-lewton-0.10)
                       ("rust-librespot-core" ,rust-librespot-core-0.2)
                       ("rust-librespot-tremor" ,rust-librespot-tremor-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ogg" ,rust-ogg-0.8)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-vorbis" ,rust-vorbis-0.0.14)
                       ("rust-zerocopy" ,rust-zerocopy-0.3))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio fetching and processing logic for librespot")
    (description "The audio fetching and processing logic for librespot")
    (license license:expat)))

(define-public rust-librespot-connect-0.2
  (package
    (name "rust-librespot-connect")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-connect" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1272nnzk9xhyc2jvakfrl3451pql65mnxcc6xkqpjiypr3b5yhk9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aes-ctr" ,rust-aes-ctr-0.6)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-dns-sd" ,rust-dns-sd-0.1)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-libmdns" ,rust-libmdns-0.6)
                       ("rust-librespot-core" ,rust-librespot-core-0.2)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.2)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-2.14)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery and Spotify Connect logic for librespot")
    (description "The discovery and Spotify Connect logic for librespot")
    (license license:expat)))

(define-public rust-librespot-core-0.2
  (package
    (name "rust-librespot-core")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0j1jjqf89dlsrlfjj9yy111dvlfi9zq3789ck0f57x9iw7f70gnz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.6)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-proxy" ,rust-hyper-proxy-0.9)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.8)
                       ("rust-priority-queue" ,rust-priority-queue-1)
                       ("rust-protobuf" ,rust-protobuf-2.14)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-shannon" ,rust-shannon-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-vergen" ,rust-vergen-3))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The core functionality provided by librespot")
    (description "The core functionality provided by librespot")
    (license license:expat)))

(define-public rust-librespot-metadata-0.2
  (package
    (name "rust-librespot-metadata")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ixg23ck98rjnshp6xil4lbig1sxpbg0blvyy6pdrnq1v7jpq5c4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-librespot-core" ,rust-librespot-core-0.2)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-2.14))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata logic for librespot")
    (description "The metadata logic for librespot")
    (license license:expat)))

(define-public rust-librespot-playback-0.2
  (package
    (name "rust-librespot-playback")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-playback" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mksjvbwq7y01x54i7aqdbl7g48m7s060xki10qa9qd7hvwkvjjr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa" ,rust-alsa-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cpal" ,rust-cpal-0.13)
                       ("rust-futures-executor" ,rust-futures-executor-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-glib" ,rust-glib-0.10)
                       ("rust-gstreamer" ,rust-gstreamer-0.16)
                       ("rust-gstreamer-app" ,rust-gstreamer-app-0.16)
                       ("rust-jack" ,rust-jack-0.6)
                       ("rust-libpulse-binding" ,rust-libpulse-binding-2)
                       ("rust-libpulse-simple-binding" ,rust-libpulse-simple-binding-2)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.2)
                       ("rust-librespot-core" ,rust-librespot-core-0.2)
                       ("rust-librespot-metadata" ,rust-librespot-metadata-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-portaudio-rs" ,rust-portaudio-rs-0.3)
                       ("rust-rodio" ,rust-rodio-0.13)
                       ("rust-sdl2" ,rust-sdl2-0.34)
                       ("rust-shell-words" ,rust-shell-words-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-zerocopy" ,rust-zerocopy-0.3))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio playback logic for librespot")
    (description "The audio playback logic for librespot")
    (license license:expat)))

(define-public rust-librespot-protocol-0.2
  (package
    (name "rust-librespot-protocol")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pa3j2csmimf0l2yfdxp9ijmgm8ngxadim801jrh43xxqgj3nx8w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glob" ,rust-glob-0.3)
                       ("rust-protobuf" ,rust-protobuf-2.14)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-2.14)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2.14))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "The protobuf logic for communicating with Spotify servers")
    (license license:expat)))

(define-public rust-librespot-tremor-0.2
  (package
    (name "rust-librespot-tremor")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-tremor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zmld16zawvn7ayrf318lwdr2d7awn4bk9s0d6kpim0mz6zjbxcp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "")
    (synopsis "Rust bindings to tremor")
    (description "Rust bindings to tremor")
    (license license:expat)))

(define-public rust-minimp3-0.5
  (package
    (name "rust-minimp3")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wj3nzj1swnvwsk3a4a3hkfj1d21jsi7babi40wlrxzbbzvkhm4q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-minimp3-sys" ,rust-minimp3-sys-0.3)
                       ("rust-slice-deque" ,rust-slice-deque-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-minimp3-sys-0.3
  (package
    (name "rust-minimp3-sys")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "144vmf3s89kad0smjprzigcp2c9r5dm95n4ydilrbp399irp6772"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-mio-0.6.23
  (package
    (name "rust-mio")
    (version "0.6.23")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i2c1vl8lr45apkh8xbh9k56ihfsmqff5l7s2fya7whvp7sndzaa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-fuchsia-zircon" ,rust-fuchsia-zircon-0.3)
                       ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys-0.3)
                       ("rust-iovec" ,rust-iovec-0.1)
                       ("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miow" ,rust-miow-0.2.2)
                       ("rust-net2" ,rust-net2-0.2.37)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO")
    (license license:expat)))

(define-public rust-miow-0.2.2
  (package
    (name "rust-miow")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "miow" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kcl8rnv0bhiarcdakik670w8fnxzlxhi1ys7152sck68510in7b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
                       ("rust-net2" ,rust-net2-0.2)
                       ("rust-winapi" ,rust-winapi-0.2)
                       ("rust-ws2-32-sys" ,rust-ws2-32-sys-0.2))))
    (home-page "https://github.com/yoshuawuyts/miow")
    (synopsis
     "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O
abstractions.
")
    (description
     "This package provides a zero overhead I/O library for Windows, focusing on IOCP
and Async I/O abstractions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mp4parse-0.8
  (package
    (name "rust-mp4parse")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mp4parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pqsv1zm5x9nnkjrv25qv2yg6ba4dny6bsy6cfdzrdm8kwg2r54r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-abort-on-panic" ,rust-abort-on-panic-1)
                       ("rust-afl" ,rust-afl-0.1)
                       ("rust-afl-plugin" ,rust-afl-plugin-0.1)
                       ("rust-bitreader" ,rust-bitreader-0.3)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-num-traits" ,rust-num-traits-0.1))
       #:cargo-development-inputs (("rust-test-assembler" ,rust-test-assembler-0.1))))
    (home-page "https://github.com/mozilla/mp4parse-rust")
    (synopsis "Parser for ISO base media file format (mp4)")
    (description "Parser for ISO base media file format (mp4)")
    (license license:mpl2.0)))

(define-public rust-muldiv-0.2
  (package
    (name "rust-muldiv")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "muldiv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "014jlry2l2ph56mp8knw65637hh49q7fmrraim2bx9vz0a638684"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sdroege/rust-muldiv")
    (synopsis
     "Provides a trait for numeric types to perform combined multiplication and
division with overflow protection
")
    (description
     "This package provides a trait for numeric types to perform combined
multiplication and division with overflow protection")
    (license license:expat)))

(define-public rust-ndk-0.3
  (package
    (name "rust-ndk")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1avk39s8w21inkzq09x83yghgq3v8rmhrycba8prg6rif8hk5547"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jni" ,rust-jni-0.14)
                       ("rust-jni-glue" ,rust-jni-glue-0.0)
                       ("rust-jni-sys" ,rust-jni-sys-0.3)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.2)
                       ("rust-num-enum" ,rust-num-enum-0.5)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Safe Rust bindings to the Android NDK")
    (description "Safe Rust bindings to the Android NDK")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-0.6
  (package
    (name "rust-ndk")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m1dfjw35qpys1hr4qib6mm3zacd01k439l7cx5f7phd0dzcfci0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-jni" ,rust-jni-0.18)
                       ("rust-jni-glue" ,rust-jni-glue-0.0)
                       ("rust-jni-sys" ,rust-jni-sys-0.3)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.3)
                       ("rust-num-enum" ,rust-num-enum-0.5)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Safe Rust bindings to the Android NDK")
    (description "Safe Rust bindings to the Android NDK")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-context-0.1
  (package
    (name "rust-ndk-context")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk-context" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Handles for accessing Android APIs")
    (description "Handles for accessing Android APIs")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-glue-0.3
  (package
    (name "rust-ndk-glue")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk-glue" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11cksaj1f2sy4dwqyssrvvhbnd86zajfvm17bj81rb2i9p1g1jn5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-logger" ,rust-android-logger-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ndk" ,rust-ndk-0.3)
                       ("rust-ndk-macro" ,rust-ndk-macro-0.2)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.2))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Startup code for android binaries")
    (description "Startup code for android binaries")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-glue-0.6
  (package
    (name "rust-ndk-glue")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk-glue" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pz6cdmmlzsb2jhrfvkma5d5vw2i331dlghqnkk2c0l6hdxll30d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-logger" ,rust-android-logger-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ndk" ,rust-ndk-0.6)
                       ("rust-ndk-context" ,rust-ndk-context-0.1)
                       ("rust-ndk-macro" ,rust-ndk-macro-0.3)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.3))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Startup code for android binaries")
    (description "Startup code for android binaries")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-sys-0.3
  (package
    (name "rust-ndk-sys")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndk-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15zsq4p6k5asf4mc0rknd8cz9wxrwvi50qdspgf87qcfgkknlnkf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jni-sys" ,rust-jni-sys-0.3))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "FFI bindings for the Android NDK")
    (description "FFI bindings for the Android NDK")
    (license (list license:expat license:asl2.0))))

(define-public rust-net2-0.2.37
  (package
    (name "rust-net2")
    (version "0.2.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "net2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bk8jp0i12gvhrlaqbfq19ancja70r1rg3sywbhjl0385g8k05ir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/deprecrated/net2-rs")
    (synopsis
     "Extensions to the standard library's networking types as proposed in RFC 1158.
")
    (description
     "Extensions to the standard library's networking types as proposed in RFC 1158.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nix-0.22.3
  (package
    (name "rust-nix")
    (version "0.22.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bsgc8vjq07a1wg9vz819bva3dvn58an4r87h80dxrfqkqanz4g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.6))
       #:cargo-development-inputs (("rust-assert-impl" ,rust-assert-impl-0.1)
                                   ("rust-caps" ,rust-caps-0.5)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-semver" ,rust-semver-1)
                                   ("rust-sysctl" ,rust-sysctl-0.1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))

(define-public rust-ntest-0.7
  (package
    (name "rust-ntest")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ntest" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i4xsvx52hmcnga2xbjl74hdylz4jy8bc2swcichlvw1di4lwm2w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ntest-proc-macro-helper" ,rust-ntest-proc-macro-helper-0.7)
                       ("rust-ntest-test-cases" ,rust-ntest-test-cases-0.7)
                       ("rust-ntest-timeout" ,rust-ntest-timeout-0.7))
       #:cargo-development-inputs (("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/becheran/ntest")
    (synopsis
     "Testing framework for rust which enhances the built-in library with some useful features.")
    (description
     "Testing framework for rust which enhances the built-in library with some useful
features.")
    (license license:expat)))

(define-public rust-ntest-proc-macro-helper-0.7
  (package
    (name "rust-ntest-proc-macro-helper")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ntest-proc-macro-helper" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lkyfx97aynbm7cnhzyc9cr0rpq1xzng1hwmzizbf1a6855y6llg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/becheran/ntest")
    (synopsis
     "Provide helper functions for the procedural macros used in ntest.")
    (description
     "Provide helper functions for the procedural macros used in ntest.")
    (license license:expat)))

(define-public rust-ntest-test-cases-0.7
  (package
    (name "rust-ntest-test-cases")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ntest-test-cases" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ghal2rb03cnj7ciqgdq0dvifdf8qp2hnmi9z1ip1j5b02s1xa4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/becheran/ntest")
    (synopsis "Test cases for ntest framework.")
    (description "Test cases for ntest framework.")
    (license license:expat)))

(define-public rust-ntest-timeout-0.7
  (package
    (name "rust-ntest-timeout")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ntest-timeout" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08v3r6hggh43qabl887pkz88k6lg6hrc62mppxyabb0pw44v03di"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ntest-proc-macro-helper" ,rust-ntest-proc-macro-helper-0.7)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/becheran/ntest")
    (synopsis "Timeout attribute for the ntest framework.")
    (description "Timeout attribute for the ntest framework.")
    (license license:expat)))

(define-public rust-oboe-0.4
  (package
    (name "rust-oboe")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oboe" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lh20l8b4lx5h9a7lpf9n66z47sh2508w7x2203hsklvw7rchqr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jni" ,rust-jni-0.19)
                       ("rust-ndk" ,rust-ndk-0.6)
                       ("rust-ndk-glue" ,rust-ndk-glue-0.6)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oboe-sys" ,rust-oboe-sys-0.4))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Safe interface for oboe an android library for low latency audio IO")
    (description
     "Safe interface for oboe an android library for low latency audio IO")
    (license license:asl2.0)))

(define-public rust-oboe-sys-0.4
  (package
    (name "rust-oboe-sys")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oboe-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gcl494yy880h2gfgsbdd32g2h0s1n94v58j5hil9mrf6yvsnw1k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-fetch-unroll" ,rust-fetch-unroll-0.3))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Unsafe bindings for oboe an android library for low latency audio IO")
    (description
     "Unsafe bindings for oboe an android library for low latency audio IO")
    (license license:asl2.0)))

(define-public rust-ogg-0.5
  (package
    (name "rust-ogg")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sg29xkg332s3cqj7axawvnzv7nfldk7f853c2xa1a006d1yb39z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "Ogg container decoder and encoder written in pure Rust")
    (license license:bsd-3)))

(define-public rust-ogg-0.8
  (package
    (name "rust-ogg")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vjxmqcv9252aj8byy70iy2krqfjknfcxg11lcyikj11pzlb8lb9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "Ogg container decoder and encoder written in pure Rust")
    (license license:bsd-3)))

(define-public rust-ogg-sys-0.0.9
  (package
    (name "rust-ogg-sys")
    (version "0.0.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cpx6n5ndh2d59g43l6rj3myzi5jsc0n6rldpx0impqp5qbqqnx9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "https://github.com/tomaka/ogg-sys")
    (synopsis "FFI for libogg, the media container.")
    (description "FFI for libogg, the media container.")
    (license license:expat)))

(define-public rust-ole32-sys-0.2
  (package
    (name "rust-ole32-sys")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ole32-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "134xg38xicrqynx4pfjfxnpp8x83m3gqw5j3s8y27rc22w14jb2x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.2)
                       ("rust-winapi-build" ,rust-winapi-build-0.1))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis
     "Contains function definitions for the Windows API library ole32. See winapi for types and constants.")
    (description
     "Contains function definitions for the Windows API library ole32.  See winapi for
types and constants.")
    (license license:expat)))

(define-public rust-owning-ref-0.3
  (package
    (name "rust-owning-ref")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "owning-ref" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dqgf5hwbmvkf2ffbik5xmhvaqvqi6iklhwk9x47n0wycd0lzy6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/Kimundi/owning-ref-rs")
    (synopsis
     "A library for creating references that carry their owner with them.")
    (description
     "This package provides a library for creating references that carry their owner
with them.")
    (license license:expat)))

(define-public rust-owo-colors-1
  (package
    (name "rust-owo-colors")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "owo-colors" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rybl2lvhaycpkpaq45099idp5ny7nv4sqsafz0cvfqw1wjfy9vz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2))))
    (home-page "https://github.com/jam1garner/owo-colors")
    (synopsis "Zero-allocation terminal colors that'll make people go owo")
    (description "Zero-allocation terminal colors that'll make people go owo")
    (license license:expat)))

(define-public rust-parking-lot-0.4
  (package
    (name "rust-parking-lot")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parking-lot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ph0kv3dfcxpjbi83wkzammqb7lm95j8in7w7hz17hgkjxdqz78l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-owning-ref" ,rust-owning-ref-0.3)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.2))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-parking-lot-core-0.2
  (package
    (name "rust-parking-lot-core")
    (version "0.2.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parking-lot-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15qv920lymlbbwzzkkhn86i3q2pbvhlb7hlacfpai8fmpk1i5w6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-petgraph" ,rust-petgraph-0.4)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-smallvec" ,rust-smallvec-0.6)
                       ("rust-thread-id" ,rust-thread-id-3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "An advanced API for creating custom synchronization primitives.")
    (description
     "An advanced API for creating custom synchronization primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-password-hash-0.2
  (package
    (name "rust-password-hash")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "password-hash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rr4kd52ld978a2xhcvlc54p1d92yhxl9kvbajba7ia6rs5b5q3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/password-hash")
    (synopsis
     "Traits which describe the functionality of password hashing algorithms,
as well as a `no_std`-friendly implementation of the PHC string format
(a well-defined subset of the Modular Crypt Format a.k.a. MCF)
")
    (description
     "Traits which describe the functionality of password hashing algorithms, as well
as a `no_std`-friendly implementation of the PHC string format (a well-defined
subset of the Modular Crypt Format a.k.a.  MCF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-pasts-0.4
  (package
    (name "rust-pasts")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pasts" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11rdczdhpazclhkbbjafv5nd9ybll9a110crhh67si0p5rdc6mz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/AldaronLau/pasts/blob/main/CHANGELOG.md")
    (synopsis "Minimal and simpler alternative to the futures crate.")
    (description "Minimal and simpler alternative to the futures crate.")
    (license (list license:asl2.0 license:zlib))))

(define-public rust-pbkdf2-0.8
  (package
    (name "rust-pbkdf2")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ykgicvyjm41701mzqhrfmiz5sm5y0zwfg6csaapaqaf49a54pyr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-crypto-mac" ,rust-crypto-mac-0.11)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-password-hash" ,rust-password-hash-0.2)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-sha2" ,rust-sha2-0.9))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-pkg-config-0.3.24
  (package
    (name "rust-pkg-config")
    (version "0.3.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkg-config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ghcyjp5537r7qigmgl3dj62j01arlpddaq93a3i414v3iskz2aq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis
     "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.
")
    (description
     "This package provides a library to run the pkg-config system tool at build time
in order to be used in Cargo build scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-portaudio-rs-0.3
  (package
    (name "rust-portaudio-rs")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qnmc7amk0fzbcs985ixv0k4955f0fmpkhrl9ps9pk3cz7pvbdnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-portaudio-sys" ,rust-portaudio-sys-0.1))))
    (home-page "")
    (synopsis "PortAudio bindings for Rust")
    (description "PortAudio bindings for Rust")
    (license license:expat)))

(define-public rust-portaudio-sys-0.1
  (package
    (name "rust-portaudio-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdpywirpr1kqkbak7hnny62gmsc93qgc3ij3j2zskrvjpxa952i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "")
    (synopsis "Bindings to PortAudio")
    (description "Bindings to PortAudio")
    (license license:expat)))

(define-public rust-priority-queue-1
  (package
    (name "rust-priority-queue")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "priority-queue" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w6a4wkxm7h7qhxqgivgxbixw51czmkd83x1vr0gqg4dq054ifh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/garro95/priority-queue")
    (synopsis
     "A Priority Queue implemented as a heap with a function to efficiently change the priority of an item.")
    (description
     "This package provides a Priority Queue implemented as a heap with a function to
efficiently change the priority of an item.")
    (license (list license:lgpl3 license:mpl2.0))))

(define-public rust-protobuf-2.14
  (package
    (name "rust-protobuf")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11bl8hf522s9mbkckivnn9n8s3ss4g41w6jmfdsswmr5adqd71lf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))

(define-public rust-protobuf-codegen-2.14
  (package
    (name "rust-protobuf-codegen")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "031bx325lsgcx7wc76vc2cqph6q0b34jgc8nz0g2rkwcfnx3n4fy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2.14))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.
")
    (description "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.")
    (license license:expat)))

(define-public rust-protobuf-codegen-pure-2.14
  (package
    (name "rust-protobuf-codegen-pure")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen-pure" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h34gfqlb7bqmgqv1mfgy5wk35z5r2h5ki3p3pdcmw1vqzmly6id"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2.14)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-2.14))))
    (home-page
     "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-codegen-pure/")
    (synopsis "Pure-rust codegen for protobuf using protobuf-parser crate

WIP
")
    (description "Pure-rust codegen for protobuf using protobuf-parser crate

WIP")
    (license license:expat)))

(define-public rust-quale-1
  (package
    (name "rust-quale")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quale" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "135wx0fhg5qv4887c4fyj0bhqixknf97ihmd4gmwqr6c2g2i2s64"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/frewsxcv/rust-quale")
    (synopsis
     "A Rust port of the `which` utility. Locates an executable in the userâs path.")
    (description
     "This package provides a Rust port of the `which` utility.  Locates an executable
in the userâs path.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rental-0.5
  (package
    (name "rust-rental")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rental" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bhzz2pfbg0yaw8p1l31bggq4jn077wslf6ifhj22vf3r8mgx2fc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rental-impl" ,rust-rental-impl-0.5)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/jpernst/rental")
    (synopsis
     "A macro to generate safe self-referential structs, plus premade types for common use cases.")
    (description
     "This package provides a macro to generate safe self-referential structs, plus
premade types for common use cases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rental-impl-0.5
  (package
    (name "rust-rental-impl")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rental-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pj0qgmvwwsfwyjqyjxzikkwbwc3vj7hm3hdykr47dy5inbnhpj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://www.jpernst.com")
    (synopsis
     "An implementation detail of rental. Should not be used directly.")
    (description
     "An implementation detail of rental.  Should not be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ringbuf-0.2
  (package
    (name "rust-ringbuf")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ringbuf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18n2qmbvvxj9s775p6q2dv5s68ndbpvb7fr3mx5fg2gpa26z2npn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cache-padded" ,rust-cache-padded-1))))
    (home-page "https://github.com/agerasev/ringbuf")
    (synopsis
     "Lock-free SPSC FIFO ring buffer with direct access to inner data")
    (description
     "Lock-free SPSC FIFO ring buffer with direct access to inner data")
    (license (list license:expat license:asl2.0))))

(define-public rust-rodio-0.13
  (package
    (name "rust-rodio")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rodio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03sjp37vb7ss1dwf76sf6yavmndqklx35shjpg8zd49ickd2wp5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-claxon" ,rust-claxon-0.4)
                       ("rust-cpal" ,rust-cpal-0.13)
                       ("rust-hound" ,rust-hound-3)
                       ("rust-lewton" ,rust-lewton-0.10)
                       ("rust-minimp3" ,rust-minimp3-0.5))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.9))))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Audio playback library")
    (description "Audio playback library")
    (license (list license:expat license:asl2.0))))

(define-public rust-rspotify-0.8
  (package
    (name "rust-rspotify")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rspotify" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14xdic0zhalmvk32y1ffanvgwdqki91qw549kj6mqcdirxka2959"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.10)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-derive-builder" ,rust-derive-builder-0.7)
                       ("rust-dotenv" ,rust-dotenv-0.13)
                       ("rust-env-logger" ,rust-env-logger-0.6)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-itertools" ,rust-itertools-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-1)
                       ("rust-rand" ,rust-rand-0.6)
                       ("rust-random" ,rust-random-0.12)
                       ("rust-reqwest" ,rust-reqwest-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-url" ,rust-url-1)
                       ("rust-webbrowser" ,rust-webbrowser-0.5))))
    (home-page "https://github.com/ramsayleung/rspotify")
    (synopsis "Spotify API wrapper")
    (description "Spotify API wrapper")
    (license license:expat)))

(define-public rust-sdl2-0.34
  (package
    (name "rust-sdl2")
    (version "0.34.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jymfs8ibf1xli4vn562l05bl6zknmff0qz5l7swy2j6m4zvrv6y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-c-vec" ,rust-c-vec-2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
                       ("rust-sdl2-sys" ,rust-sdl2-sys-0.34))))
    (home-page "https://github.com/Rust-SDL2/rust-sdl2")
    (synopsis "SDL2 bindings for Rust")
    (description "SDL2 bindings for Rust")
    (license license:expat)))

(define-public rust-sdl2-sys-0.34
  (package
    (name "rust-sdl2-sys")
    (version "0.34.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x4g141ais0k16frypykc1bfxlg5smraavg2lr0mlnqp3yi9m8j1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.53)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-unidiff" ,rust-unidiff-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2)
                       ("rust-version-compare" ,rust-version-compare-0.0))))
    (home-page "https://github.com/rust-sdl2/rust-sdl2")
    (synopsis "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (description "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (license license:expat)))

(define-public rust-secret-service-2
  (package
    (name "rust-secret-service")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "secret-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18l0yz9sb062jddcx56qi70d4ry2js3irkgysdgii0w77d15rnp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-block-modes" ,rust-block-modes-0.8)
                       ("rust-hkdf" ,rust-hkdf-0.11)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-zbus" ,rust-zbus-1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page "https://github.com/hwchen/secret-service-rs.git")
    (synopsis "Library to interface with Secret Service API")
    (description "Library to interface with Secret Service API")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-0.4
  (package
    (name "rust-security-framework")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dg504y0ys27qksdcnn8qcyvrfhkjx8f5d6jvpm7i7yrsw18k034"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-0.4))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description "Security.framework bindings for macOS and iOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-2.6
  (package
    (name "rust-security-framework")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p0hgqba3h2glm7mgp5d45l2gpmh28kn5vddlfa032mg5wblzh9d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9.3)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2.6))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-tempdir" ,rust-tempdir-0.3)
                                   ("rust-x509-parser" ,rust-x509-parser-0.12))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description "Security.framework bindings for macOS and iOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-sys-0.4
  (package
    (name "rust-security-framework-sys")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01c4fkj78frz6kz9vri1am2pbkz52raxws5lbrli5xajjbci3gqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description "Apple `Security.framework` low-level FFI bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-sys-2.6
  (package
    (name "rust-security-framework-sys")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mn5lm0jip9nm6ydqm6qd9alyiwq15c027777jsbyibs2wxa2q01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8.3)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description "Apple `Security.framework` low-level FFI bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-shannon-0.2
  (package
    (name "rust-shannon")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "shannon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qa52zs4y1i87ysr11g9p6shpdagl14bb340gfm6rd97jhfb99by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1))))
    (home-page "")
    (synopsis "Shannon cipher implementation")
    (description "Shannon cipher implementation")
    (license license:expat)))

(define-public rust-slice-deque-0.3
  (package
    (name "rust-slice-deque")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slice-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "098gvqjw52qw4gac567c9hx3y6hw9al7hjqb5mnvmvydh3i6xvri"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-mach" ,rust-mach-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/gnzlbg/slice_deque")
    (synopsis "A double-ended queue that Deref's into a slice.")
    (description
     "This package provides a double-ended queue that Deref's into a slice.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-0.1
  (package
    (name "rust-stdweb")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stdweb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gjk7ch31a3kgdc39kj4zqinf10yqaf717wanh9kwwbbwg430m7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clippy" ,rust-clippy-0.0)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "A standard library for the client-side Web")
    (description
     "This package provides a standard library for the client-side Web")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-0.2.25
  (package
    (name "rust-tokio")
    (version "0.2.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14l0rll6y1dyzh6qcd8rma2ch3wx0dxzxq8b54di744sjirs40v7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-0.5)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-iovec" ,rust-iovec-0.1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mio" ,rust-mio-0.6.23)
                       ("rust-mio-named-pipes" ,rust-mio-named-pipes-0.1)
                       ("rust-mio-uds" ,rust-mio-uds-0.6)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-0.2)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-tokio-compat-0.1
  (package
    (name "rust-tokio-compat")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-compat" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08n7lkf5l2drb7hph1r3s6jj9cbarbbcr69dvnbr4yxa6m8n4yqh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
                       ("rust-tokio" ,rust-tokio-0.2.25)
                       ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
                       ("rust-tokio-executor" ,rust-tokio-executor-0.1.10)
                       ("rust-tokio-reactor" ,rust-tokio-reactor-0.1.12)
                       ("rust-tokio-timer" ,rust-tokio-timer-0.2.13))
       #:cargo-development-inputs (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Compatibility between `tokio` 0.2 and legacy versions.
")
    (description "Compatibility between `tokio` 0.2 and legacy versions.")
    (license license:expat)))

(define-public rust-tokio-compat-02-0.2
  (package
    (name "rust-tokio-compat-02")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-compat-02" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01vyvimvyip9m779a3v5nvf3bm7sc9s949vs1bzqzgmp49w27m77"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio" ,rust-tokio-0.2.25)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))
       #:cargo-development-inputs (("rust-hyper" ,rust-hyper-0.13)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio 0.2 compat utilities")
    (description "Tokio 0.2 compat utilities")
    (license license:expat)))

(define-public rust-tokio-executor-0.1.10
  (package
    (name "rust-tokio-executor")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-executor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w8n78d2vixs1vghqc4wy9w0d1h6qkli51c1yzhzbns88n7inbgv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
                       ("rust-futures" ,rust-futures-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Future execution primitives
")
    (description "Future execution primitives")
    (license license:expat)))

(define-public rust-tokio-reactor-0.1.12
  (package
    (name "rust-tokio-reactor")
    (version "0.1.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-reactor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l8klnd41q55f3ialzz0lb7s5bfwa38nh86sa9vai2xsqh75kg09"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mio" ,rust-mio-0.6.23)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.9)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-executor" ,rust-tokio-executor-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-sync" ,rust-tokio-sync-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Event loop that drives Tokio I/O resources.
")
    (description "Event loop that drives Tokio I/O resources.")
    (license license:expat)))

(define-public rust-tokio-stream-0.1.9
  (package
    (name "rust-tokio-stream")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-stream" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n804q6lpcsrgs1acj7101bm1cv6wpjf23x4zvjc9zfn2x0xam6z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-tokio-timer-0.2.13
  (package
    (name "rust-tokio-timer")
    (version "0.2.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-timer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15pjjj6daks3sii8p24a509b0dapl2kyk740nwfgz59w64nly14k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-executor" ,rust-tokio-executor-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Timer facilities for Tokio
")
    (description "Timer facilities for Tokio")
    (license license:expat)))

(define-public rust-tokio-util-0.7
  (package
    (name "rust-tokio-util")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0igzhn80k8l9w6r5qj0bci70kxhbsm1j31gr406pghyxvvc3qinc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3.21)
                       ("rust-futures-io" ,rust-futures-io-0.3.21)
                       ("rust-futures-sink" ,rust-futures-sink-0.3.21)
                       ("rust-futures-util" ,rust-futures-util-0.3.21)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-test" ,rust-futures-test-0.3.21)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.
")
    (description "Additional utilities for working with Tokio.")
    (license license:expat)))

(define-public rust-tracing-error-0.1
  (package
    (name "rust-tracing-error")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-error" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "092y3357af6058mdw7nmr7sysqdka8b4cyaqz940fl2a7nwc1mxl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for enriching errors with `tracing`.
")
    (description "Utilities for enriching errors with `tracing`.")
    (license license:expat)))

(define-public rust-unic-emoji-char-0.9
  (package
    (name "rust-unic-emoji-char")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unic-emoji-char" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ka9fr7s6lv0z43r9xphg9injn35pfxf9g9q18ki0wl9d0g241qb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unic-char-property" ,rust-unic-char-property-0.9)
                       ("rust-unic-char-range" ,rust-unic-char-range-0.9)
                       ("rust-unic-ucd-version" ,rust-unic-ucd-version-0.9))))
    (home-page "https://github.com/open-i18n/rust-unic/")
    (synopsis "UNIC â Unicode Emoji â Emoji Character Properties")
    (description "UNIC â Unicode Emoji â Emoji Character Properties")
    (license (list license:expat license:asl2.0))))

(define-public rust-unidiff-0.3
  (package
    (name "rust-unidiff")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unidiff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b13vhp2x7jlvmkm44h5niqcxklyrmz6afmppvykp4zimhcjg9nq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/messense/unidiff-rs")
    (synopsis "Unified diff parsing/metadata extraction library for Rust")
    (description "Unified diff parsing/metadata extraction library for Rust")
    (license license:expat)))

(define-public rust-vorbis-0.0.14
  (package
    (name "rust-vorbis")
    (version "0.0.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbis" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xn7diq8qz2zmsmwzg3rcsxmpmm2gj7wgnl2gdan0lq7ax21k2jy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-vorbis-sys" ,rust-vorbis-sys-0.1)
                       ("rust-vorbisfile-sys" ,rust-vorbisfile-sys-0.0.8))))
    (home-page "https://github.com/tomaka/vorbis-rs")
    (synopsis "High-level bindings for the official libvorbis library.")
    (description "High-level bindings for the official libvorbis library.")
    (license license:asl2.0)))

(define-public rust-vorbis-sys-0.1
  (package
    (name "rust-vorbis-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbis-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zgv7lwa4b2z091g25h83zil8bawk4frc1f0ril5xa31agpxd7mx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24))))
    (home-page "")
    (synopsis "FFI for the libvorbis library")
    (description "FFI for the libvorbis library")
    (license license:expat)))

(define-public rust-vorbisfile-sys-0.0.8
  (package
    (name "rust-vorbisfile-sys")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbisfile-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1la2j2zbzdjd93byz21ij58c540bfn1r9pi0bssrjimcw7bhchsg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3.24)
                       ("rust-vorbis-sys" ,rust-vorbis-sys-0.1))))
    (home-page "")
    (synopsis "FFI for the vorbisfile library")
    (description "FFI for the vorbisfile library")
    (license license:expat)))

(define-public rust-whoami-0.9
  (package
    (name "rust-whoami")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "whoami" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "012mw2q72gpmf354yw2qc5w105ziac75shpqp1f62x4hnqx7g13q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cala-core" ,rust-cala-core-0.1))))
    (home-page "https://github.com/libcala/whoami/blob/main/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description "Retrieve the current user and environment.")
    (license (list license:expat license:boost1.0))))

(define-public rust-zbus-1
  (package
    (name "rust-zbus")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jgwydwjgk16dyrzdbc1k0dnqj9kv9p3fwcv92a7l9np3hlv5glw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-derivative" ,rust-derivative-2)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-nb-connect" ,rust-nb-connect-1)
                       ("rust-nix" ,rust-nix-0.22.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-polling" ,rust-polling-2)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.4)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-ntest" ,rust-ntest-0.7)
                                   ("rust-zbus-polkit" ,rust-zbus-polkit-1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "API for D-Bus communication")
    (description "API for D-Bus communication")
    (license license:expat)))

(define-public rust-zbus-macros-1
  (package
    (name "rust-zbus-macros")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19p0pdwdf52zkaknav0pj5qvgcf52xk8a4p3a4ymxybwhjkmjfgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-zbus" ,rust-zbus-1)
                                   ("rust-zvariant" ,rust-zvariant-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "proc-macros for zbus")
    (description "proc-macros for zbus")
    (license license:expat)))

(define-public rust-zbus-polkit-1
  (package
    (name "rust-zbus-polkit")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus-polkit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yzbs8sga4s3h97vb6n5nvdvlnmhws2vj46bn44hbncfm25f51mc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-zbus" ,rust-zbus-1)
                       ("rust-zvariant" ,rust-zvariant-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "PolicyKit binding")
    (description "PolicyKit binding")
    (license license:expat)))

(define-public rust-zeroize-0.4
  (package
    (name "rust-zeroize")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zeroize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fbwsinb9yp6n7gh7h9b24mycwk7vkqcp6rmssi8pwnykpngzpfp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-semver" ,rust-semver-0.9))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
     "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
'optimized away' by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-zvariant-2
  (package
    (name "rust-zvariant")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0995d59vl8409mk3qrbshqrz5d76dq52szg0x2vqji07y9app356"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-repr" ,rust-serde-repr-0.1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))

(define-public rust-zvariant-derive-2
  (package
    (name "rust-zvariant-derive")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s9xk9c4p9vl0j2vr1abqc12mgv500sjc3fnh8ij3d1yb4i5xjp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs (("rust-byteorder" ,rust-byteorder-1)
                                   ("rust-enumflags2" ,rust-enumflags2-0.6)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-repr" ,rust-serde-repr-0.1)
                                   ("rust-zvariant" ,rust-zvariant-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))
