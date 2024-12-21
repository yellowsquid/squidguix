(define-module (yellowsquid packages kanidm)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public kanidm
  (package
    (name "kanidm")
    (version "1.4.5")
    (home-page "https://kanidm.github.io/kanidm/stable/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kanidm/kanidm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nm5j6wq8dq8f80996cwklvw8a8kvqb3db4n0nbpabg451kzyyfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:rust ,rust-1.79
       #:tests? #f ;needs rustdoc, which requires rebuilding rust
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-argon2" ,rust-argon2-0.5)
                       ("rust-askama" ,rust-askama-0.12)
                       ("rust-askama_axum" ,rust-askama-axum-0.4)
                       ("rust-assert_cmd" ,rust-assert-cmd-2)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum" ,rust-axum-0.7)
                       ("rust-axum-auth" ,rust-axum-auth-0.7)
                       ("rust-axum-extra" ,rust-axum-extra-0.9)
                       ("rust-axum-htmx" ,rust-axum-htmx-0.5)
                       ("rust-axum-server" ,rust-axum-server-0.7)
                       ("rust-base32" ,rust-base32-0.5)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap_complete" ,rust-clap-complete-4)
                       ("rust-compact_jwt" ,rust-compact-jwt-0.4)
                       ("rust-concread" ,rust-concread-0.5)
                       ("rust-cron" ,rust-cron-0.12)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-dhat" ,rust-dhat-0.3)
                       ("rust-dialoguer" ,rust-dialoguer-0.10)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-enum-iterator" ,rust-enum-iterator-2)
                       ("rust-escargot" ,rust-escargot-0.5)
                       ("rust-fantoccini" ,rust-fantoccini-0.21)
                       ("rust-fernet" ,rust-fernet-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fs4" ,rust-fs4-0.8)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gethostname" ,rust-gethostname-0.5)
                       ("rust-gix" ,rust-gix-0.64)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-idlset" ,rust-idlset-0.2)
                       ("rust-image" ,rust-image-0.24)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-jsonschema" ,rust-jsonschema-0.21)
                       ("rust-lazy_static" ,rust-lazy-static-1)
                       ("rust-ldap3_client" ,rust-ldap3-client-0.5)
                       ("rust-ldap3_proto" ,rust-ldap3-proto-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libnss" ,rust-libnss-0.8)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.25)
                       ("rust-lodepng" ,rust-lodepng-3)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-mathru" ,rust-mathru-0.13)
                       ("rust-mimalloc" ,rust-mimalloc-0.1)
                       ("rust-nonempty" ,rust-nonempty-0.8)
                       ("rust-notify-debouncer-full" ,rust-notify-debouncer-full-0.1)
                       ("rust-num_enum" ,rust-num-enum-0.5)
                       ("rust-oauth2" ,rust-oauth2-4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.20)
                       ("rust-opentelemetry-otlp" ,rust-opentelemetry-otlp-0.13)
                       ("rust-opentelemetry_api" ,rust-opentelemetry-api-0.20)
                       ("rust-opentelemetry_sdk" ,rust-opentelemetry-sdk-0.20)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-peg" ,rust-peg-0.8)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-prctl" ,rust-prctl-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-qrcode" ,rust-qrcode-0.12)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand_chacha" ,rust-rand-chacha-0.3)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-rusqlite" ,rust-rusqlite-0.28)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-sd-notify" ,rust-sd-notify-0.4)
                       ("rust-selinux" ,rust-selinux-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde_cbor_2" ,rust-serde-cbor-2-0.12)
                       ("rust-serde_json" ,rust-serde-json-1)
                       ("rust-serde_urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-serde_with" ,rust-serde-with-3)
                       ("rust-sha-crypt" ,rust-sha-crypt-0.5)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-shellexpand" ,rust-shellexpand-2)
                       ("rust-smartstring" ,rust-smartstring-1)
                       ("rust-smolset" ,rust-smolset-1)
                       ("rust-sshkey-attest" ,rust-sshkey-attest-0.5)
                       ("rust-sshkeys" ,rust-sshkeys-0.3)
                       ("rust-svg" ,rust-svg-0.13)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-forest" ,rust-tracing-forest-0.1)
                       ("rust-tracing-opentelemetry" ,rust-tracing-opentelemetry-0.21)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-utoipa" ,rust-utoipa-4)
                       ("rust-utoipa-swagger-ui" ,rust-utoipa-swagger-ui-6)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-webauthn-authenticator-rs" ,rust-webauthn-authenticator-rs-0.5)
                       ("rust-webauthn-rs" ,rust-webauthn-rs-0.5)
                       ("rust-webauthn-rs-core" ,rust-webauthn-rs-core-0.5)
                       ("rust-webauthn-rs-proto" ,rust-webauthn-rs-proto-0.5)
                       ("rust-whoami" ,rust-whoami-1)
                       ("rust-x509-cert" ,rust-x509-cert-0.2)
                       ("rust-zxcvbn" ,rust-zxcvbn-2))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'set-build-profile
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-output-to-file "libs/profiles/guix.toml"
                          (lambda ()
                            (format #t
                                    "htmx_ui_pkg_path = \"~a/share/kanidm/ui\"
admin_bind_path = \"/var/run/kanidmd/sock\"
default_config_path = \"/etc/kanidm/server.toml\"
default_unix_shell_path = ~s
"
                                    out out
                                    (search-input-file inputs "bin/sh"))))
                        (setenv "KANIDM_BUILD_PROFILE" "guix"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (unix (assoc-ref outputs "unix"))
                            (client (assoc-ref outputs "client")))
                        (mkdir-p out)
                        (mkdir-p unix)
                        (mkdir-p client)
                        (setenv "CARGO_TARGET_DIR" "./target")

                        ;; Daemon
                        (invoke "install" "-D"
                                "target/release/kanidmd"
                                (string-append out "/bin/kanidmd"))
                        ;; TODO: build web_ui
                        (mkdir-p (string-append out "/share/kanidm"))
                        (copy-recursively "server/core/static"
                                          (string-append out
                                                         "/share/kanidm/ui"))

                        ;; Unix integration
                        (invoke "install" "-D"
                                "target/release/kanidm_unixd"
                                (string-append unix "/bin/kanidm_unixd"))
                        (invoke "install" "-D"
                                "target/release/kanidm_unixd_tasks"
                                (string-append unix "/bin/kanidm_unixd_tasks"))
                        (invoke "install" "-D"
                                "target/release/kanidm_ssh_authorizedkeys"
                                (string-append unix "/bin/kanidm_ssh_authorizedkeys"))
                        (invoke "install" "-D"
                                "target/release/kanidm_unixd"
                                (string-append unix "/bin/kanidm_unixd"))
                        (invoke "install" "-D"
                                "target/release/libnss_kanidm.so"
                                (string-append unix "/lib/libnss_kanidm.so.2"))
                        (invoke "install" "-D"
                                "target/release/libpam_kanidm.so"
                                (string-append unix
                                               "/lib/security/pam_kanidm.so"))

                        ;; Client tools
                        (invoke "install" "-D"
                                "target/release/kanidm"
                                (string-append client
                                               "/bin/kanidm"))))))))
    (native-inputs (list pkg-config))
    (inputs (list eudev linux-pam openssl sqlite))
    (outputs (list "out" "unix" "client"))
    (synopsis "Simple, secure and fast identity management platform")
    (description
     "Kanidm is a simple and secture identity management platform, allowing other
applications and services to offload the challenge of authenticating and storing
identities to Kanidm.")
    (license license:mpl2.0)))

(define-public rust-accelerate-src-0.3
  (package
    (name "rust-accelerate-src")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "accelerate-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17fiqyq7f9k41pbsyrvk9pxyx9z6fw399wq036cvwkbmb14xcpj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/blas-lapack-rs/accelerate-src")
    (synopsis
     "The package provides a source of BLAS and LAPACK via the Accelerate framework")
    (description
     "This package provides The package provides a source of BLAS and LAPACK via
the Accelerate framework.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-addr-0.11
  (package
    (name "rust-addr")
    (version "0.11.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "addr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7fhhvhfdvvzvsh52hz56l9s9p1z3nh0q830llynf7rrblrfrlk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-no-std-net" ,rust-no-std-net-0.5)
                       ("rust-psl" ,rust-psl-2)
                       ("rust-psl-types" ,rust-psl-types-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/addr-rs/addr")
    (synopsis "Library for parsing domain names")
    (description "This package provides a library for parsing domain names.")
    (license (list license:expat license:asl2.0))))

(define-public rust-addr2line-0.22
  (package
    (name "rust-addr2line")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "addr2line" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y66f1sa27i9kvmlh76ynk60rxfrmkba9ja8x527h32wdb206ibf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-gimli" ,rust-gimli-0.29)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-object" ,rust-object-0.35)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
     "Cross-platform symbolication library written in Rust, using `gimli`")
    (description
     "This package provides a cross-platform symbolication library written in Rust,
using `gimli`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-aes-kw-0.2
  (package
    (name "rust-aes-kw")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aes-kw" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "131xvnah1magbr8q0lwmg3c13lv54vh41f2z79zmzyyf5lsjpyk9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8))))
    (home-page "https://github.com/RustCrypto/key-wraps/")
    (synopsis
     "NIST 800-38F AES Key Wrap (KW) and Key Wrap with Padding (KWP) modes")
    (description
     "This package provides NIST 800-38F AES Key Wrap (KW) and Key Wrap with
Padding (KWP) modes.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic-polyfill" ,rust-atomic-polyfill-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "Non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance.")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-log-sys-0.3
  (package
    (name "rust-android-log-sys")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "android_log-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dwrvwkx2xxqys6nrhfavbbqfx2rs61nq8akrlnqkfbapxb81k2y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-mobile/android_log-sys-rs")
    (synopsis "FFI bindings to Android log Library")
    (description "This package provides FFI bindings to Android log Library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-logger-0.13
  (package
    (name "rust-android-logger")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "android_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bvp6lf39q0zykn70lys562kdb14r9dfm91m79jxq53cfi7i7564"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-log-sys" ,rust-android-log-sys-0.3)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/rust-mobile/android_logger-rs")
    (synopsis
     "Logging implementation for `log` which hooks to android log output")
    (description
     "This package provides a logging implementation for `log` which hooks to android
log output.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cfmkza63xpn1kkz844mgjwm9miaiz4jkyczmwxzivcsypk1vv0v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "This package provides ANSI text styling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-any-ascii-0.1
  (package
    (name "rust-any-ascii")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "any_ascii" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07kyb9is518jr1sbc6804kgg5pnx8djl328q3al28lcbxdvkf0vh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://anyascii.com")
    (synopsis "Unicode to ASCII transliteration")
    (description "This package provides Unicode to ASCII transliteration.")
    (license license:isc)))

(define-public rust-anyhow-1
  (package
    (name "rust-anyhow")
    (version "1.0.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anyhow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04xgvbdlgcfd80ahvgsk1dq9q4969ilj9n4xp62052nlsxk73wvl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn" ,rust-syn-2)
                                   ("rust-thiserror" ,rust-thiserror-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/anyhow")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
     "This package provides Flexible concrete Error type built on std::error::Error.")
    (license (list license:expat license:asl2.0))))

(define-public rust-argon2-0.5
  (package
    (name "rust-argon2")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "argon2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-blake2" ,rust-blake2-0.10)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-password-hash" ,rust-password-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.4)
                                   ("rust-password-hash" ,rust-password-hash-0.5))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/argon2")
    (synopsis
     "Pure Rust implementation of the Argon2 password hashing function")
    (description
     "This package provides Pure Rust implementation of the Argon2 password
hashing function with support for the Argon2d, Argon2i, and Argon2id algorithmic
variants.")
    (license (list license:expat license:asl2.0))))

(define-public rust-askama-axum-0.4
  (package
    (name "rust-askama-axum")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "askama_axum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qzifp9qgxhchgnsf53gbgngdbdn7qjif3vnc2pw8nmzrpvh65m4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-askama" ,rust-askama-0.12)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-http" ,rust-http-1))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-http-body-util" ,rust-http-body-util-0.1)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4))))
    (home-page "https://github.com/djc/askama")
    (synopsis "Axum integration for Askama templates")
    (description
     "This package provides Axum integration for Askama templates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-assert-cmd-2
  (package
    (name "rust-assert-cmd")
    (version "2.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "assert_cmd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-doc-comment" ,rust-doc-comment-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-predicates" ,rust-predicates-3)
                       ("rust-predicates-core" ,rust-predicates-core-1)
                       ("rust-predicates-tree" ,rust-predicates-tree-1)
                       ("rust-wait-timeout" ,rust-wait-timeout-0.2))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-escargot" ,rust-escargot-0.5))))
    (home-page "https://github.com/assert-rs/assert_cmd")
    (synopsis "Test CLI Applications")
    (description "This package provides Test CLI Applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-dnssd-0.5
  (package
    (name "rust-async-dnssd")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-dnssd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10hb894c05l0ijd5bbn9759vwwdafpxwy7w4i076d3fcjrcw1vwq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/stbuehler/rust-async-dnssd")
    (synopsis "Asynchronous wrapper for DNS-SD C libraries")
    (description
     "This package provides Asynchronous wrapper for DNS-SD C libraries.")
    (license license:expat)))

(define-public rust-async-fs-2
  (package
    (name "rust-async-fs")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-lock" ,rust-async-lock-3)
                       ("rust-blocking" ,rust-blocking-1)
                       ("rust-futures-lite" ,rust-futures-lite-2))))
    (home-page "https://github.com/smol-rs/async-fs")
    (synopsis "Async filesystem primitives")
    (description "This package provides Async filesystem primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-async-process-2
  (package
    (name "rust-async-process")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-process" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x3305pq0fzaqmw7q4c93sgabq97zhkr32xig5dkhkcscn4pg858"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-channel" ,rust-async-channel-2)
                       ("rust-async-io" ,rust-async-io-2)
                       ("rust-async-lock" ,rust-async-lock-3)
                       ("rust-async-signal" ,rust-async-signal-0.2)
                       ("rust-async-task" ,rust-async-task-4)
                       ("rust-blocking" ,rust-blocking-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-event-listener" ,rust-event-listener-5)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/smol-rs/async-process")
    (synopsis "Async interface for working with processes")
    (description
     "This package provides Async interface for working with processes.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-async-recursion-1
  (package
    (name "rust-async-recursion")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-recursion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-futures-executor" ,rust-futures-executor-0.3)
                                   ("rust-macrotest" ,rust-macrotest-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dcchut/async-recursion")
    (synopsis "Recursion for async functions")
    (description "This package provides Recursion for async functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-task-4
  (package
    (name "rust-async-task")
    (version "4.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-task" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-portable-atomic" ,rust-portable-atomic-1))))
    (home-page "https://github.com/smol-rs/async-task")
    (synopsis "Task abstraction for building executors")
    (description
     "This package provides Task abstraction for building executors.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-async-trait-0.1
  (package
    (name "rust-async-trait")
    (version "0.1.83")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-trait" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p8q8gm4fv2fdka8hwy2w3f8df7p5inixqi7rlmbnky3wmysw73j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/async-trait")
    (synopsis "Type erasure for async trait methods")
    (description "This package provides Type erasure for async trait methods.")
    (license (list license:expat license:asl2.0))))

(define-public rust-authenticator-ctap2-2021-0.3
  (package
    (name "rust-authenticator-ctap2-2021")
    (version "0.3.2-dev.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "authenticator-ctap2-2021" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mnhhm4fy4yxpv5b9sfs37knc3b8wygklxzq1h6gf018bq76jv6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bindgen" ,rust-bindgen-0.58)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-0.5)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-devd-rs" ,rust-devd-rs-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libudev" ,rust-libudev-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memoffset" ,rust-memoffset-0.6)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-runloop" ,rust-runloop-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-cbor" ,rust-serde-cbor-0.11)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-warp" ,rust-warp-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/mozilla/authenticator-rs/")
    (synopsis
     "Library for interacting with CTAP1/2 security keys for Web Authentication")
    (description
     "This package provides Library for interacting with CTAP1/2 security keys
for Web Authentication.  Used by Firefox.")
    (license license:mpl2.0)))

(define-public rust-automod-1
  (package
    (name "rust-automod")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "automod" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12rsa5barxi8v916hlvvpjyh43y5x2yjc2bg1xs6v960vccyxwzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/automod")
    (synopsis "Pull in every source file in a directory as a module")
    (description
     "This package provides Pull in every source file in a directory as a module.")
    (license (list license:expat license:asl2.0))))

(define-public rust-av-data-0.4
  (package
    (name "rust-av-data")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "av-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lk5pq6jfmfp5ihvnzqdqxympk5rk7648bcsvwhgj02xaairhnyp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byte-slice-cast" ,rust-byte-slice-cast-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-num-derive" ,rust-num-derive-0.4)
                       ("rust-num-rational" ,rust-num-rational-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/rust-av/rust-av")
    (synopsis "Multimedia data structures")
    (description "This package provides Multimedia data structures.")
    (license license:expat)))

(define-public rust-aws-lc-rs-1
  (package
    (name "rust-aws-lc-rs")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aws-lc-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xxdyn1zcc4jiy0n3bfn56486djbsfd76w0pzn50wlx7s2dlvrsa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aws-lc-fips-sys" ,rust-aws-lc-fips-sys-0.12)
                       ("rust-aws-lc-sys" ,rust-aws-lc-sys-0.20)
                       ("rust-mirai-annotations" ,rust-mirai-annotations-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-untrusted" ,rust-untrusted-0.7)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/aws/aws-lc-rs")
    (synopsis
     "Cryptographic library using AWS-LC for its cryptographic operations")
    (description
     "A cryptographic library using AWS-LC for its cryptographic operations.
This library strives to be API-compatible with the popular Rust library named
ring.")
    (license (list license:isc license:asl2.0))))

(define-public rust-aws-lc-sys-0.20
  (package
    (name "rust-aws-lc-sys")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aws-lc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00q73wwzbcp9v1sn155d5gajj5ki9mqlpj9d1hj2vbf6529283hg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/aws/aws-lc-rs")
    (synopsis
     "General-purpose cryptographic library for AWS")
    (description
     "A general-purpose cryptographic library maintained by the AWS Cryptography
team for AWS and their customers.  It's based on code from the Google
@code{BoringSSL} project and the @code{OpenSSL} project.")
    (license (list license:isc license:asl2.0 license:openssl))))

(define-public rust-axum-0.7
  (package
    (name "rust-axum")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bkhgnj7rk1aih1c1ylqkmn72mjbgi8lql1paim35j3s613kjkjh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-axum-macros" ,rust-axum-macros-0.4)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-matchit" ,rust-matchit-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.24)
                       ("rust-tower" ,rust-tower-0.5)
                       ("rust-tower-http" ,rust-tower-http-0.6)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-axum-macros" ,rust-axum-macros-0.4)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-time" ,rust-time-0.3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.24)
                                   ("rust-tower" ,rust-tower-0.5)
                                   ("rust-tower-http" ,rust-tower-http-0.6)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                                   ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description
     "This package provides Web framework that focuses on ergonomics and modularity.")
    (license license:expat)))

(define-public rust-axum-auth-0.7
  (package
    (name "rust-axum-auth")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-auth" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02hjwhji03lrjsabkg22hnzk0paqvy0kbz7w2j3gcm2z30x12sc1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-http" ,rust-http-1))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/owez/axum-auth")
    (synopsis "High-level http auth extractors for axum")
    (description
     "This package provides High-level http auth extractors for axum.")
    (license (list license:expat license:asl2.0))))

(define-public rust-axum-core-0.4
  (package
    (name "rust-axum-core")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16b1496c4gm387q20hkv5ic3k5bd6xmnvk50kwsy6ymr8rhvvwh9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-1)
                       ("rust-tower-http" ,rust-tower-http-0.6)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description "This package provides Core types and traits for axum.")
    (license license:expat)))

(define-public rust-axum-extra-0.9
  (package
    (name "rust-axum-extra")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ydi660sqd9bl6252axx12xkdn9v3jqgkidny6f71sla305j5hvk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-axum" ,rust-axum-0.7)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-axum-macros" ,rust-axum-macros-0.4)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.18)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-prost" ,rust-prost-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-html-form" ,rust-serde-html-form-0.2)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.5)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.5)
                                   ("rust-tower-http" ,rust-tower-http-0.6))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Extra utilities for axum")
    (description "This package provides Extra utilities for axum.")
    (license license:expat)))

(define-public rust-axum-htmx-0.5
  (package
    (name "rust-axum-htmx")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-htmx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07lh36gz23sz2lcr8aj7wkg5930r9g59pavcl1g6wjq9vhghbxs0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.4)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tower" ,rust-tower-0.4))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7))))
    (home-page "https://github.com/robertwayne/axum-htmx")
    (synopsis
     "Set of htmx extractors, responders, and request guards for axum")
    (description
     "This package provides a set of htmx extractors, responders, and request guards
for axum.")
    (license (list license:expat license:asl2.0))))

(define-public rust-axum-macros-0.4
  (package
    (name "rust-axum-macros")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1klv77c889jm05bzayaaiinalarhvh2crc2w4nvp3l581xaj7lap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Macros for axum")
    (description "This package provides Macros for axum.")
    (license license:expat)))

(define-public rust-axum-server-0.7
  (package
    (name "rust-axum-server")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n67cx39cm9zsm0dwm0nla67qjswj90ccqrwq0x3kagn904ckfjn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs (("rust-axum" ,rust-axum-0.7)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4)
                                   ("rust-tower-http" ,rust-tower-http-0.5))))
    (home-page "https://github.com/programatik29/axum-server")
    (synopsis "High level server designed to be used with axum framework")
    (description
     "This package provides High level server designed to be used with axum framework.")
    (license license:expat)))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.73")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "backtrace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02iffg2pkg5nc36pgml8il7f77s138hhjw9f9l56v5zqlilk5hjw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addr2line" ,rust-addr2line-0.22)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-miniz-oxide" ,rust-miniz-oxide-0.7)
                       ("rust-object" ,rust-object-0.36)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
     "Library to acquire a stack trace (backtrace) at runtime in a Rust program")
    (description
     "This package provides a library to acquire a stack trace (backtrace) at runtime
in a Rust program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bardecoder-0.4
  (package
    (name "rust-bardecoder")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bardecoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "071gz5gr7wyl02l1xjcqh4hxmfmw3y1n6m7c8j0bvcqkkv15ivgq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-image" ,rust-image-0.23)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-newtype-derive" ,rust-newtype-derive-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/piderman314/bardecoder")
    (synopsis "Detect and decode QR Codes")
    (description "This package provides Detect and decode QR Codes.")
    (license license:expat)))

(define-public rust-base32-0.5
  (package
    (name "rust-base32")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base32" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xp0a3xml25xw2bp5pyac2nld7vmmfjl02qynnyfn6aznfggwb82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/andreasots/base32")
    (synopsis "Base32 encoder/decoder for Rust")
    (description "This package provides Base32 encoder/decoder for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-base64urlsafedata-0.1
  (package
    (name "rust-base64urlsafedata")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64urlsafedata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110kfb0jsqb6v54p4bhmvp2crza502g3nik7aafhl4klpc5d7cqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Base 64 Url Safe wrapper for Serde")
    (description "This package provides Base 64 Url Safe wrapper for Serde.")
    (license license:mpl2.0)))

(define-public rust-base64urlsafedata-0.5
  (package
    (name "rust-base64urlsafedata")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64urlsafedata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1izhcay5n5h6zj6b0nps9zdb7q3wxfnm8x4d0skyzlawvx78jmhs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-cbor-2" ,rust-serde-cbor-2-0.12)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Base 64 Url Safe wrapper for Serde")
    (description "This package provides Base 64 Url Safe wrapper for Serde.")
    (license license:mpl2.0)))

(define-public rust-bcrypt-pbkdf-0.10
  (package
    (name "rust-bcrypt-pbkdf")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bcrypt-pbkdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18pjhsy3m2v0silsp4mjzz8i92zrpqxk9b059zrnk1w8zvhw5ska"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blowfish" ,rust-blowfish-0.9)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/bcrypt-pbkdf")
    (synopsis "The bcrypt-pbkdf password-based key derivation function")
    (description
     "This package provides bcrypt-pbkdf password-based key derivation function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bindgen-0.60
  (package
    (name "rust-bindgen")
    (version "0.60.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rl8pzzbxsgkx0v20bvvbwrlqhbifzw2p3ikwrns9b543fydsb86"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries")
    (description
     "This package provides Automatically generates Rust FFI bindings to C and
C++ libraries.")
    (license license:bsd-3)))

(define-public rust-bindgen-0.65
  (package
    (name "rust-bindgen")
    (version "0.65.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i9wci1h3xnk8hi7cf06capgifnmpk9dd59zqznh6jcsdx37ppyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-annotate-snippets" ,rust-annotate-snippets-0.9)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries")
    (description
     "This package provides Automatically generates Rust FFI bindings to C and
C++ libraries.")
    (license license:bsd-3)))

(define-public rust-bitfield-0.13
  (package
    (name "rust-bitfield")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitfield" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06g7jb5r2b856vnhx76081fg90jvmy61kjqcfjysgmd5hclvvbs6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dzamlo/rust-bitfield")
    (synopsis "This crate provides macros to generate bitfield-like struct")
    (description
     "This crate provides macros to generate bitfield-like struct.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitflags-2
  (package
    (name "rust-bitflags")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "Macro to generate structures which behave like bitflags")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitstring-0.1
  (package
    (name "rust-bitstring")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitstring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k3fkd8q1lr6smlpk1r20wcwzp1v2xqymndny5zfgkq0nq6rr8r2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/stbuehler/rust-bitstring")
    (synopsis "Bitstring traits and implementations")
    (description "This package provides Bitstring traits and implementations.")
    (license license:expat)))

(define-public rust-blas-src-0.8
  (package
    (name "rust-blas-src")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17chr7lalnjifd71rvxwmq83mw8nvg7cdfj6dkcxdf8cgamgnj5v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-accelerate-src" ,rust-accelerate-src-0.3)
                       ("rust-blis-src" ,rust-blis-src-0.2)
                       ("rust-intel-mkl-src" ,rust-intel-mkl-src-0.5)
                       ("rust-netlib-src" ,rust-netlib-src-0.8)
                       ("rust-openblas-src" ,rust-openblas-src-0.10))))
    (home-page "https://github.com/blas-lapack-rs/blas-src")
    (synopsis "The package provides a BLAS source of choice")
    (description
     "This package provides The package provides a BLAS source of choice.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-blis-src-0.2
  (package
    (name "rust-blis-src")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blis-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ykj5gcs17ifs43awhpjpnlq27q58f8g8ajh08qhd2yfc5krn4fw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/blas-lapack-rs/blis-src")
    (synopsis "Rust native linking for BLIS library")
    (description "This package provides Rust native linking for BLIS library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bluez-async-0.7
  (package
    (name "rust-bluez-async")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bluez-async" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16wxyg4kys1825w8dljrlan9cw50yk9j5hdgnqy8w3ll7i0x9rsw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bluez-generated" ,rust-bluez-generated-0.3)
                       ("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-tokio" ,rust-dbus-tokio-0.7)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/bluez-rs/bluez-async/")
    (synopsis
     "An async wrapper around the D-Bus interface of BlueZ")
    (description
     "This package provides an async wrapper around the D-Bus interface of
@code{BlueZ} (the Linux Bluetooth daemon), supporting GATT client (central)
functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bluez-generated-0.3
  (package
    (name "rust-bluez-generated")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bluez-generated" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rz3mw2wgl7fis2kdkvagydmhqz7f6ir2v30fp5biw42pjfna72d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9))))
    (home-page "https://github.com/bluez-rs/bluez-async/")
    (synopsis "Generated async D-Bus bindings for talking to BlueZ on Linux")
    (description
     "This package provides generated async D-Bus bindings for talking to
@code{BlueZ} on Linux.")
    (license (list license:expat license:asl2.0))))

(define-public rust-boolinator-2
  (package
    (name "rust-boolinator")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "boolinator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nccxzb1dfkjfrgzqaw1a90p26zlvv6nah5ckcpj6bn9a4zqga6g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/DanielKeep/rust-boolinator")
    (synopsis
     "Combine @code{Option} and @code{Result}-style combinators with @code{bool}s")
    (description
     "This package provides the @code{Boolinator} trait, which lets you use
@code{Option} and @code{Result}-style combinators with @code{bool}s.")
    (license (list license:expat license:asl2.0))))

(define-public rust-boringssl-src-0.5
  (package
    (name "rust-boringssl-src")
    (version "0.5.2+6195bf8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "boringssl-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mf6mr59qhgf1zrx9wfs0rmqd6f9ya5x0g81la1flxp2qp66bdbs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cmake" ,rust-cmake-0.1))))
    (home-page "https://github.com/BusyJay/boringssl-src-rs")
    (synopsis "Crate for building boringssl")
    (description "This package provides a crate for building boringssl.")
    (license (list license:expat license:asl2.0))))

(define-public rust-borrow-or-share-0.2
  (package
    (name "rust-borrow-or-share")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "borrow-or-share" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ciski5i69a8mx6f0fh901hn73fii3g39lpl8k3xgi88651b9siy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/yescallop/borrow-or-share")
    (synopsis "Traits for either borrowing or sharing data")
    (description
     "This package provides Traits for either borrowing or sharing data.")
    (license license:expat-0)))

(define-public rust-borsh-1
  (package
    (name "rust-borsh")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "borsh" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vgq96r3k9srkr9xww1pf63vdmslhnk4ciqaqzfjqqpgbpajwdm6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ascii" ,rust-ascii-1)
                       ("rust-borsh-derive" ,rust-borsh-derive-1)
                       ("rust-bson" ,rust-bson-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.2)
                       ("rust-hashbrown" ,rust-hashbrown-0.11))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing")
    (description
     "This package provides Binary Object Representation Serializer for Hashing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-borsh-derive-1
  (package
    (name "rust-borsh-derive")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "borsh-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02ych16fa7fqwhjww3m5mm6ndm5g9kv5p7v1r96wslsgfq2q1vy3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-syn-derive" ,rust-syn-derive-0.1))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing")
    (description
     "This package provides Binary Object Representation Serializer for Hashing.")
    (license license:asl2.0)))

(define-public rust-brotli-6
  (package
    (name "rust-brotli")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0swvf6cgbwhwrpd5y23vq6wipb6q2wqvi2j0hy0xa9lkplfrgxvl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-4)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis
     "Brotli compressor and decompressor")
    (description
     "This package provides a brotli compressor and decompressor that with an
interface avoiding the rust stdlib.  This makes it suitable for embedded devices
and kernels.  It is designed with a pluggable allocator so that the standard
lib's allocator may be employed.  The default build also includes a stdlib
allocator and stream interface.  Disable this with --features=no-stdlib.  All
included code is safe.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-brotli-decompressor-4
  (package
    (name "rust-brotli-decompressor")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli-decompressor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qn39c7n6wm40i2bm0d3q2qslmaavlh804iv0ccbba4m80pbsics"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis
     "Brotli decompressor with an interface avoiding the rust stdlib")
    (description
     "This package provides a brotli decompressor that with an interface avoiding the
rust stdlib.  This makes it suitable for embedded devices and kernels.  It is
designed with a pluggable allocator so that the standard lib's allocator may be
employed.  The default build also includes a stdlib allocator and stream
interface.  Disable this with --features=no-stdlib.  Alternatively,
--features=unsafe turns off array bounds checks and memory initialization but
provides a safe interface for the caller.  Without adding the --features=unsafe
argument, all included code is safe.  For compression in addition to this
library, download https://github.com/dropbox/rust-brotli.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-bssl-sys-0.1
  (package
    (name "rust-bssl-sys")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bssl-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p5v3ad1paf12db4hmwq4j8dvcrppsscf57dwvr880q67hwi4b9i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "")
    (synopsis "Placeholder package for boringssl bindings")
    (description
     "This package provides Placeholder package for boringssl bindings.")
    (license license:expat)))

(define-public rust-btleplug-0.11
  (package
    (name "rust-btleplug")
    (version "0.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "btleplug" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kwry4pm6di4s4mdjgr46nicmnzw3yz8l05jb4lwffa9p6fmyd5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bluez-async" ,rust-bluez-async-0.7)
                       ("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-dbus" ,rust-dbus-0.9)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-jni" ,rust-jni-0.19)
                       ("rust-jni-utils" ,rust-jni-utils-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-windows" ,rust-windows-0.52))))
    (home-page "https://github.com/deviceplug/btleplug")
    (synopsis "Cross-Platform Rust Bluetooth Low Energy (BLE) GATT library")
    (description
     "This package provides a Cross-Platform Rust Bluetooth Low Energy (BLE) GATT
library.")
    (license (list license:expat license:asl2.0 license:bsd-3))))

(define-public rust-bumpalo-3
  (package
    (name "rust-bumpalo")
    (version "3.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bumpalo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis "Fast bump allocation arena for Rust")
    (description
     "This package provides a fast bump allocation arena for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytemuck-1
  (package
    (name "rust-bytemuck")
    (version "1.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytemuck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cdvfvgnww7l904326h4fsydsfgy2lmjrf5lq3ss5bmhqgfcdm3g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck-derive" ,rust-bytemuck-derive-1))))
    (home-page "https://github.com/Lokathor/bytemuck")
    (synopsis "Crate for mucking around with piles of bytes")
    (description
     "This package provides a crate for mucking around with piles of bytes.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-bytes-1
  (package
    (name "rust-bytes")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nnhpb7jlpj393qnjr1n9n6sgpl3w5ymrwl3pnjmrriam861bh4s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-loom" ,rust-loom-0.7)
                                   ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis "Types and traits for working with bytes")
    (description
     "This package provides Types and traits for working with bytes.")
    (license license:expat)))

(define-public rust-cab-0.6
  (package
    (name "rust-cab")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cab" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lh4wij6wh3hwb6l4jjfyv2rmx34v0n4d2l6q2nda8b71rjjh4hp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-lzxd" ,rust-lzxd-0.2)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/mdsteele/rust-cab")
    (synopsis "Read/write Windows cabinet (CAB) files")
    (description
     "This package provides Read/write Windows cabinet (CAB) files.")
    (license license:expat)))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jbmjhn4y94l2cjcpmbsnrkcqix1v0gl1nbynp417k7j8z0xwjsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jobserver" ,rust-jobserver-0.1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis
     "build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.")
    (description
     "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cfg-aliases-0.2
  (package
    (name "rust-cfg-aliases")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cfg_aliases" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/katharostech/cfg_aliases")
    (synopsis
     "tiny utility to help save you a lot of effort with long winded `#[cfg()]` checks.")
    (description
     "This package provides a tiny utility to help save you a lot of effort with long
winded `#[cfg()]` checks.")
    (license license:expat)))

(define-public rust-checked-int-cast-1
  (package
    (name "rust-checked-int-cast")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "checked_int_cast" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06brva5agm6g12q15f8fidz17akb85q211496p1k2qxhb9mmxk0p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/PeterReid/checked_int_cast")
    (synopsis
     "Conversions between primitive integers with overflow and underflow checking")
    (description
     "This package provides Conversions between primitive integers with overflow and underflow checking.")
    (license license:expat)))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.38")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "009l8vc5p8750vn02z30mblg4pv2qhkbfizhfwmzc6vpy5nr67x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.8)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-windows-bindgen" ,rust-windows-bindgen-0.56))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "This package provides Date and time library for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cidr-0.2
  (package
    (name "rust-cidr")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cidr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0896d3jxgmpz3avqgg37irn8pjhw8xj54i2wjkr8r5dx8l661pvb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitstring" ,rust-bitstring-0.1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/stbuehler/rust-cidr")
    (synopsis "IP network and IP host within network types")
    (description
     "This package provides IP network and IP host within network types.")
    (license license:expat)))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.5.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s37v23gcxkjy4800qgnkxkpliz68vslpr5sgn1xar56hmnkfzxr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap-builder" ,rust-clap-builder-4)
                       ("rust-clap-derive" ,rust-clap-derive-4))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-clap-cargo" ,rust-clap-cargo-0.14)
                                   ("rust-humantime" ,rust-humantime-2)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-shlex" ,rust-shlex-1)
                                   ("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-trycmd" ,rust-trycmd-0.15))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-builder-4
  (package
    (name "rust-clap-builder")
    (version "4.5.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m6w10l2f65h3ch0d53lql6p26xxrh20ffipra9ysjsfsjmq1g0r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-clap-lex" ,rust-clap-lex-0.7)
                       ("rust-strsim" ,rust-strsim-0.11)
                       ("rust-terminal-size" ,rust-terminal-size-0.4)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.2))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-cargo-0.14
  (package
    (name "rust-clap-cargo")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-cargo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gmlr0cahj7nfzcqfc16z76rb8ar3nnidm9snx4bi5psrrlymci3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstyle" ,rust-anstyle-1)
                       ("rust-cargo-metadata" ,rust-cargo-metadata-0.18)
                       ("rust-clap" ,rust-clap-4))))
    (home-page "https://github.com/crate-ci/clap-cargo")
    (synopsis "Re-usable CLI flags for `cargo` plugins")
    (description
     "This package provides Re-usable CLI flags for `cargo` plugins.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-4
  (package
    (name "rust-clap-complete")
    (version "4.5.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_complete" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qmhfyanci10wdinc5jrga7p9sb38jy73v7b7cpyb0dwjkg77g46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-clap-lex" ,rust-clap-lex-0.7)
                       ("rust-completest" ,rust-completest-0.4)
                       ("rust-completest-pty" ,rust-completest-pty-0.5)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-shlex" ,rust-shlex-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-clap" ,rust-clap-4)
                                   ("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-trycmd" ,rust-trycmd-0.15))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description
     "This package provides Generate shell completion scripts for your clap::Command.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.5.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Parse command line argument by defining a struct, derive crate")
    (description
     "This package provides Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-codspeed-2
  (package
    (name "rust-codspeed")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codspeed" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15yf7gnb4s1fdvprdpn41yfydxpnv2clyd7lar0ia76zz6fhw2j5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-colored" ,rust-colored-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://codspeed.io")
    (synopsis "Core instrumentation library for CodSpeed")
    (description
     "This package provides Core instrumentation library for @code{CodSpeed}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-codspeed-criterion-compat-2
  (package
    (name "rust-codspeed-criterion-compat")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "codspeed-criterion-compat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sd5g2q2zp6fzk24bzi42kmn94kw3iygkplcwpypgq90kk5sdccf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-codspeed" ,rust-codspeed-2)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-criterion" ,rust-criterion-0.5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-smol" ,rust-smol-2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://codspeed.io")
    (synopsis "Criterion.rs compatibility layer for CodSpeed")
    (description
     "This package provides Criterion.rs compatibility layer for @code{CodSpeed}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-compact-jwt-0.2
  (package
    (name "rust-compact-jwt")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "compact_jwt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0csb8fy33jg9iybgnxbnzpdnp6xv6s0q80jdlcw7hmv8k7qnx9vs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/kanidm/compact-jwt")
    (synopsis "Minimal implementation of JWT for OIDC and other applications")
    (description
     "This package provides Minimal implementation of JWT for OIDC and other applications.")
    (license license:mpl2.0)))

(define-public rust-compact-jwt-0.4
  (package
    (name "rust-compact-jwt")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "compact_jwt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rpj3q92iz2fhfs6z0j41q6gsf9lcc6xdn00pgxk6z4bqyqr6r3b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-kanidm-hsm-crypto" ,rust-kanidm-hsm-crypto-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-kdf" ,rust-openssl-kdf-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/kanidm/compact-jwt")
    (synopsis "Minimal implementation of JWT for OIDC and other applications")
    (description
     "This package provides Minimal implementation of JWT for OIDC and other applications.")
    (license license:mpl2.0)))

(define-public rust-completest-0.4
  (package
    (name "rust-completest")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "completest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nsxyiz1dnwy2pk5n17lbhiijgcy9rc4g5sr1w2als0kwvzy23a1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/assert-rs/completest")
    (synopsis "Run completions for your program")
    (description "This package provides Run completions for your program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-completest-pty-0.5
  (package
    (name "rust-completest-pty")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "completest-pty" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04s30kz6rvi4d7grx8my5siwmfiv6sm09ib152yj5ccxk4mg5lpv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-completest" ,rust-completest-0.4)
                       ("rust-ptyprocess" ,rust-ptyprocess-0.4)
                       ("rust-vt100" ,rust-vt100-0.15))))
    (home-page "https://github.com/assert-rs/completest")
    (synopsis "Run completions for your program")
    (description "This package provides Run completions for your program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-concread-0.5
  (package
    (name "rust-concread")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "concread" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gm09lb67nkiv963bk2h9xqrjaxcr051p38apggrf99cabphr86b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-dhat" ,rust-dhat-0.3)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sptr" ,rust-sptr-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-function-name" ,rust-function-name-0.3)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                                   ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/kanidm/concread/")
    (synopsis "Concurrently Readable Data-Structures for Rust")
    (description
     "This package provides Concurrently Readable Data-Structures for Rust.")
    (license license:mpl2.0)))

(define-public rust-cookie-store-0.21
  (package
    (name "rust-cookie-store")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie_store" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1akbrsgvb66zmbi5kzbanmh10mpqg8khv5anxyv4i4a1x2vycd29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.18)
                       ("rust-idna" ,rust-idna-0.5)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-publicsuffix" ,rust-publicsuffix-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/pfernie/cookie_store")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
     "This package provides Implementation of Cookie storage and retrieval.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cpufeatures-0.2
  (package
    (name "rust-cpufeatures")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpufeatures" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "012m7rrak4girqlii3jnqwrr73gv1i980q4wra5yyyhvzwk5xzjk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
     "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets,
with no_std support and support for mobile targets including Android and iOS")
    (description
     "This package provides Lightweight runtime CPU feature detection for aarch64, loongarch64, and
x86/x86_64 targets, with no_std support and support for mobile targets including
Android and @code{iOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc32fast-1
  (package
    (name "rust-crc32fast")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crc32fast" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://github.com/srijs/rust-crc32fast")
    (synopsis "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
     "This package provides Fast, SIMD-accelerated CRC32 (IEEE) checksum computation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cron-0.12
  (package
    (name "rust-cron")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cron" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01qc1cnhibxh55pwv3mwaxvfgbjpgk1lfl7an5m4ljvv0xrkx33g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-once-cell" ,rust-once-cell-1))
       #:cargo-development-inputs (("rust-chrono-tz" ,rust-chrono-tz-0.6))))
    (home-page "https://github.com/zslayton/cron")
    (synopsis "cron expression parser and schedule explorer.")
    (description
     "This package provides a cron expression parser and schedule explorer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-0.8
  (package
    (name "rust-crossbeam")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam")
    (synopsis "Tools for concurrent programming")
    (description "This package provides tools for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-queue-0.3
  (package
    (name "rust-crossbeam-queue")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-queue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d8y8y3z48r9javzj67v3p2yfswd278myz1j9vzc4sp7snslc0yz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-queue")
    (synopsis "Concurrent queues")
    (description "This package provides Concurrent queues.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-utils-0.8
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-loom" ,rust-loom-0.7))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description "This package provides Utilities for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-0.5
  (package
    (name "rust-crypto")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0inhdj8fz7hfz7p8kzp4cqj8pb0dkqsgc0gk6w687w7qj9a6w7mz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-password-hash" ,rust-password-hash-0.5)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-universal-hash" ,rust-universal-hash-0.5))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Facade crate for all of the RustCrypto traits (e.g. `aead`, `cipher`, `digest`)")
    (description
     "This package provides Facade crate for all of the @code{RustCrypto} traits (e.g. `aead`, `cipher`,
`digest`).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dav1d-0.10
  (package
    (name "rust-dav1d")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dav1d" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qd13sm1bfbc5chjgrzk4syffkky994lkyzhqrqklqxg1fj58jqd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-av-data" ,rust-av-data-0.4)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-dav1d-sys" ,rust-dav1d-sys-0.8)
                       ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "libdav1d bindings")
    (description "This package provides libdav1d bindings.")
    (license license:expat)))

(define-public rust-dav1d-sys-0.8
  (package
    (name "rust-dav1d-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dav1d-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "158fqp97ny3206sydnimc2jy1c1gcxa4llqvvkp3ii2dixg1rjvf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list dav1d))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "FFI bindings to dav1d")
    (description "This package provides FFI bindings to dav1d.")
    (license license:expat)))

(define-public rust-dbus-tokio-0.7
  (package
    (name "rust-dbus-tokio")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dbus-tokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04xd3z2dnjv4d45kj3wqnwbnwllrp1zsg8v3q0qp2rxwb7a8hxh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-crossroads" ,rust-dbus-crossroads-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux for inter-process communication")
    (description
     "This package provides Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux
for inter-process communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dcv-color-primitives-0.6
  (package
    (name "rust-dcv-color-primitives")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dcv-color-primitives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k3384cfwhc8z9pxj0gb6dz8sjcwsamnpbrkbc570sfhzvnn5b87"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-paste" ,rust-paste-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/aws/dcv-color-primitives")
    (synopsis "a library to perform image color model conversion")
    (description
     "This package provides a library to perform image color model conversion.")
    (license license:expat-0)))

(define-public rust-deflate64-0.1
  (package
    (name "rust-deflate64")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deflate64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06scix17pa7wzzfsnhkycpcc6s04shs49cdaxx2k1sl0226jnsfs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/anatawa12/deflate64-rs#readme")
    (synopsis "Deflate64 implementation based on .NET's implementation")
    (description
     "This package provides Deflate64 implementation based on .NET's implementation.")
    (license license:expat)))

(define-public rust-defmt-0.3
  (package
    (name "rust-defmt")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "defmt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q79ryg6q1i2nfhs5wcrc018y8sblvsjlryl45qqi2v6c8id57d9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-defmt-macros" ,rust-defmt-macros-0.3))))
    (home-page "https://knurling.ferrous-systems.com/")
    (synopsis
     "highly efficient logging framework that targets resource-constrained devices, like microcontrollers")
    (description
     "This package provides a highly efficient logging framework that targets
resource-constrained devices, like microcontrollers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devd-rs-0.3
  (package
    (name "rust-devd-rs")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "devd-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15fkr028silhhk2d6m0lhw9mql3nzhja7h01zi30nlchnl2g24wk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-nom" ,rust-nom-7))))
    (home-page "https://codeberg.org/valpackett/devd-rs")
    (synopsis
     "An interface to devd, the device hotplug daemon on FreeBSD and DragonFlyBSD")
    (description
     "This package provides An interface to devd, the device hotplug daemon on @code{FreeBSD} and
@code{DragonFlyBSD}.")
    (license (list license:unlicense license:expat))))

(define-public rust-dhat-0.3
  (package
    (name "rust-dhat")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dhat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09xq763lpf0kdv4fzbdgxkd4sgv3p08dwrz41kg37qi88vc13kcq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-mintex" ,rust-mintex-0.1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thousands" ,rust-thousands-0.2))))
    (home-page "https://github.com/nnethercote/dhat-rs")
    (synopsis "library for heap profiling and ad hoc profiling with DHAT.")
    (description
     "This package provides a library for heap profiling and ad hoc profiling with
DHAT.")
    (license (list license:expat license:asl2.0))))

(define-public rust-document-features-0.2
  (package
    (name "rust-document-features")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "document-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "182h528pjyv4ppil2pd2nir46qrb393x5kvm4y51yhnjmgm6jsfb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-litrs" ,rust-litrs-0.4))))
    (home-page "https://slint.rs")
    (synopsis
     "Extract documentation for the feature flags from comments in Cargo.toml")
    (description
     "This package provides Extract documentation for the feature flags from comments in Cargo.toml.")
    (license (list license:expat license:asl2.0))))

(define-public rust-enum-iterator-2
  (package
    (name "rust-enum-iterator")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-iterator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0df6mighw65h5cm3l0fl37hq35rq31zz8773v19f26dfngkbk062"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-enum-iterator-derive" ,rust-enum-iterator-derive-1))))
    (home-page "https://github.com/stephaneyfx/enum-iterator")
    (synopsis
     "Tools to iterate over all values of a type (e.g. all variants of an enumeration)")
    (description
     "This package provides tools to iterate over all values of a type (e.g. all
variants of an enumeration).")
    (license license:bsd-0)))

(define-public rust-enum-iterator-derive-1
  (package
    (name "rust-enum-iterator-derive")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-iterator-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nz6kz8jz2w1vy4y3r0mb8pa5nj3y77mdxdn3b38db322cf9kax1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/stephaneyfx/enum-iterator")
    (synopsis "Procedural macro to derive Sequence")
    (description "This package provides Procedural macro to derive Sequence.")
    (license license:bsd-0)))

(define-public rust-escargot-0.5
  (package
    (name "rust-escargot")
    (version "0.5.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "escargot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1djza8py0rrgc5jlrijfrym0ljmr0cmy0rvjicafz6j5klzg4060"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/crate-ci/escargot")
    (synopsis "Cargo API written in Paris")
    (description "This package provides Cargo API written in Paris.")
    (license (list license:expat license:asl2.0))))

(define-public rust-event-listener-5
  (package
    (name "rust-event-listener")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "event-listener" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fkm6q4hjn61wl52xyqyyxai0x9w0ngrzi0wf1qsf8vhsadvwck0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-concurrent-queue" ,rust-concurrent-queue-2)
                       ("rust-loom" ,rust-loom-0.7)
                       ("rust-parking" ,rust-parking-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-portable-atomic-util" ,rust-portable-atomic-util-0.2))))
    (home-page "https://github.com/smol-rs/event-listener")
    (synopsis "Notify async tasks or threads")
    (description "This package provides Notify async tasks or threads.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fantoccini-0.21
  (package
    (name "rust-fantoccini")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fantoccini" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rakgi4xd54n1j9miv2fvzjk1i8g8sfip24ybb6i6415k0zbclnx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-cookie" ,rust-cookie-0.18)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.27)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.6)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-macros" ,rust-openssl-macros-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-webdriver" ,rust-webdriver-0.50))
       #:cargo-development-inputs (("rust-hyper" ,rust-hyper-1)
                                   ("rust-hyper-util" ,rust-hyper-util-0.1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/jonhoo/fantoccini")
    (synopsis
     "High-level API for programmatically interacting with web pages through WebDriver")
    (description
     "This package provides High-level API for programmatically interacting with web pages through
@code{WebDriver}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-faster-hex-0.5
  (package
    (name "rust-faster-hex")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "faster-hex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08x05gdd5hsxmhbk820043abc1wqmgms56amawqmcr45zn1ax2yn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/NervosFoundation/faster-hex")
    (synopsis "Fast hex encoding")
    (description "This package provides Fast hex encoding.")
    (license license:expat)))

(define-public rust-fastrand-2
  (package
    (name "rust-fastrand")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fastrand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19nyzdq3ha4g173364y2wijmd6jlyms8qx40daqkxsnl458jmh78"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2))))
    (home-page "https://github.com/smol-rs/fastrand")
    (synopsis "simple and fast random number generator")
    (description
     "This package provides a simple and fast random number generator.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fernet-0.2
  (package
    (name "rust-fernet")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fernet" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0czxf8xqhj2pimwqqfp679ddk2pb2lmhgv6a5kkrwfs8x5gp4sy6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/mozilla-services/fernet-rs/")
    (synopsis "An implementation of fernet in Rust")
    (description "This package provides An implementation of fernet in Rust.")
    (license license:mpl2.0)))

(define-public rust-fido-hid-rs-0.5
  (package
    (name "rust-fido-hid-rs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fido-hid-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zcc0411rnw6ikwlllcks2g5sihqrwdn5q4dfi8b78hjbwih205c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bindgen" ,rust-bindgen-0.65)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-udev" ,rust-udev-0.7)
                       ("rust-windows" ,rust-windows-0.41))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "USB HID library for FIDO authenticators")
    (description
     "This package provides USB HID library for FIDO authenticators.")
    (license license:mpl2.0)))

(define-public rust-file-id-0.1
  (package
    (name "rust-file-id")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "file-id" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hx8zmiqpydj4b471nd1llj1jb8bmjxbwqmq1jy92bm8dhgfffz1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis
     "Utility for reading inode numbers (Linux, MacOS) and file IDs (Windows)")
    (description
     "This package provides Utility for reading inode numbers (Linux, @code{MacOS}) and file IDs (Windows).")
    (license (list license:cc0 license:artistic2.0))))

(define-public rust-filetime-0.2
  (package
    (name "rust-filetime")
    (version "0.2.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "filetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l259b9b70n869jlv4zwcx5yff1p937k848mx17jr1z3lks1sh5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libredox" ,rust-libredox-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/alexcrichton/filetime")
    (synopsis "Platform-agnostic accessors of timestamps in File metadata")
    (description
     "This package provides Platform-agnostic accessors of timestamps in File metadata.")
    (license (list license:expat license:asl2.0))))

(define-public rust-flate2-1
  (package
    (name "rust-flate2")
    (version "1.0.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flate2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "083rg629001bizy25ddhlsmb9s4a297hh1d4vv7x1fv9isz1n8bz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cloudflare-zlib-sys" ,rust-cloudflare-zlib-sys-0.3)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-libz-ng-sys" ,rust-libz-ng-sys-1)
                       ("rust-libz-rs-sys" ,rust-libz-rs-sys-0.2)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-miniz-oxide" ,rust-miniz-oxide-0.7))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis
     "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.
Supports miniz_oxide and multiple zlib implementations. Supports zlib, gzip,
and raw deflate streams.")
    (description
     "This package provides DEFLATE compression and decompression exposed as Read/@code{BufRead/Write}
streams.  Supports miniz_oxide and multiple zlib implementations.  Supports
zlib, gzip, and raw deflate streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fluent-uri-0.3
  (package
    (name "rust-fluent-uri")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fluent-uri" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rgp9mv03bifxysy863kf38aagvyl76ca6gd3dcx6iyzjrfvc60r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-borrow-or-share" ,rust-borrow-or-share-0.2)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/yescallop/fluent-uri-rs")
    (synopsis "generic URI/IRI handling library compliant with RFC 3986/3987.")
    (description
     "This package provides a generic URI/IRI handling library compliant with RFC
3986/3987.")
    (license license:expat)))

(define-public rust-foldhash-0.1
  (package
    (name "rust-foldhash")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "foldhash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18in1a8mjcg43pfrdkhwzr0w988zb2bmb6sqwi07snjlkhvcc7pq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/orlp/foldhash")
    (synopsis
     "fast, non-cryptographic, minimally DoS-resistant hashing algorithm.")
    (description
     "This package provides a fast, non-cryptographic, minimally @code{DoS-resistant}
hashing algorithm.")
    (license license:zlib)))

(define-public rust-fraction-0.15
  (package
    (name "rust-fraction")
    (version "0.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fraction" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rss1021dpzr12xl3dyqyj36ji4vv4f83yy9im039cx1y0zqw58g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-juniper" ,rust-juniper-0.15)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-postgres-types" ,rust-postgres-types-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/dnsl48/fraction.git")
    (synopsis "Lossless fractions and decimals; drop-in float replacement")
    (description
     "This package provides Lossless fractions and decimals; drop-in float replacement.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fs4-0.8
  (package
    (name "rust-fs4")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0m3pk9iq16b61p3g643234bfy0kdbyjymxczklafy2fsn81qgp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-smol" ,rust-smol-2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-libc" ,rust-libc-0.2)
                                   ("rust-smol-potat" ,rust-smol-potat-1)
                                   ("rust-tempdir" ,rust-tempdir-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/al8n/fs4-rs")
    (synopsis
     "No libc, pure Rust cross-platform file locks. Original fs2, now supports async and replace libc by rustix")
    (description
     "This package provides No libc, pure Rust cross-platform file locks.  Original fs2, now supports async
and replace libc by rustix.")
    (license (list license:expat license:asl2.0))))

(define-public rust-function-name-0.3
  (package
    (name "rust-function-name")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "function_name" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19wq3s8ajl1p7b01kjb2zwgj3n7rf6pcabp1bw5r82bdi5x5gaxi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-function-name-proc-macro" ,rust-function-name-proc-macro-0.3))))
    (home-page "https://crates.io/crates/function_name")
    (synopsis "macro that expands to the name of the annotated function")
    (description
     "This package provides macro that expands to the name of the annotated function.")
    (license license:expat)))

(define-public rust-function-name-proc-macro-0.3
  (package
    (name "rust-function-name-proc-macro")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "function_name-proc-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cvkp002v6zzphrv0s9vgq68vclfkv4sni4mznh6gp8lwghn8d37"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://crates.io/crates/function_name")
    (synopsis "macro that expands to the name of the annotated function")
    (description
     "This package provides macro that expands to the name of the annotated function.")
    (license license:expat)))

(define-public rust-futures-0.3
  (package
    (name "rust-futures")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3))
       #:cargo-development-inputs (("rust-assert-matches" ,rust-assert-matches-1)
                                   ("rust-pin-project" ,rust-pin-project-1)
                                   ("rust-static-assertions" ,rust-static-assertions-1)
                                   ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.")
    (description
     "This package provides An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-channel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.")
    (description
     "This package provides Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-concurrency-3
  (package
    (name "rust-futures-concurrency")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-concurrency" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mki9bvr13kzmsp1i4vhgyy72swp6vs595pblfmxjkrs1fri2d0i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-pin-project" ,rust-pin-project-1))
       #:cargo-development-inputs (("rust-async-io" ,rust-async-io-1)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-lite" ,rust-futures-lite-1))))
    (home-page "https://github.com/yoshuawuyts/futures-concurrency")
    (synopsis "Structured concurrency operations for async Rust")
    (description
     "This package provides Structured concurrency operations for async Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-portable-atomic" ,rust-portable-atomic-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.")
    (description
     "This package provides The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-executor-0.3
  (package
    (name "rust-futures-executor")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-executor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Executors for asynchronous tasks based on the futures-rs library.")
    (description
     "This package provides Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-io-0.3
  (package
    (name "rust-futures-io")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-io" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.")
    (description
     "This package provides The `@code{AsyncRead`}, `@code{AsyncWrite`}, `@code{AsyncSeek`}, and
`@code{AsyncBufRead`} traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.")
    (description
     "This package provides The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-sink" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.")
    (description
     "This package provides The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-task" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.")
    (description "This package provides tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-macro" ,rust-futures-macro-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Common utilities and extension traits for the futures-rs library.")
    (description
     "This package provides Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gethostname-0.5
  (package
    (name "rust-gethostname")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gethostname" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c0y7mc8bpsjkvvykqcic9cynskvm82iz4fn4335pmhqd2m5adnw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/swsnr/gethostname.rs")
    (synopsis "gethostname for all platforms")
    (description "This package provides gethostname for all platforms.")
    (license license:asl2.0)))

(define-public rust-gif-0.13
  (package
    (name "rust-gif")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-color-quant" ,rust-color-quant-1)
                       ("rust-weezl" ,rust-weezl-0.1))))
    (home-page "https://github.com/image-rs/image-gif")
    (synopsis "GIF de- and encoder")
    (description "This package provides GIF de- and encoder.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gimli-0.29
  (package
    (name "rust-gimli")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gimli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zgzprnjaawmg6zyic4f2q2hc39kdhn116qnkqpgvsasgc3x9v20"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis "library for reading and writing the DWARF debugging format.")
    (description
     "This package provides a library for reading and writing the DWARF debugging
format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-0.64
  (package
    (name "rust-gix")
    (version "0.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kv3w87h8gbwg16k7nqzalb417v8yzh7fq0nh28350nckz91916p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-archive" ,rust-gix-archive-0.13)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-config" ,rust-gix-config-0.38)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.6)
                       ("rust-gix-discover" ,rust-gix-discover-0.33)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.23)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-odb" ,rust-gix-odb-0.61)
                       ("rust-gix-pack" ,rust-gix-pack-0.51)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.45)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-revision" ,rust-gix-revision-0.27)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.11)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.12)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.11)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Interact with git repositories just like git would")
    (description
     "This package provides Interact with git repositories just like git would.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-actor-0.31
  (package
    (name "rust-gix-actor")
    (version "0.31.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wm3i9g69hkfhwiw1c4z9fr1hkfxsfxyzdh07b637f1lgqsm9r50"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "way to identify git actors")
    (description "This package provides a way to identify git actors.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-archive-0.13
  (package
    (name "rust-gix-archive")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m4nwxcfrivnf74kx38sxgcalba8nbl2fq4xlvad28q2vzmvpdk3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.13)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "archive generation from of a worktree stream")
    (description
     "This package provides archive generation from of a worktree stream.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-attributes-0.22
  (package
    (name "rust-gix-attributes")
    (version "0.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mv293x9l976arqj8sqsbk2ar6hibmninr03nwl8qa41gsffjz73"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing .gitattributes files")
    (description
     "This package provides a crate of the gitoxide project dealing .gitattributes
files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-command-0.3
  (package
    (name "rust-gix-command")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yqxzmx3a3fxwii6gc5gyr105sfax1ad86h26az1r2fscxw8cxhd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-shell-words" ,rust-shell-words-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project handling internal git command execution")
    (description
     "This package provides a crate of the gitoxide project handling internal git
command execution.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-commitgraph-0.24
  (package
    (name "rust-gix-commitgraph")
    (version "0.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y7wc0y0xb0kh3c22pj3ns04zdqglqb22gj71kn3cn2ngzv0cfqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Read-only access to the git commitgraph file format")
    (description
     "This package provides Read-only access to the git commitgraph file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-0.38
  (package
    (name "rust-gix-config")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n768fy37w3wy8jsjk34draw8fcljq44yrf8qay9xw0v7p83zx98"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "git-config file parser and editor from the gitoxide project")
    (description
     "This package provides a git-config file parser and editor from the gitoxide
project.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-value-0.14
  (package
    (name "rust-gix-config-value")
    (version "0.14.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0giq3js6ls6hwrv4hphh4k22b6mgnhn1cfvpn9qxq5fxfiyrja5k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project providing git-config value parsing")
    (description
     "This package provides a crate of the gitoxide project providing git-config value
parsing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-credentials-0.24
  (package
    (name "rust-gix-credentials")
    (version "0.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17kk6k9g2in27kswc0b8qvzhs1kw9l7gbhz60hp21lg46bsqi18r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project to interact with git credentials helpers")
    (description
     "This package provides a crate of the gitoxide project to interact with git
credentials helpers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-date-0.8
  (package
    (name "rust-gix-date")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h5qygjxr8p2g2vdrzpvx0plnyy9gsym29wjxc5fx48ly8qnkvcy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project parsing dates the way git does")
    (description
     "This package provides a crate of the gitoxide project parsing dates the way git
does.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-diff-0.44
  (package
    (name "rust-gix-diff")
    (version "0.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17bci0q2d4bhm6g1gx6ibzbxjj6ykxy6303x8q4rgd85lg4db5hr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Calculate differences between various git objects")
    (description
     "This package provides Calculate differences between various git objects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-dir-0.6
  (package
    (name "rust-gix-dir")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r8kif2i622saw068c63jh08q64a5qixnggxgdsjvp80m9wmd5qc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-discover" ,rust-gix-discover-0.33)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing with directory walks")
    (description
     "This package provides a crate of the gitoxide project dealing with directory
walks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-discover-0.33
  (package
    (name "rust-gix-discover")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03j52f646q05igg4bln6svjxhxiz944khinj7sx33jy3rqqjfrk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.45)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Discover git repositories and check if a directory is a git repository")
    (description
     "This package provides Discover git repositories and check if a directory is a git repository.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-features-0.38
  (package
    (name "rust-gix-features")
    (version "0.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sfw6zs3qgmlqjkj4cvyfz6q6dgdlw1d16c7yckwgyg5kyn4aw5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-28)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate to integrate various capabilities using compile-time feature flags")
    (description
     "This package provides a crate to integrate various capabilities using
compile-time feature flags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-filter-0.11
  (package
    (name "rust-gix-filter")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06m6ph3b67696ckq5gfn9pwm77sh507km7sfzx6my9r8v8w7fm76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.17)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project implementing git filters")
    (description
     "This package provides a crate of the gitoxide project implementing git filters.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.11
  (package
    (name "rust-gix-fs")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yix2q3k3mniw0xkviz1sj1qfkfrw0bwk03nsz2b25yzgk19kpva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fastrand" ,rust-fastrand-2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate providing file system specific utilities to `gitoxide`")
    (description
     "This package provides a crate providing file system specific utilities to
`gitoxide`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-glob-0.16
  (package
    (name "rust-gix-glob")
    (version "0.16.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ixlbibj1lrs2vx6vv19dg01lbsj9lsws4r8x6mwhp16z9dg2zgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing with pattern matching")
    (description
     "This package provides a crate of the gitoxide project dealing with pattern
matching.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ignore-0.11
  (package
    (name "rust-gix-ignore")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09x5vf27pmi7sgnpqw5mfk0n7a5di69l88n8mphx8573k27znsjy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing .gitignore files")
    (description
     "This package provides a crate of the gitoxide project dealing .gitignore files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-index-0.33
  (package
    (name "rust-gix-index")
    (version "0.33.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gyply1fzp0wbl8jvckiw4hyv34na7lq0jd4z14bp15xapml96ls"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "work-in-progress crate of the gitoxide project dedicated implementing the git index file")
    (description
     "This package provides a work-in-progress crate of the gitoxide project dedicated
implementing the git index file.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-lock-14
  (package
    (name "rust-gix-lock")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17g1sknpvjqaq2s29c693mbmkp8sign0174qfi3n3x7ijzi7zg73"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "git-style lock-file implementation")
    (description "This package provides a git-style lock-file implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-macros-0.1
  (package
    (name "rust-gix-macros")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05ycxnh5sxjsn4lvay309n6knr8ksvkb6zx6f50ik24zc4iyk74r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Proc-macro utilities for gix")
    (description "This package provides Proc-macro utilities for gix.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-mailmap-0.23
  (package
    (name "rust-gix-mailmap")
    (version "0.23.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024jy339znwi11pz826favmn6in5fn73ygskq33r19dnxnkaqvgg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project for parsing mailmap files")
    (description
     "This package provides a crate of the gitoxide project for parsing mailmap files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-negotiate-0.13
  (package
    (name "rust-gix-negotiate")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0962x6gxhwp55kyr26hzsswv8qbg9h1f16x8kd8n7fq7cgxpkj4y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project implementing negotiation algorithms")
    (description
     "This package provides a crate of the gitoxide project implementing negotiation
algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-object-0.42
  (package
    (name "rust-gix-object")
    (version "0.42.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11p3pynmriglj5j3hzh5qa4sz7pnhzxxzr1w85xzmhp7ni32zni5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Immutable and mutable git objects with decoding and encoding support")
    (description
     "This package provides Immutable and mutable git objects with decoding and encoding support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-odb-0.61
  (package
    (name "rust-gix-odb")
    (version "0.61.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16xwwfjna9m3rd66hkhcyps6s3bq23r5wpbxpfixi4qxakz89lr0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-pack" ,rust-gix-pack-0.51)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements various git object databases")
    (description "This package implements various git object databases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pack-0.51
  (package
    (name "rust-gix-pack")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aajbwpmfzg7a33yzqfjz2rmczvbask13hd19gwmvrgz3x4r819y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements git packs and related data structures")
    (description
     "This package implements git packs and related data structures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-packetline-blocking-0.17
  (package
    (name "rust-gix-packetline-blocking")
    (version "0.17.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline-blocking" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jp1fz5mqbikh1xfrxyc1qv57lnh62crg2fmwhr4fa1xi8vl47f3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "duplicate of `gix-packetline` with the `blocking-io` feature pre-selected")
    (description
     "This package provides a duplicate of `gix-packetline` with the `blocking-io`
feature pre-selected.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-path-0.10
  (package
    (name "rust-gix-path")
    (version "0.10.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n9b71kwwswibnhw99zb7y3ya9ngndsw1rwdpj54sd1ivaxxa8wd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project dealing paths and their conversions")
    (description
     "This package provides a crate of the gitoxide project dealing paths and their
conversions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pathspec-0.7
  (package
    (name "rust-gix-pathspec")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0in0np7i0bx9rcz9lihm1s236814177y035299n3ij2dz2wd21yk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing magical pathspecs")
    (description
     "This package provides a crate of the gitoxide project dealing magical pathspecs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-prompt-0.8
  (package
    (name "rust-gix-prompt")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vwkl0dgimli7m8bhpx35n42p20hssyqkscrl1qnlvabpv99a1by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project for handling prompts in the terminal")
    (description
     "This package provides a crate of the gitoxide project for handling prompts in
the terminal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-protocol-0.45
  (package
    (name "rust-gix-protocol")
    (version "0.45.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17dav9m6spm0m05m6wxxicqwqgdikccr4w4lfyypfhgji67dmn5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-transport" ,rust-gix-transport-0.42)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project for implementing git protocols")
    (description
     "This package provides a crate of the gitoxide project for implementing git
protocols.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.45
  (package
    (name "rust-gix-ref")
    (version "0.45.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dcv2pgaz6gzhx38zw0qh9pkmhqhf4hqq2gf7waia9snlnh9cvk3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.31)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-lock" ,rust-gix-lock-14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-14)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate to handle git references")
    (description "This package provides a crate to handle git references.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-refspec-0.23
  (package
    (name "rust-gix-refspec")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drzc1p43mc8nprxlqm4s86y9b23rsz89dvqghgmsmb25v6zhs38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-revision" ,rust-gix-revision-0.27)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project for parsing and representing refspecs")
    (description
     "This package provides a crate of the gitoxide project for parsing and
representing refspecs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revision-0.27
  (package
    (name "rust-gix-revision")
    (version "0.27.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kmd96vky58yf46g5nxqny8gl3df4447vb6x6w2lp30iq91kxc81"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project dealing with finding names for revisions and parsing specifications")
    (description
     "This package provides a crate of the gitoxide project dealing with finding names
for revisions and parsing specifications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revwalk-0.13
  (package
    (name "rust-gix-revwalk")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q23mcf4ji5q8qi3g86vxxgz4x4ykgqja0kyacgi9bvimg50q0qv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate providing utilities for walking the revision graph")
    (description
     "This package provides a crate providing utilities for walking the revision
graph.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-sec-0.10
  (package
    (name "rust-gix-sec")
    (version "0.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-sec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1prnyh4kzawip2y569hp48lrg408m5cknjjvy0s7yfk9lmpx4iqm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project providing a shared trust model")
    (description
     "This package provides a crate of the gitoxide project providing a shared trust
model.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-status-0.11
  (package
    (name "rust-gix-status")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dipwd6r0nbyipqc94fdlcn60gd1bispkd3brq3x1hv5rf2b1xw3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-diff" ,rust-gix-diff-0.44)
                       ("rust-gix-dir" ,rust-gix-dir-0.6)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project dealing with 'git status'-like functionality")
    (description
     "This package provides a crate of the gitoxide project dealing with git
status'-like functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-submodule-0.12
  (package
    (name "rust-gix-submodule")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zalx2cc2b2sj8kqncbkbpnj1np9lxr09j4yscwmx000m9lhybhg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.7)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.23)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project dealing git submodules")
    (description
     "This package provides a crate of the gitoxide project dealing git submodules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-tempfile-14
  (package
    (name "rust-gix-tempfile")
    (version "14.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0330lm287bxg0p8jsaxaca80v9hjiksb7r6qjpq5q2ryc5dcysh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "tempfile implementation with a global registry to assure cleanup")
    (description
     "This package provides a tempfile implementation with a global registry to assure
cleanup.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-trace-0.1
  (package
    (name "rust-gix-trace")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-trace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zhm2lwqr070rq3bdn4b1zjs7mn7bhlkfgwfap6xspwi11s2c97r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate to provide minimal `tracing` support that can be turned off to zero cost")
    (description
     "This package provides a crate to provide minimal `tracing` support that can be
turned off to zero cost.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.42
  (package
    (name "rust-gix-transport")
    (version "0.42.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n60132nd6wwslf6zh09l0ak713z5cdq5rbwqay9bzizfs1jph17"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-2)
                       ("rust-gix-command" ,rust-gix-command-0.3)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.24)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.17)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.27)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project dedicated to implementing the git transport layer")
    (description
     "This package provides a crate of the gitoxide project dedicated to implementing
the git transport layer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-traverse-0.39
  (package
    (name "rust-gix-traverse")
    (version "0.39.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h48bbjym6fkdhy9q7p8v6rn9ksvkwxp8fs1415cyw8ya66a36g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.24)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.5)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.13)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "crate of the gitoxide project")
    (description "This package provides a crate of the gitoxide project.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-url-0.27
  (package
    (name "rust-gix-url")
    (version "0.27.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mhicbdcchm01bqpyrxa55wpzf7n7jnnyh5bnpqahbm9pcsrpsz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project implementing parsing and serialization of gix-url")
    (description
     "This package provides a crate of the gitoxide project implementing parsing and
serialization of gix-url.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-utils-0.1
  (package
    (name "rust-gix-utils")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p6lschmdrg1j9cd3rm6q96dyrvivzi2305d7ck1588gzpvjs69m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate with `gitoxide` utilities that don't need feature toggles")
    (description
     "This package provides a crate with `gitoxide` utilities that don't need feature
toggles.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-validate-0.8
  (package
    (name "rust-gix-validate")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kqad8a2wdz69ma7hspi21pazgpkrc5hg4iw37gsvca99b9pvhl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Validation functions for various kinds of names in git")
    (description
     "This package provides Validation functions for various kinds of names in git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-0.34
  (package
    (name "rust-gix-worktree")
    (version "0.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19p7xzrl1i4jw2fhzywqkfd00dn58k9nksll0qi7548bprp35xr6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.11)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project for shared worktree related types and utilities.")
    (description
     "This package provides a crate of the gitoxide project for shared worktree
related types and utilities.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-state-0.11
  (package
    (name "rust-gix-worktree-state")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n2wv2fgdryxn1pa0rg2k43lqc7kpjx4665ihnj6f47mnl2n5v9r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-glob" ,rust-gix-glob-0.16)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-index" ,rust-gix-index-0.33)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.34)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "crate of the gitoxide project implementing setting the worktree to a particular state")
    (description
     "This package provides a crate of the gitoxide project implementing setting the
worktree to a particular state.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-stream-0.13
  (package
    (name "rust-gix-worktree-stream")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08gd3fca4jjaygsawpm7s6f3p7rvb3br87ap8ipqahcs4jb4hpg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.22)
                       ("rust-gix-features" ,rust-gix-features-0.38)
                       ("rust-gix-filter" ,rust-gix-filter-0.11)
                       ("rust-gix-fs" ,rust-gix-fs-0.11)
                       ("rust-gix-hash" ,rust-gix-hash-0.14)
                       ("rust-gix-object" ,rust-gix-object-0.42)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.39)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "generate a byte-stream from a git-tree")
    (description
     "This package provides generate a byte-stream from a git-tree.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-0.8
  (package
    (name "rust-gloo")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kdr8ahxl77fby89fvfwq13kqqyyw63pnjpv6gynz4gnbvd9r698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gloo-console" ,rust-gloo-console-0.2)
                       ("rust-gloo-dialogs" ,rust-gloo-dialogs-0.1)
                       ("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-gloo-file" ,rust-gloo-file-0.2)
                       ("rust-gloo-history" ,rust-gloo-history-0.1)
                       ("rust-gloo-net" ,rust-gloo-net-0.3)
                       ("rust-gloo-render" ,rust-gloo-render-0.1)
                       ("rust-gloo-storage" ,rust-gloo-storage-0.2)
                       ("rust-gloo-timers" ,rust-gloo-timers-0.2)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-gloo-worker" ,rust-gloo-worker-0.2))))
    (home-page "https://gloo-rs.web.app/")
    (synopsis "modular toolkit for Rust and WebAssembly")
    (description
     "This package provides a modular toolkit for Rust and @code{WebAssembly}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-console-0.2
  (package
    (name "rust-gloo-console")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-console" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gqd35vn0i5y6hzfrsb2i032p1j832c08sar6dr19gny0lycxdw2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with browser's console")
    (description
     "This package provides Convenience crate for working with browser's console.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-dialogs-0.1
  (package
    (name "rust-gloo-dialogs")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-dialogs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rh2j0l8rbj8pbypxqy99qi2x3hq52sclijs8h47zlkjmij261k7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with dialogs in browser")
    (description
     "This package provides Convenience crate for working with dialogs in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-events-0.1
  (package
    (name "rust-gloo-events")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-events" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z4j14r2lim77s0jm1dpk306jyycmx2kirid33j0b0gdmgw0gcb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with DOM event listeners")
    (description
     "This package provides Convenience crate for working with DOM event listeners.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-file-0.2
  (package
    (name "rust-gloo-file")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-file" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxnd7l8gglv5yqhah6ny329hc0c98vn7h5xg0yv8f0aax75dmd8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with JavaScript files and blobs")
    (description
     "This package provides Convenience crate for working with @code{JavaScript} files and blobs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-history-0.1
  (package
    (name "rust-gloo-history")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-history" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zsy3m5bgah8hyd95sc9b68afn1nhs7g43lkndip1m0fpy85swl5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-events" ,rust-gloo-events-0.1)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.5)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Universal Session History")
    (description "This package provides Universal Session History.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-net-0.3
  (package
    (name "rust-gloo-net")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-net" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0866ih3bff7dwxdfc813pk5nwz2ayyqwi5vbzlax7n4ygly4wsx6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "HTTP requests library for WASM Apps")
    (description "This package provides HTTP requests library for WASM Apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-render-0.1
  (package
    (name "rust-gloo-render")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-render" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3pxj22l489ldakj6521a0f0n1r9v8xrai3k12d9kv7xxm31n9g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with browser's requestAnimationFrame")
    (description
     "This package provides Convenience crate for working with browser's @code{requestAnimationFrame}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-storage-0.2
  (package
    (name "rust-gloo-storage")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-storage" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1074j754a6c21sbmqws5qwaha0a13fikv17ps476zzfvyl5vcsjx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis
     "Convenience crate for working with local and session storage in browser")
    (description
     "This package provides Convenience crate for working with local and session storage in browser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-utils-0.1
  (package
    (name "rust-gloo-utils")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13m59g36spynspvhx0xsaahbkdshn1v03gcjf87s7cvc443wnzq3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for common `web_sys` features")
    (description
     "This package provides Convenience crate for common `web_sys` features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-utils-0.2
  (package
    (name "rust-gloo-utils")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1am31cd6889shb7158bg9zzsjcpvyzxrhfhxgia8rc8k84smam8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for common `web_sys` features")
    (description
     "This package provides Convenience crate for common `web_sys` features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-worker-0.2
  (package
    (name "rust-gloo-worker")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sjiw13069i7bpiyb03w3kyddn3q07fmj4vd60l1l1kqva21aiqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anymap2" ,rust-anymap2-0.13)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-gloo-console" ,rust-gloo-console-0.2)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for working with Web Workers")
    (description
     "This package provides Convenience crate for working with Web Workers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-grpcio-0.12
  (package
    (name "rust-grpcio")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "grpcio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02j3dxlww8m2gf09m2abiw2msgyqkvd2z4w1viibdbhva3535630"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-grpcio-sys" ,rust-grpcio-sys-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/tikv/grpc-rs")
    (synopsis
     "The rust language implementation of gRPC, base on the gRPC c core library")
    (description
     "This package provides The rust language implementation of @code{gRPC}, base on the @code{gRPC} c core
library.")
    (license license:asl2.0)))

(define-public rust-grpcio-sys-0.12
  (package
    (name "rust-grpcio-sys")
    (version "0.12.1+1.46.5-patched")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "grpcio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qpcvarqzwd2lz43z02wxx6rkd3zhk7dm3a2y01l5x5n0cc5sqng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-boringssl-src" ,rust-boringssl-src-0.5)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list grpc))
    (home-page "https://github.com/tikv/grpc-rs")
    (synopsis "FFI bindings to gRPC c core library")
    (description
     "This package provides FFI bindings to @code{gRPC} c core library.")
    (license license:asl2.0)))

(define-public rust-h3-0.0.5
  (package
    (name "rust-h3")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01a8wabm2fvzzhq3a1hffmqw0bjq5j52nzxhc7ixk0mcqbhrs1nm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "An async HTTP/3 implementation")
    (description "This package provides An async HTTP/3 implementation.")
    (license license:expat)))

(define-public rust-h3-0.0.6
  (package
    (name "rust-h3")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ay6bfnj49wdyhvsqf78msdv3zxl32cjfk745z8scirvjsh7axjy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "An async HTTP/3 implementation")
    (description "This package provides An async HTTP/3 implementation.")
    (license license:expat)))

(define-public rust-h3-quinn-0.0.6
  (package
    (name "rust-h3-quinn")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lb5ppa583pvg5qvr7bm5r7y5y8djin5w4yz9p9jz0fgsycivh5q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.5)
                       ("rust-quinn" ,rust-quinn-0.11)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "QUIC transport implementation based on Quinn")
    (description
     "This package provides QUIC transport implementation based on Quinn.")
    (license license:expat)))

(define-public rust-h3-quinn-0.0.7
  (package
    (name "rust-h3-quinn")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h3-quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mfq4kf97vir2kcqh8k5basz8kq85w9xii1na98fmvpw2gs9kiqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-h3" ,rust-h3-0.0.6)
                       ("rust-quinn" ,rust-quinn-0.11)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h3")
    (synopsis "QUIC transport implementation based on Quinn")
    (description
     "This package provides QUIC transport implementation based on Quinn.")
    (license license:expat)))

(define-public rust-hashbrown-0.14
  (package
    (name "rust-hashbrown")
    (version "0.14.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rkyv" ,rust-rkyv-0.7)
                                   ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.15
  (package
    (name "rust-hashbrown")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yx4xq091s7i6mw6bn77k8cp4jrpcac149xr32rg8szqsj27y20y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-foldhash" ,rust-foldhash-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hermit-abi-0.3
  (package
    (name "rust-hermit-abi")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hermit-abi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/hermit-os/hermit-rs")
    (synopsis "Hermit system calls definitions")
    (description "This package provides Hermit system calls definitions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hostname-validator-1
  (package
    (name "rust-hostname-validator")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hostname-validator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qh5sxkckalibc28029ndnfd7w0s8mwvb68d82xbb25gr55acn7m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/pop-os/hostname-validator")
    (synopsis "Validate hostnames according to IETF RFC 1123")
    (description
     "This package provides Validate hostnames according to IETF RFC 1123.")
    (license license:expat)))

(define-public rust-http-1
  (package
    (name "rust-http")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-itoa" ,rust-itoa-1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/hyperium/http")
    (synopsis "set of types for representing HTTP requests and responses.")
    (description
     "This package provides a set of types for representing HTTP requests and
responses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-1
  (package
    (name "rust-hyper")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16pspkgizcnsr1qcpqvm5l45nfwk7244q9av56cqqwm40slg1gxv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))
       #:cargo-development-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                                   ("rust-futures-channel" ,rust-futures-channel-0.3)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-http-body-util" ,rust-http-body-util-0.1)
                                   ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-spmc" ,rust-spmc-0.3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4)
                                   ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://hyper.rs")
    (synopsis "fast and correct HTTP library.")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-hyper-rustls-0.27
  (package
    (name "rust-hyper-rustls")
    (version "0.27.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ma1wyfnqnkz7zyr7wpply3xfvlijd0rqqhb6ajs28c9jhnbxr2y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-platform-verifier" ,rust-rustls-platform-verifier-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description
     "This package provides Rustls+hyper integration for pure rust HTTPS.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-hyper-util-0.1
  (package
    (name "rust-hyper-util")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-bytes" ,rust-bytes-1)
                                   ("rust-http-body-util" ,rust-http-body-util-0.1)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-pnet-datalink" ,rust-pnet-datalink-0.35)
                                   ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://hyper.rs")
    (synopsis "hyper utilities")
    (description "This package provides hyper utilities.")
    (license license:expat)))

(define-public rust-idlset-0.2
  (package
    (name "rust-idlset")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "idlset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yyrx3prh5lvgrjxg046sd28hgjxvwg1f2kinffwwv1c4x8831gg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-smallvec" ,rust-smallvec-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/kanidm/idlset/")
    (synopsis "Fast u64 set operations library")
    (description "This package provides Fast u64 set operations library.")
    (license license:mpl2.0)))

(define-public rust-image-0.24
  (package
    (name "rust-image")
    (version "0.24.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17gnr6ifnpzvhjf6dwbl9hki8x6bji5mwcqp0048x1jm5yfi742n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-color-quant" ,rust-color-quant-1)
                       ("rust-dav1d" ,rust-dav1d-0.10)
                       ("rust-dcv-color-primitives" ,rust-dcv-color-primitives-0.6)
                       ("rust-exr" ,rust-exr-1)
                       ("rust-gif" ,rust-gif-0.13)
                       ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
                       ("rust-mp4parse" ,rust-mp4parse-0.17)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-png" ,rust-png-0.17)
                       ("rust-qoi" ,rust-qoi-0.4)
                       ("rust-ravif" ,rust-ravif-0.11)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rgb" ,rust-rgb-0.8)
                       ("rust-tiff" ,rust-tiff-0.9)
                       ("rust-webp" ,rust-webp-0.2))
       #:cargo-development-inputs (("rust-crc32fast" ,rust-crc32fast-1)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.3)
                                   ("rust-num-complex" ,rust-num-complex-0.4)
                                   ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/image-rs/image")
    (synopsis
     "Imaging library. Provides basic image processing and encoders/decoders for common image formats")
    (description
     "This package provides Imaging library.  Provides basic image processing and encoders/decoders for
common image formats.")
    (license (list license:expat license:asl2.0))))

(define-public rust-implicit-clone-0.3
  (package
    (name "rust-implicit-clone")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "implicit-clone" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lxzpz48392nwyf47dqlsj4n3q61zskfziyafd3v5k1hghg21mng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/yewstack/implicit-clone")
    (synopsis "Immutable types and ImplicitClone trait similar to Copy")
    (description
     "This package provides Immutable types and @code{ImplicitClone} trait similar to Copy.")
    (license (list license:expat license:asl2.0))))

(define-public rust-intel-mkl-src-0.5
  (package
    (name "rust-intel-mkl-src")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "intel-mkl-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "155q49a7nfbq1lllchsyx8jv2q2pijrjh1w08awvrbjyfcxb6q3j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-intel-mkl-tool" ,rust-intel-mkl-tool-0.1))))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "Redistribution of Intel(R) MKL as a crate")
    (description
     "This package provides Redistribution of Intel(R) MKL as a crate.")
    (license (license:non-copyleft "file://License.txt"))))

(define-public rust-intel-mkl-tool-0.1
  (package
    (name "rust-intel-mkl-tool")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "intel-mkl-tool" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1myyrxvmyij4c60w9x15npwzhlbjm8y8c94lvfsnrl5pbyakz8md"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-curl" ,rust-curl-0.4)
                       ("rust-dirs" ,rust-dirs-2)
                       ("rust-env-logger" ,rust-env-logger-0.7)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-structopt" ,rust-structopt-0.3)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-zstd" ,rust-zstd-0.5))))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "Seek Intel(R) MKL library from system")
    (description
     "This package provides Seek Intel(R) MKL library from system.")
    (license license:expat)))

(define-public rust-io-kit-sys-0.4
  (package
    (name "rust-io-kit-sys")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "io-kit-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ysy5k3wf54yangy25hkj10xx332cj2hb937xasg6riziv7yczk1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-mach2" ,rust-mach2-0.4))))
    (home-page "https://github.com/jtakakura/io-kit-rs")
    (synopsis "Bindings to IOKit for macOS")
    (description "This package provides Bindings to IOKit for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-iri-string-0.5
  (package
    (name "rust-iri-string")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iri-string" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18ffjybllc1h81wp1g96b6vym1bq19aajs0qz997xsbyxqs1j1xz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/lo48576/iri-string")
    (synopsis "IRI as string types")
    (description "This package provides IRI as string types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-0.13
  (package
    (name "rust-itertools")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-permutohedron" ,rust-permutohedron-0.2)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros")
    (description
     "This package provides Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jni-utils-0.1
  (package
    (name "rust-jni-utils")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jni-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i56wy306kdcyn21vvfy1aq7mw0i0mk00w0l3y8xwqdd7qn9z7i5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-jni" ,rust-jni-0.19)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/deviceplug/jni-utils-rs")
    (synopsis "Extra Utilities for JNI in Rust")
    (description "This package provides Extra Utilities for JNI in Rust.")
    (license license:bsd-3)))

(define-public rust-jobserver-0.1
  (package
    (name "rust-jobserver")
    (version "0.1.32")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jobserver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-lang/jobserver-rs")
    (synopsis "An implementation of the GNU Make jobserver for Rust.")
    (description
     "This package provides An implementation of the GNU Make jobserver for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.69")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "js-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))
       #:cargo-development-inputs (("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (description
     "This package provides Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-json-pointer-0.3
  (package
    (name "rust-json-pointer")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json-pointer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15r9kkj83h26lk2q9p6q4qpl3kr79789vqff2ci4i6ki9swl3s2z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://gitlab.com/jmap-rs/json-pointer")
    (synopsis
     "crate for parsing and using JSON pointers, as specified in RFC 6901.")
    (description
     "This package provides a crate for parsing and using JSON pointers, as specified
in RFC 6901.")
    (license license:expat)))

(define-public rust-json-schema-test-suite-0.3
  (package
    (name "rust-json-schema-test-suite")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json_schema_test_suite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g2c6nixrq6qyffwvl5iq7cqfca05w0ik5rfx39lpk3107mrb6m5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-json-schema-test-suite-proc-macro" ,rust-json-schema-test-suite-proc-macro-0.3)
                       ("rust-json-schema-test-suite-test-case" ,rust-json-schema-test-suite-test-case-0.3)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/macisamuele/json-schema-test-suite-rs")
    (synopsis
     "Procedural Macro Attribute to run all the test cases described in JSON-Schema-Test-Suite")
    (description
     "This package provides Procedural Macro Attribute to run all the test cases described in
JSON-Schema-Test-Suite.")
    (license license:expat)))

(define-public rust-json-schema-test-suite-proc-macro-0.3
  (package
    (name "rust-json-schema-test-suite-proc-macro")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json_schema_test_suite_proc_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dfj4882yny0kjcbrgy4swddr5vva84pzliafqzlrd0apy6sm5cp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-json-schema-test-suite-test-case" ,rust-json-schema-test-suite-test-case-0.3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/macisamuele/json-schema-test-suite-rs")
    (synopsis
     "Procedural Macro Attribute to run all the test cases described in JSON-Schema-Test-Suite")
    (description
     "This package provides Procedural Macro Attribute to run all the test cases described in
JSON-Schema-Test-Suite.")
    (license license:expat)))

(define-public rust-json-schema-test-suite-test-case-0.3
  (package
    (name "rust-json-schema-test-suite-test-case")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json_schema_test_suite_test_case" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18l69gmynkjk1m354xyjfdv6an2fwvx7459hnr0njaszwqhb0fzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/macisamuele/json-schema-test-suite-rs")
    (synopsis
     "Procedural Macro Attribute to run all the test cases described in JSON-Schema-Test-Suite")
    (description
     "This package provides Procedural Macro Attribute to run all the test cases described in
JSON-Schema-Test-Suite.")
    (license license:expat)))

(define-public rust-jsonschema-0.18
  (package
    (name "rust-jsonschema")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonschema" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y3mq8aagjikdjag2629kjzbd1vsi5vpqy58yh1wp6rc2h3gs2pc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytecount" ,rust-bytecount-0.6)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-fraction" ,rust-fraction-0.15)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-iso8601" ,rust-iso8601-0.6)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-num-cmp" ,rust-num-cmp-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-json-schema-test-suite" ,rust-json-schema-test-suite-0.3)
                                   ("rust-jsonschema-valid" ,rust-jsonschema-valid-0.5)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-mockito" ,rust-mockito-0.31)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-test-case" ,rust-test-case-3)
                                   ("rust-valico" ,rust-valico-3))))
    (home-page "https://github.com/Stranger6667/jsonschema-rs")
    (synopsis "crate for performing JSON schema validation")
    (description
     "This package provides a crate for performing JSON schema validation.")
    (license license:expat)))

(define-public rust-jsonschema-0.21
  (package
    (name "rust-jsonschema")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonschema" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c7bj54rwnpp99d5rnmr60vwqisqvivnh22jzyhvv1mk1fnqjx2h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytecount" ,rust-bytecount-0.6)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-fraction" ,rust-fraction-0.15)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-iso8601" ,rust-iso8601-0.6)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-num-cmp" ,rust-num-cmp-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-referencing" ,rust-referencing-0.21)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid-simd" ,rust-uuid-simd-0.8))
       #:cargo-development-inputs (("rust-codspeed-criterion-compat" ,rust-codspeed-criterion-compat-2)
                                   ("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-mockito" ,rust-mockito-1)
                                   ("rust-test-case" ,rust-test-case-3))))
    (home-page "https://github.com/Stranger6667/jsonschema")
    (synopsis "JSON schema validaton library")
    (description "This package provides JSON schema validaton library.")
    (license license:expat)))

(define-public rust-jsonschema-valid-0.5
  (package
    (name "rust-jsonschema-valid")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonschema-valid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18rjl341pd7lf2c7rqc8c1a0z5vqb3awjmw1ymx7882frmm0p34r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-iri-string" ,rust-iri-string-0.5)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-json-pointer" ,rust-json-pointer-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-textwrap" ,rust-textwrap-0.16)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/mdboom/jsonschema-valid")
    (synopsis "simple JSON schema validator.")
    (description "This package provides a simple JSON schema validator.")
    (license license:mpl2.0)))

(define-public rust-jsonway-2
  (package
    (name "rust-jsonway")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jsonway" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pprwhjyc0g2l2xgjynv18729704n7w19lj9xyx5z41w8i4vgz7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rustless/jsonway")
    (synopsis "JSON building DSL and configurable serializers for Rust")
    (description
     "This package provides JSON building DSL and configurable serializers for Rust.")
    (license license:expat)))

(define-public rust-kanidm-hsm-crypto-0.2
  (package
    (name "rust-kanidm-hsm-crypto")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanidm-hsm-crypto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pmgbz3mrranlq3fn3ssf5xvsrymawq7lrsc4x7xm8ydhs7fvcqh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-argon2" ,rust-argon2-0.5)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tss-esapi" ,rust-tss-esapi-8)
                       ("rust-tss-esapi-sys" ,rust-tss-esapi-sys-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/kanidm/hsm-crypto/")
    (synopsis "library for easily interacting with a HSM or TPM")
    (description
     "This package provides a library for easily interacting with a HSM or TPM.")
    (license license:mpl2.0)))

(define-public rust-lapack-src-0.8
  (package
    (name "rust-lapack-src")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lapack-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1chjhsbbgc0wpfsnwc2mnq0dml4cvizd6cny7m3qk2dcw4rwp8rj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-accelerate-src" ,rust-accelerate-src-0.3)
                       ("rust-intel-mkl-src" ,rust-intel-mkl-src-0.5)
                       ("rust-netlib-src" ,rust-netlib-src-0.8)
                       ("rust-openblas-src" ,rust-openblas-src-0.10))))
    (home-page "https://github.com/blas-lapack-rs/lapack-src")
    (synopsis "The package provides a LAPACK source of choice")
    (description
     "This package provides The package provides a LAPACK source of choice.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lazy-static-1
  (package
    (name "rust-lazy-static")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy_static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.9))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "macro for declaring lazily evaluated statics in Rust.")
    (description
     "This package provides a macro for declaring lazily evaluated statics in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lber-0.4
  (package
    (name "rust-lber")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lber" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02pghdykbsffimswayixinrsaxfmwwzpb854w5cqzkv4kzyzkxrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-nom" ,rust-nom-7))))
    (home-page "https://github.com/inejge/ldap3")
    (synopsis "An ASN.1/BER parser/encoder based on nom")
    (description
     "This package provides An ASN.1/BER parser/encoder based on nom.")
    (license license:expat)))

(define-public rust-ldap3-client-0.5
  (package
    (name "rust-ldap3-client")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ldap3_client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zv1bd2jrlpy97a5l40wffj2y39vx6fspp2wckz578xxk747y0n6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-ldap3-proto" ,rust-ldap3-proto-0.5)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-with" ,rust-serde-with-3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/kanidm/ldap3/")
    (synopsis "LDAP Client Library for Tokio")
    (description "This package provides LDAP Client Library for Tokio.")
    (license license:mpl2.0)))

(define-public rust-ldap3-proto-0.5
  (package
    (name "rust-ldap3-proto")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ldap3_proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07j1wwm23xhpzwaqcrgggydrr1yd1lqm9b2j1yklsfwxnk0lg879"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-lber" ,rust-lber-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-peg" ,rust-peg-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-openssl" ,rust-openssl-0.10)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-sspi" ,rust-sspi-0.11)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/kanidm/ldap3/")
    (synopsis "LDAP Protocol Bindings for Tokio")
    (description "This package provides LDAP Protocol Bindings for Tokio.")
    (license license:mpl2.0)))

(define-public rust-lexical-sort-0.3
  (package
    (name "rust-lexical-sort")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lexical-sort" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yi0jzlvjaszwl5a49r0a0gcq404rdk5ls2c9npis8qyc68lb7n0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-any-ascii" ,rust-any-ascii-0.1))))
    (home-page "https://lib.rs/crates/lexical-sort")
    (synopsis "Sort Unicode strings lexically")
    (description "This package provides Sort Unicode strings lexically.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.161")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lc5s3zd0491x9zxrv2kvclai1my1spz950pkkyry4vwh318k54f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
     "This package provides Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libflate-2
  (package
    (name "rust-libflate")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07mj9z89vbhq837q58m4v2nblgsmrn6vrp8w1j8g0kpa2kfdzna5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-adler32" ,rust-adler32-1)
                       ("rust-core2" ,rust-core2-0.4)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-dary-heap" ,rust-dary-heap-0.3)
                       ("rust-libflate-lz77" ,rust-libflate-lz77-2))))
    (home-page "https://github.com/sile/libflate")
    (synopsis
     "Rust implementation of DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (description
     "This package provides a Rust implementation of DEFLATE algorithm and related
formats (ZLIB, GZIP).")
    (license license:expat)))

(define-public rust-libflate-lz77-2
  (package
    (name "rust-libflate-lz77")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate_lz77" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gc6h98jwigscasz8vw1vv65b3rismqcbndb8hf6yf4z6qxxgq76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core2" ,rust-core2-0.4)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-rle-decode-fast" ,rust-rle-decode-fast-1))))
    (home-page "https://github.com/sile/libflate")
    (synopsis "LZ77 encoder for libflate crate")
    (description "This package provides LZ77 encoder for libflate crate.")
    (license license:expat)))

(define-public rust-libmimalloc-sys-0.1
  (package
    (name "rust-libmimalloc-sys")
    (version "0.1.39")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libmimalloc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i3b0dzz7cp0ik7ys66q92r16va78gwlbrnxhj5fnkdxsc8niai3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cty" ,rust-cty-0.2)
                       ("rust-libc" ,rust-libc-0.2))))
    (propagated-inputs (list mimalloc))
    (home-page
     "https://github.com/purpleprotocol/mimalloc_rust/tree/master/libmimalloc-sys")
    (synopsis "Sys crate wrapping the mimalloc allocator")
    (description
     "This package provides Sys crate wrapping the mimalloc allocator.")
    (license license:expat)))

(define-public rust-libnss-0.8
  (package
    (name "rust-libnss")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libnss" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06q2q776drhy61zifwz1kgxkprc00yw7kvg9arydd1y73wlw0jrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/csnewman/libnss-rs")
    (synopsis "Rust bindings for creating libnss modules")
    (description
     "This package provides Rust bindings for creating libnss modules.")
    (license license:lgpl3)))

(define-public rust-libsqlite3-sys-0.25
  (package
    (name "rust-libsqlite3-sys")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ym7x39ihcf2s0iyd3iqk6i283kgxcrdc7hxig94cybi7p83by19"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.60)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list sqlite))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description
     "This package provides Native bindings to the libsqlite3 library.")
    (license license:expat)))

(define-public rust-libudev-0.2
  (package
    (name "rust-libudev")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libudev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1znwjh088qrar0sckzm8h26sw34p4q4g9m5wwfpcb8a0vwxnsqpa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-libudev-sys" ,rust-libudev-sys-0.1))))
    (home-page "https://github.com/dcuddeback/libudev-rs")
    (synopsis "Rust wrapper for libudev")
    (description "This package provides Rust wrapper for libudev.")
    (license license:expat)))

(define-public rust-libudev-0.3
  (package
    (name "rust-libudev")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libudev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q1my5alvdwyi8i9pc9gn2mcx5rhbsssmz5cjnxzfpd65laj9cvq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-libudev-sys" ,rust-libudev-sys-0.1))))
    (home-page "https://github.com/dcuddeback/libudev-rs")
    (synopsis "Rust wrapper for libudev")
    (description "This package provides Rust wrapper for libudev.")
    (license license:expat)))

(define-public rust-libudev-sys-0.1
  (package
    (name "rust-libudev-sys")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libudev-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list eudev))
    (home-page "https://github.com/dcuddeback/libudev-sys")
    (synopsis "FFI bindings to libudev")
    (description "This package provides FFI bindings to libudev.")
    (license license:expat)))

(define-public rust-libz-rs-sys-0.2
  (package
    (name "rust-libz-rs-sys")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libz-rs-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rqvcv0g9digivmh2pm9x01z2vyzpkma4rr4wwahxv9r436k3hky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zlib-rs" ,rust-zlib-rs-0.2))))
    (home-page "https://github.com/memorysafety/zlib-rs")
    (synopsis "memory-safe zlib implementation written in rust")
    (description
     "This package provides a memory-safe zlib implementation written in rust.")
    (license license:zlib)))

(define-public rust-libz-sys-1
  (package
    (name "rust-libz-sys")
    (version "1.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libz-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bpqmfzvijbrqs29vphnafjz834lpz6pabbsnf85rqppb9pa4pf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis
     "Low-level bindings to the system libz library (also known as zlib)")
    (description
     "This package provides Low-level bindings to the system libz library (also known as zlib).")
    (license (list license:expat license:asl2.0))))

(define-public rust-linux-raw-sys-0.4
  (package
    (name "rust-linux-raw-sys")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "linux-raw-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/sunfishcode/linux-raw-sys")
    (synopsis "Generated bindings for Linux's userspace API")
    (description
     "This package provides Generated bindings for Linux's userspace API.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lockfree-object-pool-0.1
  (package
    (name "rust-lockfree-object-pool")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lockfree-object-pool" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bjm2g1g1avab86r02jb65iyd7hdi35khn1y81z4nba0511fyx4k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/EVaillant/lockfree-object-pool")
    (synopsis
     "thread-safe object pool collection with automatic return and attach/detach semantics.")
    (description
     "This package provides a thread-safe object pool collection with automatic return
and attach/detach semantics.")
    (license license:boost1.0)))

(define-public rust-lodepng-3
  (package
    (name "rust-lodepng")
    (version "3.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lodepng" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lzsl0khrcmdfqimywccmqhrl9ww5brxhpwqii0q3qv8v9yflbbv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-fallible-collections" ,rust-fallible-collections-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rgb" ,rust-rgb-0.8))))
    (home-page "https://lib.rs/crates/lodepng")
    (synopsis
     "Reading and writing PNG files without system dependencies. Pure Rust port of LodePNG")
    (description
     "This package provides Reading and writing PNG files without system dependencies.  Pure Rust port of
@code{LodePNG}.")
    (license license:zlib)))

(define-public rust-lru-0.12
  (package
    (name "rust-lru")
    (version "0.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lru" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.15))
       #:cargo-development-inputs (("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
                                   ("rust-stats-alloc" ,rust-stats-alloc-0.1))))
    (home-page "https://github.com/jeromefroe/lru-rs")
    (synopsis "LRU cache implementation")
    (description "This package provides a LRU cache implementation.")
    (license license:expat)))

(define-public rust-lzma-rs-0.3
  (package
    (name "rust-lzma-rs")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzma-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0phif4pnjrn28zcxgz3a7z86hhx5gdajmkrndfw4vrkahd682zi9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/gendx/lzma-rs")
    (synopsis "codec for LZMA, LZMA2 and XZ written in pure Rust")
    (description
     "This package provides a codec for LZMA, LZMA2 and XZ written in pure Rust.")
    (license license:expat)))

(define-public rust-lzxd-0.2
  (package
    (name "rust-lzxd")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lzxd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0090w6yjgcg5267mmf7n2qllm76widnxa4bdssd440ri31m37rsx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Lonami/lzxd")
    (synopsis
     "Decompression implementation for Microsoft's LZXD compression format.")
    (description
     "This package provides Decompression implementation for Microsoft's LZXD compression format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-malloced-1
  (package
    (name "rust-malloced")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "malloced" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v00ain5cv4p9v7vn4hxnrdx83hcmv3nxbgfcaf50f8bkqpvpzkd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/nvzqz/malloced")
    (synopsis "malloc-ed box pointer type.")
    (description "This package provides a malloc-ed box pointer type.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mathru-0.13
  (package
    (name "rust-mathru")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mathru" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v4v97vf54cjf4iwlnf7i92wal7b0rb8slngh7anm6jcis9vyhls"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-blas" ,rust-blas-0.22)
                       ("rust-blas-src" ,rust-blas-src-0.8)
                       ("rust-blas-sys" ,rust-blas-sys-0.7)
                       ("rust-lapack" ,rust-lapack-0.19)
                       ("rust-lapack-src" ,rust-lapack-src-0.8)
                       ("rust-lapack-sys" ,rust-lapack-sys-0.14)
                       ("rust-mint" ,rust-mint-0.5)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-plotters" ,rust-plotters-0.3)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://rustmath.gitlab.io/mathru")
    (synopsis "Fundamental algorithms for scientific computing in Rust")
    (description
     "This package provides Fundamental algorithms for scientific computing in Rust.")
    (license license:expat)))

(define-public rust-md4-0.10
  (package
    (name "rust-md4")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "md4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nixkyx1zsn8jkvhzgwqlh8z3yvlw4jr5539pzxfbp1l6lvar9bx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD4 hash function")
    (description "This package provides MD4 hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis
     "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for
1, 2 or 3 byte search and single substring search.")
    (description
     "This package provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32)
routines for 1, 2 or 3 byte search and single substring search.")
    (license (list license:unlicense license:expat))))

(define-public rust-memmap2-0.9
  (package
    (name "rust-memmap2")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmap2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08hkmvri44j6h14lyq4yw5ipsp91a9jacgiww4bs9jm8whi18xgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/RazrFalcon/memmap2-rs")
    (synopsis "Cross-platform Rust API for memory-mapped file IO")
    (description
     "This package provides Cross-platform Rust API for memory-mapped file IO.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mimalloc-0.1
  (package
    (name "rust-mimalloc")
    (version "0.1.43")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mimalloc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0csnyrxc16i592gm5ffham07jyj2w98qsh9jyy1rv59lmr8474b8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libmimalloc-sys" ,rust-libmimalloc-sys-0.1))))
    (home-page "https://github.com/purpleprotocol/mimalloc_rust")
    (synopsis "Performance and security oriented drop-in allocator")
    (description
     "This package provides Performance and security oriented drop-in allocator.")
    (license license:expat)))

(define-public rust-mintex-0.1
  (package
    (name "rust-mintex")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mintex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01ydy8pvyy96cjvjh4hgfqmjalr6hnbyc6c8a9xwq4yvznc4bv4v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/garypen/mintex")
    (synopsis "minimal mutex")
    (description "This package provides minimal mutex.")
    (license license:asl2.0)))

(define-public rust-mio-0.8
  (package
    (name "rust-mio")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking I/O")
    (description "This package provides Lightweight non-blocking I/O.")
    (license license:expat)))

(define-public rust-mio-1
  (package
    (name "rust-mio")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r5g65s5acsx440m0a3pylclbrd0dqz93hg15k9crpllsdbf8sa5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hermit-abi" ,rust-hermit-abi-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking I/O")
    (description "This package provides Lightweight non-blocking I/O.")
    (license license:expat)))

(define-public rust-mio-aio-0.9
  (package
    (name "rust-mio-aio")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mio-aio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zrp159m5nc1j23awbrn9rmx7c5lh47z9szg65s3zj4h95b0v5wl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-mio" ,rust-mio-0.8)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://github.com/asomers/mio-aio")
    (synopsis "POSIX AIO bindings for mio")
    (description "This package provides POSIX AIO bindings for mio.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mockito-0.31
  (package
    (name "rust-mockito")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockito" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1scdndv8z0y1fxwlqdp53366vdsjmyyg86gy751vfynrkg7gxyc0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-assert-json-diff" ,rust-assert-json-diff-2)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-similar" ,rust-similar-2))))
    (home-page "https://github.com/lipanski/mockito")
    (synopsis "HTTP mocking for Rust")
    (description "This package provides HTTP mocking for Rust.")
    (license license:expat)))

(define-public rust-netlib-src-0.8
  (package
    (name "rust-netlib-src")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "netlib-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04l2ggdaq0bjc64prsw2f8ddxn84m1rmpnkjb9nr0ijdpcv1zx1r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cmake" ,rust-cmake-0.1))))
    (home-page "https://github.com/blas-lapack-rs/netlib-src")
    (synopsis "The package provides a source of BLAS and LAPACK via Netlib")
    (description
     "This package provides The package provides a source of BLAS and LAPACK via Netlib.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nix-0.29
  (package
    (name "rust-nix")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "This package provides Rust friendly bindings to *nix APIs.")
    (license license:expat)))

(define-public rust-nonempty-0.8
  (package
    (name "rust-nonempty")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nonempty" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jfkadljx2rs7bhh22w5scsgg38i2zq044hrkilkxs9x83blmbxf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/cloudhead/nonempty")
    (synopsis "Correct by construction non-empty vector")
    (description
     "This package provides Correct by construction non-empty vector.")
    (license license:expat)))

(define-public rust-notify-debouncer-full-0.1
  (package
    (name "rust-notify-debouncer-full")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "notify-debouncer-full" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06a2wsi514dhrq8q5ghsvkgwj7n0pliid5plipxpdrwvnhg2r0gl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-file-id" ,rust-file-id-0.1)
                       ("rust-notify" ,rust-notify-6)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-deser-hjson" ,rust-deser-hjson-1)
                                   ("rust-mock-instant" ,rust-mock-instant-0.3)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rstest" ,rust-rstest-0.17)
                                   ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "notify event debouncer optimized for ease of use")
    (description
     "This package provides notify event debouncer optimized for ease of use.")
    (license (list license:cc0 license:artistic2.0))))

(define-public rust-num-bigint-0.4
  (package
    (name "rust-num-bigint")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-bigint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-num/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description "This package provides Big integer implementation for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oauth2-4
  (package
    (name "rust-oauth2")
    (version "4.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oauth2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwkmwxwygl4fwghgyanixzqgn7yvkwwwacdghz7x124v36l3263"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-hmac" ,rust-hmac-0.12)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/ramosbugs/oauth2-rs")
    (synopsis "An extensible, strongly-typed implementation of OAuth2")
    (description
     "This package provides An extensible, strongly-typed implementation of OAuth2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-object-0.35
  (package
    (name "rust-object")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pnv84mx3f3p847hfnsp4znivnwkc1x53maq459a92w42fw7mv5q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-ruzstd" ,rust-ruzstd-0.6)
                       ("rust-wasmparser" ,rust-wasmparser-0.202))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis "unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-object-0.36
  (package
    (name "rust-object")
    (version "0.36.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nggchwvjgilrxarwcmfcisa9fq1phg382y672aa6lb86ir4kdi7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-ruzstd" ,rust-ruzstd-0.7)
                       ("rust-wasmparser" ,rust-wasmparser-0.215))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis "unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-odds-0.4
  (package
    (name "rust-odds")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "odds" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17jd5fjcrlya7dbnnj0v8s83l3jhlajyljmkcy49pxsvxj9zdsdz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rawpointer" ,rust-rawpointer-0.2)
                       ("rust-rawslice" ,rust-rawslice-0.1)
                       ("rust-unchecked-index" ,rust-unchecked-index-0.2))))
    (home-page "https://github.com/bluss/odds")
    (synopsis
     "Odds and ends â collection miscellania. Extra functionality for slices (`.find()`, `RevSlice`), strings and other things. Things in odds may move to more appropriate crates if we find them.")
    (description
     "This package provides Odds and ends â collection miscellania.  Extra functionality for slices
(`.find()`, `@code{RevSlice`}), strings and other things.  Things in odds may
move to more appropriate crates if we find them.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oid-0.2
  (package
    (name "rust-oid")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hh61lx2kr0ca2rvkhf5j94asxxvb6pfwfxm06hdn4w8b4y906cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://labs.unnecessary.engineering/oid")
    (synopsis
     "Rust-native library for building, parsing, and formating Object Identifiers (OIDs)")
    (description
     "This package provides Rust-native library for building, parsing, and formating Object Identifiers
(OIDs).")
    (license (list license:expat license:asl2.0))))

(define-public rust-openblas-build-0.10
  (package
    (name "rust-openblas-build")
    (version "0.10.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openblas-build" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sg1s6q1r1x5jiyhm0l5xb96snqk4wsarkqm373sz309jm0b9dnl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/blas-lapack-rs/openblas-src")
    (synopsis "The package provides a build helper for OpenBLAS")
    (description
     "This package provides The package provides a build helper for @code{OpenBLAS}.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-openblas-src-0.10
  (package
    (name "rust-openblas-src")
    (version "0.10.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openblas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qvkadgr05kk27x58rlvhrj6g0zjrm9ahm22vc9i0sknkxj5hjda"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs" ,rust-dirs-3)
                       ("rust-openblas-build" ,rust-openblas-build-0.10)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/blas-lapack-rs/openblas-src")
    (synopsis "The package provides a source of BLAS and LAPACK via OpenBLAS")
    (description
     "This package provides The package provides a source of BLAS and LAPACK via @code{OpenBLAS}.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-openssl-0.10
  (package
    (name "rust-openssl")
    (version "0.10.68")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xbiz2bmba2fibg70s462yk2fndp3f9vz11c7iw0ilh2y54bqx31"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-openssl-macros" ,rust-openssl-macros-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9))
       #:cargo-development-inputs (("rust-hex" ,rust-hex-0.4))))
    (home-page "https://github.com/sfackler/rust-openssl")
    (synopsis "OpenSSL bindings")
    (description "This package provides @code{OpenSSL} bindings.")
    (license license:asl2.0)))

(define-public rust-openssl-kdf-0.4
  (package
    (name "rust-openssl-kdf")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-kdf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x61pfsji1phgf0z8wpbwnkjqmqgyxk03c1xakccc4fmvrs4pnpy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/puiterwijk/rust-openssl-kdf/")
    (synopsis "OpenSSL KDF function abstraction")
    (description
     "This package provides @code{OpenSSL} KDF function abstraction.")
    (license license:expat)))

(define-public rust-openssl-macros-0.1
  (package
    (name "rust-openssl-macros")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "")
    (synopsis "Internal macros used by the openssl crate")
    (description
     "This package provides Internal macros used by the openssl crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-openssl-src-300
  (package
    (name "rust-openssl-src")
    (version "300.3.1+3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14cbc0i3ly3msl7bkhj3rrnlv4g1m0qbswxxzcvz26x888yranbj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/alexcrichton/openssl-src-rs")
    (synopsis "Source of OpenSSL and logic to build it.")
    (description
     "This package provides Source of @code{OpenSSL} and logic to build it.")
    (license (list license:expat license:asl2.0))))

(define-public rust-openssl-sys-0.9
  (package
    (name "rust-openssl-sys")
    (version "0.9.104")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hf712xcxmycnlc09r8d446b3mwqchsbfrjv374fp7grrc3g7as5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-bssl-sys" ,rust-bssl-sys-0.1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-openssl-src" ,rust-openssl-src-300)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/sfackler/rust-openssl")
    (synopsis "FFI bindings to OpenSSL")
    (description "This package provides FFI bindings to @code{OpenSSL}.")
    (license license:expat)))

(define-public rust-opentelemetry-0.20
  (package
    (name "rust-opentelemetry")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m2cg0kqv8hplm3w6aajjm4yl05k19a5k9bidzmjyv8fphvxk4cm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20))
       #:cargo-development-inputs (("rust-opentelemetry-stdout" ,rust-opentelemetry-stdout-0.1))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis "OpenTelemetry API for Rust")
    (description "This package provides @code{OpenTelemetry} API for Rust.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-api-0.20
  (package
    (name "rust-opentelemetry-api")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry_api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16sv4rdm417v3d3mkk9vgksx7fvlk2qqpnm3dhhb3c9x68jzg0ca"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-urlencoding" ,rust-urlencoding-2))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis
     "OpenTelemetry is a metrics collection and distributed tracing framework")
    (description
     "This package provides @code{OpenTelemetry} is a metrics collection and distributed tracing framework.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-http-0.9
  (package
    (name "rust-opentelemetry-http")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12shasrr0zg63gh8050wm2xlw1ppkb2a8c1my3x373hxw704wnf7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-isahc" ,rust-isahc-1)
                       ("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis
     "Helper implementations for sending HTTP requests. Uses include propagating and extracting context over http, exporting telemetry, requesting sampling strategies")
    (description
     "This package provides Helper implementations for sending HTTP requests.  Uses include propagating and
extracting context over http, exporting telemetry, requesting sampling
strategies.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-jaeger-0.19
  (package
    (name "rust-opentelemetry-jaeger")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-jaeger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05dmq7jvkwi8ri8hmfv7i24j5f5vggglvw7w2gwr1ww4j2x5hsc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.5)
                       ("rust-isahc" ,rust-isahc-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.20)
                       ("rust-opentelemetry-http" ,rust-opentelemetry-http-0.9)
                       ("rust-opentelemetry-semantic-conventions" ,rust-opentelemetry-semantic-conventions-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-thrift" ,rust-thrift-0.17)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tonic" ,rust-tonic-0.9)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-jaeger")
    (synopsis "Jaeger exporter for OpenTelemetry")
    (description
     "This package provides Jaeger exporter for @code{OpenTelemetry}.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-otlp-0.13
  (package
    (name "rust-opentelemetry-otlp")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-otlp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x8jghmb6qd9im2ahs5z1swgkmxlks9vxbzsyfcl11im85f5lpky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-grpcio" ,rust-grpcio-0.12)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-opentelemetry-http" ,rust-opentelemetry-http-0.9)
                       ("rust-opentelemetry-proto" ,rust-opentelemetry-proto-0.3)
                       ("rust-opentelemetry-semantic-conventions" ,rust-opentelemetry-semantic-conventions-0.12)
                       ("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tonic" ,rust-tonic-0.9))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-time" ,rust-time-0.3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-otlp")
    (synopsis "Exporter for the OpenTelemetry Collector")
    (description
     "This package provides Exporter for the @code{OpenTelemetry} Collector.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-proto-0.3
  (package
    (name "rust-opentelemetry-proto")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sybqkq48gifj6cic3n1jxx5gcnkmwkf1gg41rfr134zm8agiqxi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-grpcio" ,rust-grpcio-0.12)
                       ("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tonic" ,rust-tonic-0.9))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-proto")
    (synopsis "Protobuf generated files and transformations")
    (description
     "This package provides Protobuf generated files and transformations.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-sdk-0.20
  (package
    (name "rust-opentelemetry-sdk")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry_sdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09l0vl76yv61pp93vr2kf4khc3x9sjhapjwzg4wq3m0j0rd713ps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-opentelemetry-http" ,rust-opentelemetry-http-0.9)
                       ("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-ordered-float" ,rust-ordered-float-3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/open-telemetry/opentelemetry-rust")
    (synopsis
     "The SDK for the OpenTelemetry metrics collection and distributed tracing framework")
    (description
     "This package provides The SDK for the @code{OpenTelemetry} metrics collection and distributed tracing
framework.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-semantic-conventions-0.12
  (package
    (name "rust-opentelemetry-semantic-conventions")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-semantic-conventions" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0scjg1lyrlykvqc8bgzm8dqrxv89kr7b5wg70240cdfi18sgkjbk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opentelemetry" ,rust-opentelemetry-0.20))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-semantic-conventions")
    (synopsis "Semantic conventions for OpenTelemetry")
    (description
     "This package provides Semantic conventions for @code{OpenTelemetry}.")
    (license license:asl2.0)))

(define-public rust-opentelemetry-stdout-0.1
  (package
    (name "rust-opentelemetry-stdout")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "opentelemetry-stdout" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1imhbnh2lffjrz5s4vbpwaryb7bqc99cgrdzvkvd7yf03cr51mcb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-opentelemetry-api" ,rust-opentelemetry-api-0.20)
                       ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20)
                       ("rust-ordered-float" ,rust-ordered-float-3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-rust/tree/main/opentelemetry-stdout")
    (synopsis "An OpenTelemetry exporter for stdout")
    (description
     "This package provides An @code{OpenTelemetry} exporter for stdout.")
    (license license:asl2.0)))

(define-public rust-outref-0.5
  (package
    (name "rust-outref")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "outref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ynw7nb89603gkfi83f9chsf76ds3b710gxfn12yyawrzl7pcc20"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Nugine/outref")
    (synopsis "Out reference")
    (description "This package provides Out reference.")
    (license license:expat)))

(define-public rust-p521-0.13
  (package
    (name "rust-p521")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "p521" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cl5y2aypa1vxg181a0na3abndz1981pfdp2zkyml88z3wbf5j8g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.2)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-hex-literal" ,rust-hex-literal-0.4)
                       ("rust-primeorder" ,rust-primeorder-0.13)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serdect" ,rust-serdect-0.2)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p521")
    (synopsis
     "Pure Rust implementation of the NIST P-521 (a.k.a. secp521r1) elliptic curve
as defined in SP 800-186")
    (description
     "This package provides Pure Rust implementation of the NIST P-521 (a.k.a.  secp521r1) elliptic curve as
defined in SP 800-186.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-paste-1
  (package
    (name "rust-paste")
    (version "1.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "paste" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-paste-test-suite" ,rust-paste-test-suite-0.0.0)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis "Macros for all your token pasting needs")
    (description
     "This package provides Macros for all your token pasting needs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pcsc-2
  (package
    (name "rust-pcsc")
    (version "2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcsc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "027a2s8lp6w025aa758s84qszcwkyg92s1mhvplrqzbbh5zrvva5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-pcsc-sys" ,rust-pcsc-sys-1))))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Bindings to the PC/SC API for smart card communication")
    (description
     "This package provides Bindings to the PC/SC API for smart card communication.")
    (license license:expat)))

(define-public rust-pcsc-sys-1
  (package
    (name "rust-pcsc-sys")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcsc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00vlrfv3kcr49ajbzzr1b4ls7g28f97mj9vdjdzick9c1yl9p7mh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list pcsc-lite))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Low-level bindings to the PC/SC C API")
    (description
     "This package provides Low-level bindings to the PC/SC C API.")
    (license license:expat)))

(define-public rust-peg-0.8
  (package
    (name "rust-peg")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "peg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13qfwkmlmm3wbwzsrc3bkb16m1xjd5w7aah5cvpx2ipk5nq86li9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-peg-macros" ,rust-peg-macros-0.8)
                       ("rust-peg-runtime" ,rust-peg-runtime-0.8))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/kevinmehall/rust-peg")
    (synopsis "simple Parsing Expression Grammar (PEG) parser generator.")
    (description
     "This package provides a simple Parsing Expression Grammar (PEG) parser
generator.")
    (license license:expat)))

(define-public rust-peg-macros-0.8
  (package
    (name "rust-peg-macros")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "peg-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09nlm6lw5zdpv9wnfr08836snsjnypaibr1cb1cs05pikhfnmbdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-peg-runtime" ,rust-peg-runtime-0.8)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/kevinmehall/rust-peg")
    (synopsis
     "Procedural macros for rust-peg. To use rust-peg, see the `peg` crate")
    (description
     "This package provides Procedural macros for rust-peg.  To use rust-peg, see the `peg` crate.")
    (license license:expat)))

(define-public rust-peg-runtime-0.8
  (package
    (name "rust-peg-runtime")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "peg-runtime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fmasxbncm503dcakq8qwkcbjihz4jklkrjy0v1190q79ksvibp3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/kevinmehall/rust-peg")
    (synopsis
     "Runtime support for rust-peg grammars. To use rust-peg, see the `peg` crate")
    (description
     "This package provides Runtime support for rust-peg grammars.  To use rust-peg, see the `peg` crate.")
    (license license:expat)))

(define-public rust-petgraph-0.6
  (package
    (name "rust-petgraph")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "petgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fixedbitset" ,rust-fixedbitset-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-quickcheck" ,rust-quickcheck-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-ahash" ,rust-ahash-0.7)
                                   ("rust-bincode" ,rust-bincode-1)
                                   ("rust-defmac" ,rust-defmac-0.2)
                                   ("rust-fxhash" ,rust-fxhash-0.2)
                                   ("rust-itertools" ,rust-itertools-0.12)
                                   ("rust-odds" ,rust-odds-0.4)
                                   ("rust-rand" ,rust-rand-0.5))))
    (home-page "https://github.com/petgraph/petgraph")
    (synopsis
     "Graph data structure library. Provides graph types and graph algorithms")
    (description
     "This package provides Graph data structure library.  Provides graph types and graph algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-7
  (package
    (name "rust-picky")
    (version "7.0.0-rc.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ppfgwrshws2ds6rgssk16nw4gcsc71p1wkf5q6894zl6234i6zy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-aes-gcm" ,rust-aes-gcm-0.10)
                       ("rust-aes-kw" ,rust-aes-kw-0.2)
                       ("rust-argon2" ,rust-argon2-0.5)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bcrypt-pbkdf" ,rust-bcrypt-pbkdf-0.10)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cab" ,rust-cab-0.6)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-des" ,rust-des-0.8)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http" ,rust-http-1)
                       ("rust-lexical-sort" ,rust-lexical-sort-0.3)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-p521" ,rust-p521-0.13)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.9)
                       ("rust-picky-asn1-der" ,rust-picky-asn1-der-0.5)
                       ("rust-picky-asn1-x509" ,rust-picky-asn1-x509-0.13)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rc2" ,rust-rc2-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sha3" ,rust-sha3-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-x25519-dalek" ,rust-x25519-dalek-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Portable X.509, PKI, JOSE and HTTP signature implementation")
    (description
     "This package provides Portable X.509, PKI, JOSE and HTTP signature implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-0.8
  (package
    (name "rust-picky-asn1")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04mjb35z1g3dny8hjj6893fh8g6pskyhifcq58gf4sy16c7ylpi9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Provide ASN.1 simple types")
    (description "This package provides Provide ASN.1 simple types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-0.9
  (package
    (name "rust-picky-asn1")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dmyv62g0xmml8k0lba2wcig26kzcykb77fdi078q6xi72r1j01n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Provide ASN.1 simple types")
    (description "This package provides Provide ASN.1 simple types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-der-0.4
  (package
    (name "rust-picky-asn1-der")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1-der" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gvrhb2nx17cnigsvbvjq69xg0zy27iabglknfnjvm1nkqx8gxsx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "An ASN.1-DER subset for serde")
    (description "This package provides An ASN.1-DER subset for serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-der-0.5
  (package
    (name "rust-picky-asn1-der")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1-der" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qxj7np1pyj1w886wjnrpvs6xp0m94vb6s6f6bwaidapw63ljy04"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "An ASN.1-DER subset for serde")
    (description "This package provides An ASN.1-DER subset for serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-x509-0.12
  (package
    (name "rust-picky-asn1-x509")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1-x509" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0232vn4i6x2w1hzysn983an6x0fqzak1ix0h4grryjb83bvj0prc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.8)
                       ("rust-picky-asn1-der" ,rust-picky-asn1-der-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-widestring" ,rust-widestring-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Provides ASN1 types defined by X.509 related RFCs")
    (description
     "This package provides ASN1 types defined by X.509 related RFCs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-asn1-x509-0.13
  (package
    (name "rust-picky-asn1-x509")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-asn1-x509" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x5d5wsr13krk3d3h89vimrs8d7n6mwyfswi692s1i5id3vlyf7k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.9)
                       ("rust-picky-asn1-der" ,rust-picky-asn1-der-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-widestring" ,rust-widestring-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Provides ASN1 types defined by X.509 related RFCs")
    (description
     "This package provides ASN1 types defined by X.509 related RFCs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-picky-krb-0.8
  (package
    (name "rust-picky-krb")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "picky-krb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s6j2lypsnliggbxmc0pslgkmhjj3pbgjcdcpj0plrbfpqggc77p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cbc" ,rust-cbc-0.1)
                       ("rust-crypto" ,rust-crypto-0.5)
                       ("rust-des" ,rust-des-0.8)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.8)
                       ("rust-picky-asn1-der" ,rust-picky-asn1-der-0.4)
                       ("rust-picky-asn1-x509" ,rust-picky-asn1-x509-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/Devolutions/picky-rs")
    (synopsis "Encode/decode Kerberos ASN.1 DER structs")
    (description
     "This package provides Encode/decode Kerberos ASN.1 DER structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pinned-0.1
  (package
    (name "rust-pinned")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pinned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nsrxs49dhjjz1gvg0pvac2rcidnwwd8l99y7vhwym2yv5xh4ad8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/futursolo/pinned")
    (synopsis "Synchronisation primitives for !Send tasks")
    (description
     "This package provides Synchronisation primitives for !Send tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pkg-config-0.3
  (package
    (name "rust-pkg-config")
    (version "0.3.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkg-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis
     "library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.")
    (description
     "This package provides a library to run the pkg-config system tool at build time
in order to be used in Cargo build scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pnet-base-0.35
  (package
    (name "rust-pnet-base")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xxj1ym32zqmy7m7ciiisv513rk9qis3p6x4mgrnmwbx0va91hgz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-no-std-net" ,rust-no-std-net-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Fundamental base types and code used by pnet")
    (description
     "This package provides Fundamental base types and code used by pnet.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pnet-datalink-0.35
  (package
    (name "rust-pnet-datalink")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_datalink" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dx7a9j2n7r463w8dv0wn1vasqnkhrajs79f6cm10qz11gn717p7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ipnetwork" ,rust-ipnetwork-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-netmap-sys" ,rust-netmap-sys-0.1)
                       ("rust-pcap" ,rust-pcap-1)
                       ("rust-pnet-base" ,rust-pnet-base-0.35)
                       ("rust-pnet-sys" ,rust-pnet-sys-0.35)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Cross-platform, datalink layer networking")
    (description
     "This package provides Cross-platform, datalink layer networking.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pnet-sys-0.35
  (package
    (name "rust-pnet-sys")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jqgl34w5jckvby74nh89hjc94m8m6pz7hjh21s0hsyvsk9l6ikx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Access to network related system function and calls")
    (description
     "This package provides Access to network related system function and calls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-portable-atomic-util-0.2
  (package
    (name "rust-portable-atomic-util")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "portable-atomic-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kbh4ik8cqr4bdbpyfh8n4xcwip93yciccvv815darif0wh89pgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-portable-atomic" ,rust-portable-atomic-1))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis "Synchronization primitives built with portable-atomic.")
    (description
     "This package provides Synchronization primitives built with portable-atomic.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-portpicker-0.1
  (package
    (name "rust-portpicker")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "portpicker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1acvi1m6g7d3j8xvdsbn0b7yqyfy7yr7fm1pw5kbdyhvmxpxg5xy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/Dentosal/portpicker-rs")
    (synopsis "Pick a free unused port")
    (description "This package provides Pick a free unused port.")
    (license license:unlicense)))

(define-public rust-postgres-derive-0.4
  (package
    (name "rust-postgres-derive")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "postgres-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18l065qawh5lm1d350s32mg6d9rzhj6878d9h7whw18vfjx5w543"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "An internal crate used by postgres-types")
    (description
     "This package provides An internal crate used by postgres-types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-postgres-protocol-0.6
  (package
    (name "rust-postgres-protocol")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "postgres-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08sbm36w68vyrzaaxiv8srrr1w65wi8ycdgf9flfz2y2xfyhxnmc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-stringprep" ,rust-stringprep-0.1))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Low level Postgres protocol APIs")
    (description "This package provides Low level Postgres protocol APIs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-postgres-types-0.2
  (package
    (name "rust-postgres-types")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "postgres-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sgfk1j8avc9a63n5qkqjx0msjnq2lxbixxv2cscrcrg0fg8s102"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-array-init" ,rust-array-init-2)
                       ("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-cidr" ,rust-cidr-0.2)
                       ("rust-eui48" ,rust-eui48-0.4)
                       ("rust-eui48" ,rust-eui48-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-geo-types" ,rust-geo-types-0.7)
                       ("rust-geo-types" ,rust-geo-types-0.6)
                       ("rust-postgres-derive" ,rust-postgres-derive-0.4)
                       ("rust-postgres-protocol" ,rust-postgres-protocol-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smol-str" ,rust-smol-str-0.1)
                       ("rust-time" ,rust-time-0.2)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/sfackler/rust-postgres")
    (synopsis "Conversions between Rust and Postgres values")
    (description
     "This package provides Conversions between Rust and Postgres values.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pprof-0.11
  (package
    (name "rust-pprof")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pprof" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n8hl54qxx261x07jjpjxfvl3p4cy74iqqx4k446jdg59dfysv8r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-criterion" ,rust-criterion-0.4)
                       ("rust-findshlibs" ,rust-findshlibs-0.10)
                       ("rust-inferno" ,rust-inferno-0.11)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-prost-build" ,rust-prost-build-0.11)
                       ("rust-prost-derive" ,rust-prost-derive-0.11)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-symbolic-demangle" ,rust-symbolic-demangle-10)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/tikv/pprof-rs")
    (synopsis "An internal perf tools for rust programs")
    (description
     "This package provides An internal perf tools for rust programs.")
    (license license:asl2.0)))

(define-public rust-pprof-0.12
  (package
    (name "rust-pprof")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pprof" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sm4ih3wqjld33asb685biqx1kqs9jlahb05klc6k4mgkpaqb0wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-criterion" ,rust-criterion-0.5)
                       ("rust-findshlibs" ,rust-findshlibs-0.10)
                       ("rust-inferno" ,rust-inferno-0.11)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-prost-build" ,rust-prost-build-0.11)
                       ("rust-prost-derive" ,rust-prost-derive-0.11)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-symbolic-demangle" ,rust-symbolic-demangle-12)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/tikv/pprof-rs")
    (synopsis "An internal perf tools for rust programs")
    (description
     "This package provides An internal perf tools for rust programs.")
    (license license:asl2.0)))

(define-public rust-prctl-1
  (package
    (name "rust-prctl")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prctl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lkgnid3sjfbqf3sbcgyihlw80a6n9l6m0n23b7f5pm927qk96h5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.14))))
    (home-page "https://github.com/viraptor/prctl-rs")
    (synopsis
     "This package provides safe abstraction to the linux prctl() interface. Some
functions may be architecture-specific.")
    (description
     "This package provides safe abstraction to the linux prctl() interface.  Some
functions may be architecture-specific.")
    (license license:expat)))

(define-public rust-primeorder-0.13
  (package
    (name "rust-primeorder")
    (version "0.13.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "primeorder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-serdect" ,rust-serdect-0.2))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/primeorder")
    (synopsis
     "Pure Rust implementation of complete addition formulas for prime order elliptic
curves (Renes-Costello-Batina 2015). Generic over field elements and curve
equation coefficients")
    (description
     "This package provides Pure Rust implementation of complete addition formulas for prime order elliptic
curves (Renes-Costello-Batina 2015).  Generic over field elements and curve
equation coefficients.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.89")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vlq56v41dsj69pnk7lil7fxvbfid50jnzdn3xnr31g05mkb0fgi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-quote" ,rust-quote-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-tar" ,rust-tar-0.4))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-prokio-0.1
  (package
    (name "rust-prokio")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "127l9k5076xwlaf0b64hw3l14wqjss2krldb2ddgm4apdq85xd83"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo" ,rust-gloo-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pinned" ,rust-pinned-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4))))
    (home-page "https://github.com/futursolo/prokio")
    (synopsis
     "An asynchronous runtime compatible with WebAssembly and non-WebAssembly targets")
    (description
     "This package provides An asynchronous runtime compatible with @code{WebAssembly} and
non-@code{WebAssembly} targets.")
    (license (list license:expat license:asl2.0))))

(define-public rust-prost-build-0.11
  (package
    (name "rust-prost-build")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-build" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w5jx97q96ydhkg67wx3lb11kfy8195c56g0476glzws5iak758i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-prettyplease" ,rust-prettyplease-0.1)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.9)
                       ("rust-pulldown-cmark-to-cmark" ,rust-pulldown-cmark-to-cmark-10)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis
     "Generate Prost annotated Rust types from Protocol Buffers files")
    (description
     "This package provides Generate Prost annotated Rust types from Protocol Buffers files.")
    (license license:asl2.0)))

(define-public rust-prost-types-0.11
  (package
    (name "rust-prost-types")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ryk38sqkp2nf4dgdqdfbgn6zwwvjraw6hqq6d9a6088shj4di1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-prost" ,rust-prost-0.11))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "Prost definitions of Protocol Buffers well known types")
    (description
     "This package provides Prost definitions of Protocol Buffers well known types.")
    (license license:asl2.0)))

(define-public rust-qrcode-0.12
  (package
    (name "rust-qrcode")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "qrcode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zzmrwb44r17zn0hkpin0yldwxjdwya2nkvv23jwcc1nbx2z3lhn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-checked-int-cast" ,rust-checked-int-cast-1)
                       ("rust-image" ,rust-image-0.23))
       #:cargo-development-inputs (("rust-image" ,rust-image-0.23))))
    (home-page "https://github.com/kennytm/qrcode-rust")
    (synopsis "QR code encoder in Rust")
    (description "This package provides QR code encoder in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quinn-0.11
  (package
    (name "rust-quinn")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quinn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mi1gxhy483f9ray0i9b2g41681gq750h85w69qrljq3d5rqwbdj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-2)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-quinn-proto" ,rust-quinn-proto-0.11)
                       ("rust-quinn-udp" ,rust-quinn-udp-0.5)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-smol" ,rust-smol-2)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/quinn-rs/quinn")
    (synopsis "Versatile QUIC transport protocol implementation")
    (description
     "This package provides Versatile QUIC transport protocol implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quinn-proto-0.11
  (package
    (name "rust-quinn-proto")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quinn-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1paz642iqzmah1f673pwvl3azp7avwsah31cb2i6rl3sxhwzp4ms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-platform-verifier" ,rust-rustls-platform-verifier-0.3)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/quinn-rs/quinn")
    (synopsis "State machine for the QUIC transport protocol")
    (description
     "This package provides State machine for the QUIC transport protocol.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quinn-udp-0.5
  (package
    (name "rust-quinn-udp")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quinn-udp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1182ifvapi7f3b4fc530z0q8myi9aal88l2kahbnyg5p0lvfrzwb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/quinn-rs/quinn")
    (synopsis
     "UDP sockets with ECN information for the QUIC transport protocol")
    (description
     "This package provides UDP sockets with ECN information for the QUIC transport protocol.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))
       #:cargo-development-inputs (("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "This package provides Quasi-quoting macro quote!(...).")
    (license (list license:expat license:asl2.0))))

(define-public rust-rc2-0.8
  (package
    (name "rust-rc2")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pf17xj083bppby905ciwdh8wvrr7yli0l75m95bhf4lism4vik2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis "RC2 block cipher")
    (description "This package provides RC2 block cipher.")
    (license (list license:expat license:asl2.0))))

(define-public rust-referencing-0.21
  (package
    (name "rust-referencing")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "referencing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ad07vsq06qia1f05h8wml249bjyzs2n0q4dz542zlzpkdzxhnpw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-fluent-uri" ,rust-fluent-uri-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/Stranger6667/jsonschema")
    (synopsis
     "An implementation-agnostic JSON reference resolution library for Rust")
    (description
     "This package provides An implementation-agnostic JSON reference resolution library for Rust.")
    (license license:expat)))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
     "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (description
     "This package provides An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-automata-0.4
  (package
    (name "rust-regex-automata")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-automata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18wd530ndrmygi6xnz3sp345qi0hy2kdbsa89182nwbl6br5i1rn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex/tree/master/regex-automata")
    (synopsis "Automata construction and matching using regular expressions")
    (description
     "This package provides Automata construction and matching using regular expressions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-syntax-0.8
  (package
    (name "rust-regex-syntax")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-syntax" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1))))
    (home-page "https://github.com/rust-lang/regex/tree/master/regex-syntax")
    (synopsis "regular expression parser.")
    (description "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-reqwest-0.12
  (package
    (name "rust-reqwest")
    (version "0.12.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vq40h75fmrkfjyyjxl84g0pzjzz0n989ag1cajy17g78spn4z57"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.18)
                       ("rust-cookie-store" ,rust-cookie-store-0.21)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-h3" ,rust-h3-0.0.6)
                       ("rust-h3-quinn" ,rust-h3-quinn-0.0.7)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.27)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.6)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-quinn" ,rust-quinn-0.11)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.8)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-1)
                       ("rust-system-configuration" ,rust-system-configuration-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tokio-socks" ,rust-tokio-socks-0.5)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-streams" ,rust-wasm-streams-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26)
                       ("rust-windows-registry" ,rust-windows-registry-0.2))
       #:cargo-development-inputs (("rust-brotli" ,rust-brotli-6)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-hyper-util" ,rust-hyper-util-0.1)
                                   ("rust-libflate" ,rust-libflate-2)
                                   ("rust-rustls" ,rust-rustls-0.23)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://github.com/seanmonstar/reqwest")
    (synopsis "higher level HTTP client library")
    (description "This package provides higher level HTTP client library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rgb-0.8
  (package
    (name "rust-rgb")
    (version "0.8.50")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rgb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-defmt" ,rust-defmt-0.3)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://lib.rs/crates/rgb")
    (synopsis
     "`struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods for color manipulation.
Allows no-copy high-level interoperability. Also adds common convenience methods and implements standard Rust traits to make `RGB`/`RGBA` pixels and slices first-class Rust objects")
    (description
     "This package provides `struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods
for color manipulation.  Allows no-copy high-level interoperability.  Also adds
common convenience methods and implements standard Rust traits to make
`RGB`/`RGBA` pixels and slices first-class Rust objects.")
    (license license:expat)))

(define-public rust-rmp-0.8
  (package
    (name "rust-rmp")
    (version "0.8.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rmp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description
     "This package provides Pure Rust @code{MessagePack} serialization implementation.")
    (license license:expat)))

(define-public rust-rmp-serde-1
  (package
    (name "rust-rmp-serde")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rmp-serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nylmh7w2vpa1bwrnx1jfp2l4yz6i5qrmpic5zll166gfyj9kraj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-rmp" ,rust-rmp-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Serde bindings for RMP")
    (description "This package provides Serde bindings for RMP.")
    (license license:expat)))

(define-public rust-route-recognizer-0.3
  (package
    (name "rust-route-recognizer")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "route-recognizer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ikp3blbina00jdbifxw1c9whg6mljli24lq5pv82iar53xr9axg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustasync/route-recognizer")
    (synopsis
     "Recognizes URL patterns with support for dynamic and wildcard segments")
    (description
     "This package provides Recognizes URL patterns with support for dynamic and wildcard segments.")
    (license license:expat)))

(define-public rust-rpassword-7
  (package
    (name "rust-rpassword")
    (version "7.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rpassword" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-rtoolbox" ,rust-rtoolbox-0.0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications")
    (description
     "This package provides Read passwords in console applications.")
    (license license:asl2.0)))

(define-public rust-runloop-0.1
  (package
    (name "rust-runloop")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "runloop" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ggs14zmkc5zklfm2v5zj7h79bfmv6mbpbw45s4j2y8n0jvb8yax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ttaubert/rust-runloop/")
    (synopsis
     "Cancelable non-blocking polling threads (with optional timeouts)")
    (description
     "This package provides Cancelable non-blocking polling threads (with optional timeouts).")
    (license license:mpl2.0)))

(define-public rust-rusqlite-0.28
  (package
    (name "rust-rusqlite")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aj2jvlcdy1miahy6wsia50ak26q3ziynl8yx0raqffb7sy17qh1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-fallible-streaming-iterator" ,rust-fallible-streaming-iterator-0.1)
                       ("rust-hashlink" ,rust-hashlink-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.25)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-unicase" ,rust-unicase-2)
                                   ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Ergonomic wrapper for SQLite")
    (description "This package provides Ergonomic wrapper for SQLite.")
    (license license:expat)))

(define-public rust-rustc-demangle-0.1
  (package
    (name "rust-rustc-demangle")
    (version "0.1.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc-demangle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description "This package provides Rust compiler symbol demangling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-hash-2
  (package
    (name "rust-rustc-hash")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lni0lf846bzrf3jvci6jaf4142n1mdqxvcpczk5ch9pfgyk8c2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/rustc-hash")
    (synopsis "speedy, non-cryptographic hashing algorithm used by rustc")
    (description
     "This package provides a speedy, non-cryptographic hashing algorithm used by
rustc.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rustix-0.38
  (package
    (name "rust-rustix")
    (version "0.38.38")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ap3q9b90lnp1razrh4wdnfs0icrs2mplmzb7qlm53jkwqlh49ma"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-errno" ,rust-errno-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
     "This package provides Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rustls-0.23
  (package
    (name "rust-rustls")
    (version "0.23.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i5h0sw8j6bly8rilidb0snd5czpkyjhzqfcd890mz1f7628r3y5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-brotli" ,rust-brotli-6)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-4)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1)
                       ("rust-zlib-rs" ,rust-zlib-rs-0.2))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust")
    (description
     "This package provides Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.8
  (package
    (name "rust-rustls-native-certs")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12izz1ahwj3yr9fkd39q1w535577z9wsapsahz6jcwxyyaj1ibzw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-security-framework" ,rust-security-framework-2))))
    (home-page "https://github.com/rustls/rustls-native-certs")
    (synopsis
     "rustls-native-certs allows rustls to use the platform native certificate store")
    (description
     "This package provides rustls-native-certs allows rustls to use the platform native certificate store.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-pemfile-2
  (package
    (name "rust-rustls-pemfile")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls-pki-types" ,rust-rustls-pki-types-1))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
     "This package provides Basic .pem file parser for keys and certificates.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-pki-types-1
  (package
    (name "rust-rustls-pki-types")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pki-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jv78c32pgf1i0bn7rzf4xdr9qh5wsvigp6akc1yhzls7hdj1w8n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-web-time" ,rust-web-time-1))))
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description
     "This package provides Shared types for the rustls PKI ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-platform-verifier-0.3
  (package
    (name "rust-rustls-platform-verifier")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-platform-verifier" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08sizxah1vl60nwrc0lcs8mjyafzw36kwziy7fwybaxrjgsa7gck"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-logger" ,rust-android-logger-0.13)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-jni" ,rust-jni-0.19)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-platform-verifier-android" ,rust-rustls-platform-verifier-android-0.1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-security-framework" ,rust-security-framework-2)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rustls/rustls-platform-verifier")
    (synopsis
     "rustls-platform-verifier supports verifying TLS certificates in rustls with the operating system verifier")
    (description
     "This package provides rustls-platform-verifier supports verifying TLS certificates in rustls with the
operating system verifier.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-platform-verifier-android-0.1
  (package
    (name "rust-rustls-platform-verifier-android")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-platform-verifier-android" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustls/rustls-platform-verifier")
    (synopsis
     "The internal JVM support component of the rustls-platform-verifier crate. You shouldn't depend on this directly")
    (description
     "This package provides The internal JVM support component of the rustls-platform-verifier crate.  You
shouldn't depend on this directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-webpki-0.102
  (package
    (name "rust-rustls-webpki")
    (version "0.102.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bl55zb5m78l45d8i5frracq96x8shsshmfwby1zsxm1zpa54swf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-untrusted" ,rust-untrusted-0.9))))
    (home-page "https://github.com/rustls/webpki")
    (synopsis "Web PKI X.509 Certificate Verification")
    (description
     "This package provides Web PKI X.509 Certificate Verification.")
    (license license:isc)))

(define-public rust-rustversion-1
  (package
    (name "rust-rustversion")
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustversion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mm3fckyvb0l2209in1n2k05sws5d9mpkszbnwhq3pkq8apjhpcm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/rustversion")
    (synopsis "Conditional compilation according to rustc compiler version")
    (description
     "This package provides Conditional compilation according to rustc compiler version.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ruzstd-0.6
  (package
    (name "rust-ruzstd")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yygqpar2x910lnii4k5p43aj4943hlnxpczmqhsfddmxrqa8x2i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-derive-more" ,rust-derive-more-0.99)
                       ("rust-twox-hash" ,rust-twox-hash-1))))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format.")
    (license license:expat)))

(define-public rust-ruzstd-0.7
  (package
    (name "rust-ruzstd")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13xz8iv0c96m4mrcx9zmn1rimvfqprv641a3yabsf6wvc59v48jh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-twox-hash" ,rust-twox-hash-1))))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format.")
    (license license:expat)))

(define-public rust-scc-2
  (package
    (name "rust-scc")
    (version "2.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00gri4vh0b30davmvyya4b0m556d7li3rx51vj4h287rsnyy6avr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-loom" ,rust-loom-0.7)
                       ("rust-sdd" ,rust-sdd-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/wvwwvwwv/scalable-concurrent-containers/")
    (synopsis
     "High performance containers and utilities for concurrent and asynchronous programming")
    (description
     "This package provides High performance containers and utilities for concurrent and asynchronous
programming.")
    (license license:asl2.0)))

(define-public rust-schemars-0.8
  (package
    (name "rust-schemars")
    (version "0.8.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14lyx04388wgbilgcm0nl75w6359nw16glswfqv7x2rpi9329h09"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-enumset" ,rust-enumset-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-schemars-derive" ,rust-schemars-derive-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-smol-str" ,rust-smol-str-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Generate JSON Schemas from Rust code")
    (description "This package provides Generate JSON Schemas from Rust code.")
    (license license:expat)))

(define-public rust-schemars-derive-0.8
  (package
    (name "rust-schemars-derive")
    (version "0.8.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03ncmrkldfmdc9skmlyysx2vqdlyyz91r5mbavw77zwaay4fbvmi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde-derive-internals" ,rust-serde-derive-internals-0.29)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Macros for #[derive(JsonSchema)], for use with schemars")
    (description
     "This package provides Macros for #[derive(@code{JsonSchema})], for use with schemars.")
    (license license:expat)))

(define-public rust-sd-notify-0.4
  (package
    (name "rust-sd-notify")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sd-notify" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0la4d28ym4rarm8bax07yh9f5r0jb2iqxwmjz00ffgirgxghrqhv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-sendfd" ,rust-sendfd-0.4))))
    (home-page "https://github.com/lnicola/sd-notify")
    (synopsis "Lightweight crate for systemd service state notifications")
    (description
     "This package provides Lightweight crate for systemd service state notifications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sdd-2
  (package
    (name "rust-sdd")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sdd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a5x3df18ih33mjc3ndvimcf5irb5hkd6p66sgzskahg9jv5hwhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-loom" ,rust-loom-0.7))))
    (home-page "https://github.com/wvwwvwwv/scalable-delayed-dealloc/")
    (synopsis "Scalable lock-free delayed memory reclaimer")
    (description
     "This package provides Scalable lock-free delayed memory reclaimer.")
    (license license:asl2.0)))

(define-public rust-security-framework-2
  (package
    (name "rust-security-framework")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description
     "This package provides Security.framework bindings for @code{macOS} and @code{iOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-sys-2
  (package
    (name "rust-security-framework-sys")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1byfpx39sbmndfjrlqqylcxdpn3mpjyb9d92dffzw24vkgz2knkm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
     "This package provides Apple `Security.framework` low-level FFI bindings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-selinux-0.4
  (package
    (name "rust-selinux")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "selinux" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d9hkh8p38ys8igv45rqg1ibsx8zhm8z2fm3pnv5wc41di1v4f81"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-reference-counted-singleton" ,rust-reference-counted-singleton-0.1)
                       ("rust-selinux-sys" ,rust-selinux-sys-0.6)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-assert-matches" ,rust-assert-matches-1)
                                   ("rust-serial-test" ,rust-serial-test-3)
                                   ("rust-socketpair" ,rust-socketpair-0.19)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://codeberg.org/koutheir/selinux.git")
    (synopsis "Flexible Mandatory Access Control for Linux")
    (description
     "This package provides Flexible Mandatory Access Control for Linux.")
    (license license:expat)))

(define-public rust-sendfd-0.4
  (package
    (name "rust-sendfd")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sendfd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "160pf2dp5r8smjc6ssk6jf9k93vc280wk8i362xi6zi6zjw72jv0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/standard-ai/sendfd")
    (synopsis "Send file descriptors along with data over UNIX domain sockets")
    (description
     "This package provides Send file descriptors along with data over UNIX domain sockets.")
    (license (list license:asl2.0 license:bsd-3))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.214")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1danzh1zw9pg10y3bg0b7bvbkqbk5dfpiwchg2ni4757mj9k2p7m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-bytes-0.11
  (package
    (name "rust-serde-bytes")
    (version "0.11.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_bytes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sjwczchd9p4ak4m644jpkv4r181zr8yj14fdjll1fq6rc2caz1q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/bytes")
    (synopsis "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
    (description
     "This package provides Optimized handling of `&[u8]` and `Vec<u8>` for Serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-cbor-2-0.12
  (package
    (name "rust-serde-cbor-2")
    (version "0.12.0-dev")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_cbor_2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mfxl2b08f5w1xyq7740kf4vvyqnsqrg804vpvfiw7z097s7avdl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-half" ,rust-half-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/kanidm/cbor")
    (synopsis "CBOR support for serde")
    (description "This package provides CBOR support for serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.214")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rkp2idkb2p9s96fpqhlzp01qiby63wf1p2wc3x2hph93xw3ylny"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "This package provides Macros 1.1 implementation of #[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.132")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00yv8vyn1qiplziswm1vwam4a0xs1rfr162q75njc85kyjpvy9np"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-spanned-0.6
  (package
    (name "rust-serde-spanned")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_spanned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v9h2nlg8r7n7dkbgj1aw59g35kl869l652wc6zi2f4zawqinnzb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Serde-compatible spanned Value")
    (description "This package provides Serde-compatible spanned Value.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-wasm-bindgen-0.4
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gr8hrr2zx9wqq02vh5lmsyhyaf0agvapf42glq1940drlqw1d73"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description
     "This package provides Native Serde adapter for wasm-bindgen.")
    (license license:expat)))

(define-public rust-serde-wasm-bindgen-0.5
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03m01y4l2kqz63pb1bip52j8bqilzlhhsa7asfdanmrwhgi47cgk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description
     "This package provides Native Serde adapter for wasm-bindgen.")
    (license license:expat)))

(define-public rust-serde-with-3
  (package
    (name "rust-serde-with")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_with" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05z83zkx9q8k4yw3z7isb3l95c5k43q0hwcz8h739f5jdnnvsa4f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-doc-comment" ,rust-doc-comment-0.3)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-hashbrown" ,rust-hashbrown-0.15)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-with-macros" ,rust-serde-with-macros-3)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-expect-test" ,rust-expect-test-1)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-jsonschema" ,rust-jsonschema-0.18)
                                   ("rust-mime" ,rust-mime-0.3)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-rmp-serde" ,rust-rmp-serde-1)
                                   ("rust-ron" ,rust-ron-0.8)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-schemars" ,rust-schemars-0.8)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.6)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                                   ("rust-version-sync" ,rust-version-sync-0.9))))
    (home-page "https://github.com/jonasbb/serde_with/")
    (synopsis "Custom de/serialization functions for Rust's serde")
    (description
     "This package provides Custom de/serialization functions for Rust's serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-with-macros-3
  (package
    (name "rust-serde-with-macros")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_with_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17d7viab3z0ypf4jzpn73xydxn22c911n5nsycjgfkl5m4a6514x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling" ,rust-darling-0.20)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/jonasbb/serde_with/")
    (synopsis "proc-macro library for serde_with")
    (description "This package provides proc-macro library for serde_with.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serial-test-3
  (package
    (name "rust-serial-test")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serial_test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zfpzr8jl7dpf4m8dk8fr9w1wfwhnjrccmyg46h41wmcw9zlhjsb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-fslock" ,rust-fslock-0.2)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-scc" ,rust-scc-2)
                       ("rust-serial-test-derive" ,rust-serial-test-derive-3))))
    (home-page "https://github.com/palfrey/serial_test/")
    (synopsis "Allows for the creation of serialised Rust tests")
    (description
     "This package provides Allows for the creation of serialised Rust tests.")
    (license license:expat)))

(define-public rust-serial-test-derive-3
  (package
    (name "rust-serial-test-derive")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serial_test_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rrx13r2kdahbgs9qfvbx63zgzafrjjpiq42vsy1zvxw4nrrvzl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/palfrey/serial_test/")
    (synopsis "Helper crate for serial_test")
    (description "This package provides Helper crate for serial_test.")
    (license license:expat)))

(define-public rust-serialport-4
  (package
    (name "rust-serialport")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serialport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gqqxs2c1i3ald31404r00mfzy99lhma8arr5f5mkgyrkribn7i4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-io-kit-sys" ,rust-io-kit-sys-0.4)
                       ("rust-libudev" ,rust-libudev-0.3)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unescaper" ,rust-unescaper-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/serialport/serialport-rs")
    (synopsis "cross-platform low-level serial port library.")
    (description
     "This package provides a cross-platform low-level serial port library.")
    (license license:mpl2.0)))

(define-public rust-sha-crypt-0.5
  (package
    (name "rust-sha-crypt")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha-crypt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11mjg1n4hl945m15xzany4w9wpwpk8qjykvm5pa130wdf84r1rw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/sha-crypt")
    (synopsis
     "Pure Rust implementation of the SHA-crypt password hash based on SHA-512
as implemented by the POSIX crypt C library")
    (description
     "This package provides Pure Rust implementation of the SHA-crypt password hash based on SHA-512 as
implemented by the POSIX crypt C library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slab" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
     "This package provides Pre-allocated storage for a uniform data type.")
    (license license:expat)))

(define-public rust-smol-2
  (package
    (name "rust-smol")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bbws2bsp00fd5x6k23ja13p158vk76s2adaqxdgh7p5b6936dg6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-channel" ,rust-async-channel-2)
                       ("rust-async-executor" ,rust-async-executor-1)
                       ("rust-async-fs" ,rust-async-fs-2)
                       ("rust-async-io" ,rust-async-io-2)
                       ("rust-async-lock" ,rust-async-lock-3)
                       ("rust-async-net" ,rust-async-net-2)
                       ("rust-async-process" ,rust-async-process-2)
                       ("rust-blocking" ,rust-blocking-1)
                       ("rust-futures-lite" ,rust-futures-lite-2))))
    (home-page "https://github.com/smol-rs/smol")
    (synopsis "small and fast async runtime")
    (description "This package provides a small and fast async runtime.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-smol-potat-1
  (package
    (name "rust-smol-potat")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol-potat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13nqzzqjscav3flc9jhwiabw8vnb22mv2accgilsn3swmxhzlkw9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-smol-potat-macro" ,rust-smol-potat-macro-0.6))))
    (home-page "https://github.com/wusyong/smol-potat")
    (synopsis "Proc macro for smol runtime")
    (description "This package provides Proc macro for smol runtime.")
    (license (list license:expat license:asl2.0))))

(define-public rust-smol-potat-macro-0.6
  (package
    (name "rust-smol-potat-macro")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol-potat-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cirpy1309cr3n6zbmia66miyidih88sinpanj2r61hqk89dhz3b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/wusyong/smol-potat")
    (synopsis "Proc macro for smol runtime")
    (description "This package provides Proc macro for smol runtime.")
    (license (list license:expat license:asl2.0))))

(define-public rust-smolset-1
  (package
    (name "rust-smolset")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smolset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q3vkj3k5qb38bbdky94kvkqb4w421c5zinnwwlm5p0mzvl75lx8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/hbina/smolset")
    (synopsis
     "\"
An unordered set of elements optimized for small sizes.
This is a fork of the original library with overhauled internals, better fallback perforamance (O(1) insert and find) and more features!")
    (description
     "This package provides \" An unordered set of elements optimized for small sizes.  This is a fork of the
original library with overhauled internals, better fallback perforamance (O(1)
insert and find) and more features!")
    (license license:expat)))

(define-public rust-snapbox-0.6
  (package
    (name "rust-snapbox")
    (version "0.6.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "snapbox" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ym77i24r1y0apgildzdag20hzkccxfay7z22p810pgq0xi96z02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-anstyle-svg" ,rust-anstyle-svg-0.1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-content-inspector" ,rust-content-inspector-0.2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-escargot" ,rust-escargot-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-similar" ,rust-similar-2)
                       ("rust-snapbox-macros" ,rust-snapbox-macros-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-wait-timeout" ,rust-wait-timeout-0.2)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
    (synopsis "Snapshot testing toolbox")
    (description "This package provides Snapshot testing toolbox.")
    (license (list license:expat license:asl2.0))))

(define-public rust-snapbox-macros-0.3
  (package
    (name "rust-snapbox-macros")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "snapbox-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bv4lq1kw1vrd9lk7yk79a0z8q8nma2502ifysv1p913r99rymhn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6))))
    (home-page "https://github.com/assert-rs/trycmd/tree/main/crates/snapbox")
    (synopsis "Snapshot testing toolbox")
    (description "This package provides Snapshot testing toolbox.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sshkey-attest-0.5
  (package
    (name "rust-sshkey-attest")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sshkey-attest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00jskqmrczi7msz8h65qgm85j9nm6lgaxh0p9xn1dfl7vsm5wa1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-cbor-2" ,rust-serde-cbor-2-0.12)
                       ("rust-sshkeys" ,rust-sshkeys-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webauthn-rs-core" ,rust-webauthn-rs-core-0.5))
       #:cargo-development-inputs (("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "FIDO SK SSH Key Attestation")
    (description "This package provides FIDO SK SSH Key Attestation.")
    (license license:mpl2.0)))

(define-public rust-sshkeys-0.3
  (package
    (name "rust-sshkeys")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sshkeys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g85ayqcarrs1hblj1a3v5nc2904mnbz1byipagavxsbs9rp8a25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/dnaeon/rust-sshkeys")
    (synopsis "Rust library for parsing OpenSSH certificates and public keys")
    (description
     "This package provides Rust library for parsing @code{OpenSSH} certificates and public keys.")
    (license license:bsd-2)))

(define-public rust-sspi-0.11
  (package
    (name "rust-sspi")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sspi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08cfaqnga90xynh5n7pyskqjcs3m5227q0ldibi0nafr8ymizlqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-dnssd" ,rust-async-dnssd-0.5)
                       ("rust-async-recursion" ,rust-async-recursion-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-crypto-mac" ,rust-crypto-mac-0.11)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-md4" ,rust-md4-0.10)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-num-derive" ,rust-num-derive-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-picky" ,rust-picky-7)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.8)
                       ("rust-picky-asn1-der" ,rust-picky-asn1-der-0.4)
                       ("rust-picky-asn1-x509" ,rust-picky-asn1-x509-0.12)
                       ("rust-picky-krb" ,rust-picky-krb-0.8)
                       ("rust-portpicker" ,rust-portpicker-0.1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.23)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-windows" ,rust-windows-0.51)
                       ("rust-windows-sys" ,rust-windows-sys-0.48)
                       ("rust-winreg" ,rust-winreg-0.51)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/devolutions/sspi-rs")
    (synopsis
     "Rust implementation of the Security Support Provider Interface (SSPI) API")
    (description
     "This package provides a Rust implementation of the Security Support Provider
Interface (SSPI) API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-svg-0.13
  (package
    (name "rust-svg")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "svg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04kim0zxjfcif7aksd4rwrsgxva5hr24hhjd6z94k13y6fnibn02"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bodoni/svg")
    (synopsis "The package provides an SVG composer and parser")
    (description
     "This package provides The package provides an SVG composer and parser.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-symbolic-common-10
  (package
    (name "rust-symbolic-common")
    (version "10.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symbolic-common" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dz7k6wnv0ji860agi8vijdi24nrzvjsy1vzjp853qpd331wsm8v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-debugid" ,rust-debugid-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/getsentry/symbolic")
    (synopsis
     "Common types and utilities for symbolic, a library to symbolicate and process
stack traces from native applications, minidumps or minified JavaScript.")
    (description
     "This package provides Common types and utilities for symbolic, a library to symbolicate and process
stack traces from native applications, minidumps or minified @code{JavaScript}.")
    (license license:expat)))

(define-public rust-symbolic-demangle-10
  (package
    (name "rust-symbolic-demangle")
    (version "10.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symbolic-demangle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12g40acxl8ppnd4d6hnafgpkij2abcclwfkazwgsi0x4x1xqkgkr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-msvc-demangler" ,rust-msvc-demangler-0.9)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-symbolic-common" ,rust-symbolic-common-10))))
    (home-page "https://github.com/getsentry/symbolic")
    (synopsis
     "library to demangle symbols from various languages and compilers.")
    (description
     "This package provides a library to demangle symbols from various languages and
compilers.")
    (license license:expat)))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.87")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bd3mfcswvn4jkrp7ich5kk58kmpph8412yxd36nsfnh8vilrai5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "This package provides Parser for Rust source code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sync-wrapper-1
  (package
    (name "rust-sync-wrapper")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sync_wrapper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "150k6lwvr4nl237ngsz8fj5j78k712m4bggrfyjsidllraz5l1m7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3))))
    (home-page "https://docs.rs/sync_wrapper")
    (synopsis
     "tool for enlisting the compiler's help in proving the absence of concurrency")
    (description
     "This package provides a tool for enlisting the compiler's help in proving the
absence of concurrency.")
    (license license:asl2.0)))

(define-public rust-system-configuration-0.6
  (package
    (name "rust-system-configuration")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-system-configuration-sys" ,rust-system-configuration-sys-0.6))))
    (home-page "https://github.com/mullvad/system-configuration-rs")
    (synopsis "Bindings to SystemConfiguration framework for macOS")
    (description
     "This package provides Bindings to @code{SystemConfiguration} framework for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-system-configuration-sys-0.6
  (package
    (name "rust-system-configuration-sys")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "system-configuration-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/mullvad/system-configuration-rs")
    (synopsis "Low level bindings to SystemConfiguration framework for macOS")
    (description
     "This package provides Low level bindings to @code{SystemConfiguration} framework for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nyagmbd4v5g6nzfydiihcn6l9j1w9bxgzyca5lyzgnhcbyckwph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-terminal-size-0.4
  (package
    (name "rust-terminal-size")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "terminal_size" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vx6a5klj7sjkx59v78gh93j445s09y2fasiykwgsb04rbbrnnag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description
     "This package provides Gets the size of your Linux or Windows terminal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.63")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092p83mf4p1vkjb2j6h6z96dan4raq2simhirjv12slbndq26d60"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "This package provides derive(Error).")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.63")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qd21l2jjrkvnpr5da3l3b58v4wmrkn6aa0h1z5dg6kb8rc8nmd4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
     "This package provides Implementation detail of the `thiserror` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11g8hdpahgrf1wwl2rpsg5nxq3aj7ri6xr672v4qcij6cgjqizax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-deranged" ,rust-deranged-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-conv" ,rust-num-conv-0.1)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-powerfmt" ,rust-powerfmt-0.2)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-core" ,rust-time-core-0.1)
                       ("rust-time-macros" ,rust-time-macros-0.2))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]")
    (description
     "This package provides Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kqwxvfh2jkpg38fy673d6danh1bhcmmbsmffww3mphgail2l99z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-conv" ,rust-num-conv-0.1)
                       ("rust-time-core" ,rust-time-core-0.1))))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.")
    (description
     "This package provides Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio-macros" ,rust-tokio-macros-2)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-libc" ,rust-libc-0.2)
                                   ("rust-loom" ,rust-loom-0.7)
                                   ("rust-mio-aio" ,rust-mio-aio-0.9)
                                   ("rust-mockall" ,rust-mockall-0.11)
                                   ("rust-nix" ,rust-nix-0.29)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-socket2" ,rust-socket2-0.5)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.")
    (description
     "This package provides An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-tokio-macros-2
  (package
    (name "rust-tokio-macros")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio's proc macros.")
    (description "This package provides Tokio's proc macros.")
    (license license:expat)))

(define-public rust-tokio-openssl-0.6
  (package
    (name "rust-tokio-openssl")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pga4xm5fcms6k1rqg4hsl8mmna7qiizhdlsgxbbffx4r94nipsr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/tokio-rs/tokio-openssl")
    (synopsis "An implementation of SSL streams for Tokio backed by OpenSSL")
    (description
     "This package provides An implementation of SSL streams for Tokio backed by @code{OpenSSL}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-rustls-0.26
  (package
    (name "rust-tokio-rustls")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m00czrmk8x7pdjnz10a3da3i1d0sdf9j9vfp5dnk5ss1q6w8yqc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/tokio-rustls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls")
    (description
     "This package provides Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-socks-0.5
  (package
    (name "rust-tokio-socks")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-socks" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gq40sgggz21wfpshiq8pryh062vp7m36rrz3c8c2wj60aw70iqd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/sticnarf/tokio-socks")
    (synopsis "Asynchronous SOCKS proxy support for Rust")
    (description
     "This package provides Asynchronous SOCKS proxy support for Rust.")
    (license license:expat)))

(define-public rust-tokio-tungstenite-0.18
  (package
    (name "rust-tokio-tungstenite")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z8bxhq6d1ndh4x914wwk72l93ha1sl0jmnb6knvqiqi869rqcal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.18)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Tokio binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-tokio-tungstenite-0.24
  (package
    (name "rust-tokio-tungstenite")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nfw1i6yy120a14h1xagd4f31k3g1mz4rdxpvgh77jcd4i7ggigd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.8)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tungstenite" ,rust-tungstenite-0.24)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Tokio binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-tokio-util-0.7
  (package
    (name "rust-tokio-util")
    (version "0.7.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0spc0g4irbnf2flgag22gfii87avqzibwfm0si0d1g0k9ijw7rv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-test" ,rust-futures-test-0.3)
                                   ("rust-parking-lot" ,rust-parking-lot-0.12)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.")
    (description
     "This package provides Additional utilities for working with Tokio.")
    (license license:expat)))

(define-public rust-toml-datetime-0.6
  (package
    (name "rust-toml-datetime")
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_datetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "TOML-compatible datetime type")
    (description "This package provides a TOML-compatible datetime type.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.22
  (package
    (name "rust-toml-edit")
    (version "0.22.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07ffw4626k6abicjxb2idh12f1p5fn965zk660zhqsyj5b048g2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-winnow" ,rust-winnow-0.6))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Yet another format-preserving TOML parser")
    (description
     "This package provides Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tonic-0.9
  (package
    (name "rust-tonic")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nlx35lvah5hdcp6lg1d6dlprq0zz8ijj6f727szfcv479m6d0ih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-timeout" ,rust-hyper-timeout-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-prost" ,rust-prost-0.11)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.23))))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis
     "gRPC over HTTP/2 implementation focused on high performance, interoperability, and flexibility.")
    (description
     "This package provides a @code{gRPC} over HTTP/2 implementation focused on high
performance, interoperability, and flexibility.")
    (license license:expat)))

(define-public rust-tower-0.5
  (package
    (name "rust-tower")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kvbp97bhb4sk24vhihcz74ngn0i4ygxqikmxndgng3w926r6wr8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hdrhistogram" ,rust-hdrhistogram-7)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Tower is a library of modular and reusable components for building robust
clients and servers.")
    (description
     "This package provides Tower is a library of modular and reusable components for building robust
clients and servers.")
    (license license:expat)))

(define-public rust-tower-http-0.5
  (package
    (name "rust-tower-http")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xakj3x0anp55gjqibiwvzma5iz0w9pcjsr7qk97sx4qm4sd970y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-http-range-header" ,rust-http-range-header-0.4)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                                   ("rust-brotli" ,rust-brotli-3)
                                   ("rust-bytes" ,rust-bytes-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-hyper-util" ,rust-hyper-util-0.1)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower" ,rust-tower-0.4)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                                   ("rust-uuid" ,rust-uuid-1)
                                   ("rust-zstd" ,rust-zstd-0.12))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description
     "This package provides Tower middleware and utilities for HTTP clients and servers.")
    (license license:expat)))

(define-public rust-tower-http-0.6
  (package
    (name "rust-tower-http")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15yb8rh970ll3yrd3lydshysi0x89bnxbqqrypqcbj5vnq51adw4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-http-range-header" ,rust-http-range-header-0.4)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.5)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description
     "This package provides Tower middleware and utilities for HTTP clients and servers.")
    (license license:expat)))

(define-public rust-tower-layer-0.3
  (package
    (name "rust-tower-layer")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-layer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Decorates a `Service` to allow easy composition between `Service`s.")
    (description
     "This package provides Decorates a `Service` to allow easy composition between `Service`s.")
    (license license:expat)))

(define-public rust-tower-service-0.3
  (package
    (name "rust-tower-service")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-service" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Trait representing an asynchronous, request / response based, client or server.")
    (description
     "This package provides Trait representing an asynchronous, request / response based, client or server.")
    (license license:expat)))

(define-public rust-tracing-forest-0.1
  (package
    (name "rust-tracing-forest")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-forest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bsr88f4shanjr86ajrx9p8dmsfxxmr24a8llhxixpadn5fq6h7f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/QnnOkabayashi/tracing-forest")
    (synopsis
     "Preserving contextual coherence among trace data from concurrent tasks")
    (description
     "This package provides Preserving contextual coherence among trace data from concurrent tasks.")
    (license license:expat)))

(define-public rust-tracing-opentelemetry-0.21
  (package
    (name "rust-tracing-opentelemetry")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-opentelemetry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j6kzqphczra4q7vz0wdb0zkqyfb6s81cgsyiz1dsa3qcrmpqckm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-opentelemetry" ,rust-opentelemetry-0.20)
                       ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-log" ,rust-tracing-log-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                                   ("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-opentelemetry" ,rust-opentelemetry-0.20)
                                   ("rust-opentelemetry-jaeger" ,rust-opentelemetry-jaeger-0.19)
                                   ("rust-opentelemetry-otlp" ,rust-opentelemetry-otlp-0.13)
                                   ("rust-opentelemetry-semantic-conventions" ,rust-opentelemetry-semantic-conventions-0.12)
                                   ("rust-opentelemetry-stdout" ,rust-opentelemetry-stdout-0.1)
                                   ("rust-opentelemetry-sdk" ,rust-opentelemetry-sdk-0.20)
                                   ("rust-pprof" ,rust-pprof-0.11)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/tokio-rs/tracing-opentelemetry")
    (synopsis "OpenTelemetry integration for tracing")
    (description
     "This package provides @code{OpenTelemetry} integration for tracing.")
    (license license:expat)))

(define-public rust-trycmd-0.15
  (package
    (name "rust-trycmd")
    (version "0.15.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trycmd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04pspvi2hnixx1y2716di8z5cag5z1icq1jswbwdzb25vkqp71iy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-automod" ,rust-automod-1)
                       ("rust-escargot" ,rust-escargot-0.5)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-humantime-serde" ,rust-humantime-serde-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-snapbox" ,rust-snapbox-0.6)
                       ("rust-toml-edit" ,rust-toml-edit-0.22))))
    (home-page "https://github.com/assert-rs/trycmd")
    (synopsis "Snapshot testing for a herd of CLI tests")
    (description
     "This package provides Snapshot testing for a herd of CLI tests.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tss-esapi-8
  (package
    (name "rust-tss-esapi")
    (version "8.0.0-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tss-esapi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c97bavk96x50wwhqi2ypjcmad6b636hgr6klginv131c6j1f5iw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitfield" ,rust-bitfield-0.13)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-enumflags2" ,rust-enumflags2-0.7)
                       ("rust-hostname-validator" ,rust-hostname-validator-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-malloced" ,rust-malloced-1)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oid" ,rust-oid-0.2)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-picky-asn1" ,rust-picky-asn1-0.8)
                       ("rust-picky-asn1-x509" ,rust-picky-asn1-x509-0.12)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-strum" ,rust-strum-0.25)
                       ("rust-strum-macros" ,rust-strum-macros-0.25)
                       ("rust-tss-esapi-sys" ,rust-tss-esapi-sys-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/parallaxsecond/rust-tss-esapi")
    (synopsis "Rust-native wrapper around TSS 2.0 Enhanced System API")
    (description
     "This package provides Rust-native wrapper around TSS 2.0 Enhanced System API.")
    (license license:asl2.0)))

(define-public rust-tss-esapi-sys-0.5
  (package
    (name "rust-tss-esapi-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tss-esapi-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dfmrbbm2834hzimvj78rhya59mv7wfhnrrfz3aw8bhwb29d2p2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.66)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list tpm2-tss))
    (home-page "https://github.com/parallaxsecond/rust-tss-esapi")
    (synopsis "FFI wrapper around TSS 2.0 Enhanced System API")
    (description
     "This package provides FFI wrapper around TSS 2.0 Enhanced System API.")
    (license license:asl2.0)))

(define-public rust-tungstenite-0.18
  (package
    (name "rust-tungstenite")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1207jv8ciklgnqwjhxc1c1xhplrfab231191apyz0k6d56vnmvih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Lightweight stream-based @code{WebSocket} implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tungstenite-0.24
  (package
    (name "rust-tungstenite")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nsxnxazk4nisgsqpywi6za0hsbc2rd15r1scb5pag7dqvbir8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Lightweight stream-based @code{WebSocket} implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-udev-0.7
  (package
    (name "rust-udev")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "udev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06hr927z0fdn7ay0p817b9x19i5fagmpmvz95yhl4d1pf3bbpgaf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-libudev-sys" ,rust-libudev-sys-0.1)
                       ("rust-mio" ,rust-mio-0.6)
                       ("rust-mio" ,rust-mio-0.7)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/Smithay/udev-rs")
    (synopsis "libudev bindings for Rust")
    (description "This package provides libudev bindings for Rust.")
    (license license:expat)))

(define-public rust-ulid-1
  (package
    (name "rust-ulid")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ulid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12fjcwb62ix1ps105g6fh1zkl0qdvkv4hhcyqb0327yijgr07y84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-postgres-types" ,rust-postgres-types-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-web-time" ,rust-web-time-1))))
    (home-page "https://github.com/dylanhart/ulid-rs")
    (synopsis
     "a Universally Unique Lexicographically Sortable Identifier implementation")
    (description
     "This package provides a Universally Unique Lexicographically Sortable Identifier implementation.")
    (license license:expat)))

(define-public rust-unescaper-0.1
  (package
    (name "rust-unescaper")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unescaper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05dqxnzzkdfj9cl4j8pzi5814rwcxy4ac0a194vx3bx8p9ks2y68"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://hack.ink/unescaper")
    (synopsis
     "Unescape strings with escape sequences written out as literal characters")
    (description
     "This package provides Unescape strings with escape sequences written out as literal characters.")
    (license (list license:gpl3 license:expat))))

(define-public rust-unicode-width-0.2
  (package
    (name "rust-unicode-width")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-width" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-rustc-std-workspace-std" ,rust-rustc-std-workspace-std-1))))
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules.")
    (description
     "This package provides Determine displayed width of `char` and `str` types according to Unicode
Standard Annex #11 rules.")
    (license (list license:expat license:asl2.0))))

(define-public rust-uritemplate-next-0.2
  (package
    (name "rust-uritemplate-next")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uritemplate-next" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "136i9f5vj46z4xqwba3jnvg263gfi2v2zcpcn5aq4lizzk8ripmw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/judemille/rust-uritemplate/")
    (synopsis
     "Rust implementation of RFC6570 - URI Template that can processURI Templates up and to including ones designated Level 4")
    (description
     "This package provides Rust implementation of RFC6570 - URI Template that can @code{processURI}
Templates up and to including ones designated Level 4.")
    (license license:bsd-3)))

(define-public rust-url-2
  (package
    (name "rust-url")
    (version "2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v2dx50mx7xzl9454cl5qmpjnhkbahmn59gd3apyipbgyyylsy12"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-idna" ,rust-idna-0.5)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/servo/rust-url")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
     "This package provides URL library for Rust, based on the WHATWG URL Standard.")
    (license (list license:expat license:asl2.0))))

(define-public rust-utoipa-4
  (package
    (name "rust-utoipa")
    (version "4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utoipa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08xbxz3an28g0rv9agmqs1qix4nrrzppylw24r8clz901skb3by5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-utoipa-gen" ,rust-utoipa-gen-4))
       #:cargo-development-inputs (("rust-assert-json-diff" ,rust-assert-json-diff-2))))
    (home-page "https://github.com/juhaku/utoipa")
    (synopsis "Compile time generated OpenAPI documentation for Rust")
    (description
     "This package provides Compile time generated @code{OpenAPI} documentation for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-utoipa-gen-4
  (package
    (name "rust-utoipa-gen")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utoipa-gen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1glhh4zhldspf3f6dficg2mbrga926mi0pxn58rgajxw09nf3w3v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-ulid" ,rust-ulid-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/juhaku/utoipa")
    (synopsis "Code generation implementation for utoipa")
    (description
     "This package provides Code generation implementation for utoipa.")
    (license (list license:expat license:asl2.0))))

(define-public rust-utoipa-swagger-ui-6
  (package
    (name "rust-utoipa-swagger-ui")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utoipa-swagger-ui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1npjm8cziq2wjgvjymjj6v4jrpxfa3h265ml0hg9c4f08f6qcf8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-actix-web" ,rust-actix-web-4)
                       ("rust-axum" ,rust-axum-0.7)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rocket" ,rust-rocket-0.5)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-utoipa" ,rust-utoipa-4)
                       ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs (("rust-similar" ,rust-similar-2))))
    (home-page "https://github.com/juhaku/utoipa")
    (synopsis "Swagger UI for utoipa")
    (description "This package provides Swagger UI for utoipa.")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sj4l28lif2wm4xrafdfgqjywjzv43wzp8nii9a4i539myhg1igq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.6)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-borsh-derive" ,rust-borsh-derive-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zerocopy" ,rust-zerocopy-0.8))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid-macro-internal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024s8hxxjwgc218kfx9xs274dhnkv1ik9818kv7d0f1sw5zzb4bb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "Private implementation details of the uuid! macro")
    (description
     "This package provides Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-simd-0.8
  (package
    (name "rust-uuid-simd")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid-simd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n0b40m988h52xj03dkcp4plrzvz56r7xha1d681jrjg5ci85c13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-outref" ,rust-outref-0.5)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-vsimd" ,rust-vsimd-0.8))))
    (home-page "https://github.com/Nugine/simd")
    (synopsis "SIMD-accelerated UUID operations")
    (description "This package provides SIMD-accelerated UUID operations.")
    (license license:expat)))

(define-public rust-valico-3
  (package
    (name "rust-valico")
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "valico" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nsyliyyyqys172az9ps7d6fdhzsc2gx35q3cjv8cxijir05cy34"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addr" ,rust-addr-0.11)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-json-pointer" ,rust-json-pointer-0.3)
                       ("rust-jsonway" ,rust-jsonway-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-phf" ,rust-phf-0.8)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-uritemplate-next" ,rust-uritemplate-next-0.2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/rustless/valico")
    (synopsis "JSON Schema validator and JSON coercer")
    (description
     "This package provides JSON Schema validator and JSON coercer.")
    (license license:expat)))

(define-public rust-vsimd-0.8
  (package
    (name "rust-vsimd")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vsimd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r4wn54jxb12r0x023r5yxcrqk785akmbddqkcafz9fm03584c2w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Nugine/simd")
    (synopsis "SIMD utilities")
    (description "This package provides SIMD utilities.")
    (license license:expat)))

(define-public rust-wasite-0.1
  (package
    (name "rust-wasite")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ardaku/wasite/blob/stable/CHANGELOG.md")
    (synopsis "WASI Terminal Environment API")
    (description "This package provides WASI Terminal Environment API.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro-0.2))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.")
    (description
     "This package provides Easy support for interacting between JS and Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool")
    (description
     "This package provides Backend code generation of the wasm-bindgen tool.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-futures-0.4
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.4.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-futures" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
     "This package provides Bridging the gap between Rust Futures and @code{JavaScript} Promises.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (description
     "This package provides Definition of the `#[wasm_bindgen]` attribute, an internal dependency.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-macro-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (description
     "This package provides The part of the implementation of the `#[wasm_bindgen]` attribute that is not in
the shared backend crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.92")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (description
     "This package provides Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-test-0.3
  (package
    (name "rust-wasm-bindgen-test")
    (version "0.3.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yv6hyckfwnp3lkkm93di3jq62g4xqymhi10hlzaz007isjn5gyr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-gg-alloc" ,rust-gg-alloc-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-bindgen-test-macro" ,rust-wasm-bindgen-test-macro-0.3))))
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing crate for wasm-bindgen")
    (description
     "This package provides Internal testing crate for wasm-bindgen.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-test-macro-0.3
  (package
    (name "rust-wasm-bindgen-test-macro")
    (version "0.3.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-test-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w3ypw6b0ffyyx0w83mlb4bw1jmjgza9kdxyjk5h6bhs6lwrgy5p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing macro for wasm-bindgen")
    (description
     "This package provides Internal testing macro for wasm-bindgen.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-timer-0.2
  (package
    (name "rust-wasm-timer")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-timer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zsyijv3wgj9p4q47a4awla8j4kw33jd7da2fsd1wml0nh6wn3my"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1))))
    (home-page "https://github.com/tomaka/wasm-timer")
    (synopsis
     "Abstraction over std::time::Instant and futures-timer that works on WASM")
    (description
     "This package provides Abstraction over std::time::Instant and futures-timer that works on WASM.")
    (license license:expat)))

(define-public rust-wasmparser-0.202
  (package
    (name "rust-wasmparser")
    (version "0.202.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qljgwjv6a6nn9sx6bbz167s0dim4liphgp1sc8ngygscaqb6fn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-semver" ,rust-semver-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-wasmparser-0.215
  (package
    (name "rust-wasmparser")
    (version "0.215.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03nryws9f2awvfd47z5pn67aqif1z7w6z6zl3jw9jhgjh44dxysk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-web-sys-0.3
  (package
    (name "rust-web-sys")
    (version "0.3.69")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "web-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis
     "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (description
     "This package provides Bindings for all Web APIs, a procedurally generated crate from @code{WebIDL}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-webauthn-attestation-ca-0.5
  (package
    (name "rust-webauthn-attestation-ca")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webauthn-attestation-ca" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1117p0iaq0hilwrgc72928izlxv4xlqiyxjsa5ds2335ynx2w3wv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Attestation CA Descriptions")
    (description "This package provides Webauthn Attestation CA Descriptions.")
    (license license:mpl2.0)))

(define-public rust-webauthn-authenticator-rs-0.5
  (package
    (name "rust-webauthn-authenticator-rs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webauthn-authenticator-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vrbs4pdw1ycbc8xy8dc636hq6q07pg72rkdikjgkrpirpd000hc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-authenticator-ctap2-2021" ,rust-authenticator-ctap2-2021-0.3)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-btleplug" ,rust-btleplug-0.11)
                       ("rust-fido-hid-rs" ,rust-fido-hid-rs-0.5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-qrcode" ,rust-qrcode-0.12)
                       ("rust-rpassword" ,rust-rpassword-5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-cbor-2" ,rust-serde-cbor-2-0.12)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.18)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webauthn-rs-core" ,rust-webauthn-rs-core-0.5)
                       ("rust-webauthn-rs-proto" ,rust-webauthn-rs-proto-0.5)
                       ("rust-windows" ,rust-windows-0.41))
       #:cargo-development-inputs (("rust-bardecoder" ,rust-bardecoder-0.4)
                                   ("rust-clap" ,rust-clap-4)
                                   ("rust-image" ,rust-image-0.23)
                                   ("rust-serialport" ,rust-serialport-4)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Authenticator Client Library")
    (description
     "This package provides Webauthn Authenticator Client Library.")
    (license license:mpl2.0)))

(define-public rust-webauthn-rs-0.5
  (package
    (name "rust-webauthn-rs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webauthn-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h16yp1808ispkzkxavmq332fdpswqri9vp8yw33wvn2kvf7r7gv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webauthn-rs-core" ,rust-webauthn-rs-core-0.5))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Framework for Rust Web Servers")
    (description
     "This package provides Webauthn Framework for Rust Web Servers.")
    (license license:mpl2.0)))

(define-public rust-webauthn-rs-core-0.5
  (package
    (name "rust-webauthn-rs-core")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webauthn-rs-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cq054by77axp4g1z1k0b9651byxjfp6wjm70pyvhf21gzff27ng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-compact-jwt" ,rust-compact-jwt-0.2)
                       ("rust-der-parser" ,rust-der-parser-7)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-cbor-2" ,rust-serde-cbor-2-0.12)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webauthn-attestation-ca" ,rust-webauthn-attestation-ca-0.5)
                       ("rust-webauthn-rs-proto" ,rust-webauthn-rs-proto-0.5)
                       ("rust-x509-parser" ,rust-x509-parser-0.13))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Cryptographic Operation Handling")
    (description
     "This package provides Webauthn Cryptographic Operation Handling.")
    (license license:mpl2.0)))

(define-public rust-webauthn-rs-proto-0.5
  (package
    (name "rust-webauthn-rs-proto")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webauthn-rs-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hyw95rzvyha574y98cwhlv2883pnf35pcxxqgp4hzv0ak16s70z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-base64urlsafedata" ,rust-base64urlsafedata-0.5)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.4)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Specification Bindings")
    (description "This package provides Webauthn Specification Bindings.")
    (license license:mpl2.0)))

(define-public rust-webdriver-0.50
  (package
    (name "rust-webdriver")
    (version "0.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webdriver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wfv88m476s58wxypqjdjlly4pcj95jn8pk3cm8dcdidn5wvjjhl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.16)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-warp" ,rust-warp-0.3))))
    (home-page
     "https://hg.mozilla.org/mozilla-central/file/tip/testing/webdriver")
    (synopsis
     "Library implementing the wire protocol for the W3C WebDriver specification")
    (description
     "This package provides Library implementing the wire protocol for the W3C @code{WebDriver}
specification.")
    (license license:mpl2.0)))

(define-public rust-weezl-0.1
  (package
    (name "rust-weezl")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "weezl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3))))
    (home-page "https://github.com/image-rs/lzw")
    (synopsis "Fast LZW compression and decompression")
    (description
     "This package provides Fast LZW compression and decompression.")
    (license (list license:expat license:asl2.0))))

(define-public rust-whoami-1
  (package
    (name "rust-whoami")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "whoami" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vdvm6sga4v9515l6glqqfnmzp246nq66dd09cw5ri4fyn3mnb9p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-redox-syscall" ,rust-redox-syscall-0.5)
                       ("rust-wasite" ,rust-wasite-0.1)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/ardaku/whoami/blob/v1/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment")
    (description
     "This package provides Retrieve the current user and environment.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-widestring-1
  (package
    (name "rust-widestring")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "widestring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "048kxd6iykzi5la9nikpc5hvpp77hmjf1sw43sl3z2dcdrmx66bj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/starkat99/widestring-rs")
    (synopsis
     "wide string Rust library for converting to and from wide strings, such as those often used in Windows API or other FFI libaries. Both `u16` and `u32` string types are provided, including support for UTF-16 and UTF-32, malformed encoding, C-style strings, etc.")
    (description
     "This package provides a wide string Rust library for converting to and from wide
strings, such as those often used in Windows API or other FFI libaries.  Both
`u16` and `u32` string types are provided, including support for UTF-16 and
UTF-32, malformed encoding, C-style strings, etc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.41
  (package
    (name "rust-windows")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1asq00qqn0pbypyzni8p0xm33pc51al1fd4a999d1y61wafxcgjs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.41)
                       ("rust-windows-interface" ,rust-windows-interface-0.41)
                       ("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.41)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.41)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.41)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.41)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.41)
                       ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.41)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.41))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.51
  (package
    (name "rust-windows")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ja500kr2pdvz9lxqmcr7zclnnwpvw28z78ypkrc4f7fqlb9j8na"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.51)
                       ("rust-windows-implement" ,rust-windows-implement-0.51)
                       ("rust-windows-interface" ,rust-windows-interface-0.51)
                       ("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.41
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zci61gds97n7v6pnfp75dd2xsdmna09wwg6nkn7h8jgfxhjfg8n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.52
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.41
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "030arpfkvvjda1dgxlsgfmcr8kq83765k8v686wd607bpkr5y07g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.52
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_aarch64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-bindgen-0.56
  (package
    (name "rust-windows-bindgen")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0inv2w78qv6375ndrgm9vilkgscwak80igz8vkf7zw8c6fk3x3m2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-windows-metadata" ,rust-windows-metadata-0.56))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata compiler")
    (description "This package provides Windows metadata compiler.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-core-0.51
  (package
    (name "rust-windows-core")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r1f57hsshsghjyc7ypp2s0i78f7b1vr93w68sdb8baxyf2czy7i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.41
  (package
    (name "rust-windows-i686-gnu")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02s4n5dymg33aky1dclpdnrvq0wcha3y8a5jnkw06bz3a4nxzd02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.52
  (package
    (name "rust-windows-i686-gnu")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnullvm-0.52
  (package
    (name "rust-windows-i686-gnullvm")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.41
  (package
    (name "rust-windows-i686-msvc")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xsy70vjcfs811l0agg54lgkh1w59lmxfzghcwr2y7sp6il9d2jn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.52
  (package
    (name "rust-windows-i686-msvc")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_i686_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.41
  (package
    (name "rust-windows-implement")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mdcx3ilm6yf8lvs7qniz4fcv05iazcming23yk0rmncsp62h86j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description
     "This package provides The implement macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.51
  (package
    (name "rust-windows-implement")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mg5q1rzfix05xvl4fhmp5b6azm8a0pn4dk8hkc21by5zs71aazv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description
     "This package provides The implement macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-interface-0.41
  (package
    (name "rust-windows-interface")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13ghjj4yzrk4s438ychlgiw5i2fygi060vd11gfqrqr17b279xgr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description
     "This package provides The interface macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-interface-0.51
  (package
    (name "rust-windows-interface")
    (version "0.51.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xps1k3ii3cdiniv896mgcv3mbmm787gl4937m008k763hzfcih5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description
     "This package provides The interface macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-metadata-0.56
  (package
    (name "rust-windows-metadata")
    (version "0.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d1vizbp6b1wjh3qnjrh120w1iwqal3lfj52wdac847zgy1gg4rr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows metadata reader")
    (description "This package provides Windows metadata reader.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-registry-0.2
  (package
    (name "rust-windows-registry")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-registry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-result" ,rust-windows-result-0.2)
                       ("rust-windows-strings" ,rust-windows-strings-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows registry")
    (description "This package provides Windows registry.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-result-0.2
  (package
    (name "rust-windows-result")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-result" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Windows error handling")
    (description "This package provides Windows error handling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-strings-0.1
  (package
    (name "rust-windows-strings")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-strings" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-result" ,rust-windows-result-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.59
  (package
    (name "rust-windows-sys")
    (version "0.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-targets-0.52
  (package
    (name "rust-windows-targets")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-targets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.52)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.52)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.52)
                       ("rust-windows-i686-gnullvm" ,rust-windows-i686-gnullvm-0.52)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.52)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.52)
                       ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.52)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import libs for Windows")
    (description "This package provides Import libs for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.41
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "157rm1q3m6vsxzzmq953rp4624wnwwx96v89dlklwi6z3an5sfgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.52
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.41
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q5mvglb8djq3l910iwqrlhdqi67svd8m76mw4vb9m8mnwifrs4h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.52
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_gnullvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.41
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09krzmz8x1ckf8yc9kf5c6zpkrsybvigc4hmvwcr0rkb47q635wb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.52
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.52.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows_x86_64_msvc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "This package provides Import lib for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winnow-0.6
  (package
    (name "rust-winnow")
    (version "0.6.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winnow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vrsrnf2nm9jsk1161x1vacmi3ns4h3h10fib91rs28zd6jbvab8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.3)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-terminal-size" ,rust-terminal-size-0.2))))
    (home-page "https://github.com/winnow-rs/winnow")
    (synopsis "byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a byte-oriented, zero-copy, parser combinators library.")
    (license license:expat)))

(define-public rust-yew-0.20
  (package
    (name "rust-yew")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k7igb7w30b9jshb72jaflhswsar892v8gicjg10rds38gjczgjx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo" ,rust-gloo-0.8)
                       ("rust-html-escape" ,rust-html-escape-0.2)
                       ("rust-implicit-clone" ,rust-implicit-clone-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-prokio" ,rust-prokio-0.1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-yew-macro" ,rust-yew-macro-0.20))
       #:cargo-development-inputs (("rust-gloo" ,rust-gloo-0.8)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://yew.rs")
    (synopsis "framework for creating reliable and efficient web applications")
    (description
     "This package provides a framework for creating reliable and efficient web
applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yew-macro-0.20
  (package
    (name "rust-yew-macro")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00c3x9gg1miqyc0r2wwzlqazm34mcgdqi6fad2l1w7s03ly2ak5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-boolinator" ,rust-boolinator-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prettyplease" ,rust-prettyplease-0.1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/yewstack/yew")
    (synopsis "framework for making client-side single-page apps")
    (description
     "This package provides a framework for making client-side single-page apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yew-router-0.17
  (package
    (name "rust-yew-router")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vhl0k7nclh2653ga9ikbxavnncdnm4brgczwg2scwi5dm4f0vj2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gloo" ,rust-gloo-0.8)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-route-recognizer" ,rust-route-recognizer-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-yew" ,rust-yew-0.20)
                       ("rust-yew-router-macro" ,rust-yew-router-macro-0.17))
       #:cargo-development-inputs (("rust-serde" ,rust-serde-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-web-sys" ,rust-web-sys-0.3)
                                   ("rust-yew" ,rust-yew-0.20))))
    (home-page "https://github.com/yewstack/yew")
    (synopsis "router implementation for the Yew framework")
    (description
     "This package provides a router implementation for the Yew framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yew-router-macro-0.17
  (package
    (name "rust-yew-router-macro")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yew-router-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qvadrfsdrh1vjglg4s7crs46ljl31wdrpj40spxs34yng6lkcl9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/yewstack/yew")
    (synopsis "Contains macros used with yew-router")
    (description "This package contains macros used with yew-router.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zerocopy-0.8
  (package
    (name "rust-zerocopy")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19ynnnldvfvk4xvirdpxi44sg2w4irzr4ilplbdjlvz3vkk36kjs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zerocopy-derive" ,rust-zerocopy-derive-0.8))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis
     "Zerocopy makes zero-cost memory manipulation effortless. We write \"unsafe\" so you don't have to")
    (description
     "This package provides Zerocopy makes zero-cost memory manipulation effortless.  We write \"unsafe\" so
you don't have to.")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-zerocopy-derive-0.8
  (package
    (name "rust-zerocopy-derive")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hfl2xpag7i7fj48iip21gd2r1rh04sv7frvrvnfdg91rjs3gl9w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description
     "This package provides Custom derive for traits from the zerocopy crate.")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zeroize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!")
    (description
     "This package provides Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
optimized away by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-zip-2
  (package
    (name "rust-zip")
    (version "2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zip" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0biy7mxqnzaibz603jmly52gzvyvqmbndlgvw5n2i5n2xy98rpa0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-constant-time-eq" ,rust-constant-time-eq-0.3)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-deflate64" ,rust-deflate64-0.1)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-lzma-rs" ,rust-lzma-rs-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zeroize" ,rust-zeroize-1)
                       ("rust-zopfli" ,rust-zopfli-0.8)
                       ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://github.com/zip-rs/zip2.git")
    (synopsis "Library to support the reading and writing of zip files.")
    (description
     "This package provides Library to support the reading and writing of zip files.")
    (license license:expat)))

(define-public rust-zlib-rs-0.2
  (package
    (name "rust-zlib-rs")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zlib-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04a8iasrpjshw2xm6qch0zfzh4m5659pv5qzf2dyagjmaf2bnhp9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/memorysafety/zlib-rs")
    (synopsis "memory-safe zlib implementation written in rust")
    (description
     "This package provides a memory-safe zlib implementation written in rust.")
    (license license:zlib)))

(define-public rust-zopfli-0.8
  (package
    (name "rust-zopfli")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zopfli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ip9azz9ldk19m0m1hdppz3n5zcz0cywbg1vx59g4p5c3cwry0g5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-lockfree-object-pool" ,rust-lockfree-object-pool-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-simd-adler32" ,rust-simd-adler32-0.3))))
    (home-page "https://github.com/zopfli-rs/zopfli")
    (synopsis "Rust implementation of the Zopfli compression algorithm.")
    (description
     "This package provides a Rust implementation of the Zopfli compression algorithm.")
    (license license:asl2.0)))

(define-public rust-zstd-0.13
  (package
    (name "rust-zstd")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ygkr6wspm9clbp7ykyl0rv69cfsf9q4lic9wcqiwn34lrwbgwpw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-7))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding for the zstd compression library")
    (description
     "This package provides Binding for the zstd compression library.")
    (license license:expat)))

(define-public rust-zstd-0.5
  (package
    (name "rust-zstd")
    (version "0.5.4+zstd.1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0419r39dk3zx3xxlhpr9i9nac63qhn1si1qkyqbib2xsn6ynx6b9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-zstd-safe" ,rust-zstd-safe-2))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding for the zstd compression library")
    (description
     "This package provides Binding for the zstd compression library.")
    (license license:expat)))

(define-public rust-zstd-safe-2
  (package
    (name "rust-zstd-safe")
    (version "2.0.6+zstd.1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17m46z6b26ai70xrdgv98yp52676ajbx32a5sibf5klynqgr7alq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-zstd-sys" ,rust-zstd-sys-1))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings for the zstd compression library")
    (description
     "This package provides Safe low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-safe-7
  (package
    (name "rust-zstd-safe")
    (version "7.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nch85m5cr493y26yvndm6a8j6sd9mxpr2awrim3dslcnr6sp8sl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-sys" ,rust-zstd-sys-2))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings for the zstd compression library")
    (description
     "This package provides Safe low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-sys-1
  (package
    (name "rust-zstd-sys")
    (version "1.4.18+zstd.1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10gcxawj2rxgxcxdv2gyv83gxckwg0vhvn3iyx1qz0q6hxvyirm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.56)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-itertools" ,rust-itertools-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (propagated-inputs (list zstd))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Low-level bindings for the zstd compression library")
    (description
     "This package provides Low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zxcvbn-2
  (package
    (name "rust-zxcvbn")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zxcvbn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r4l1i76sdk51syp032kbjngmxwgqhjprs40yfda43ppzx8shgqh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-derive-builder" ,rust-derive-builder-0.12)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.11)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/shssoichiro/zxcvbn-rs")
    (synopsis
     "An entropy-based password strength estimator, originally for Javascript by Dropbox")
    (description
     "This package provides An entropy-based password strength estimator, originally for Javascript by
Dropbox.")
    (license license:expat)))
