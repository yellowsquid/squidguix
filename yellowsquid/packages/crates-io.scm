(define-module (yellowsquid packages crates-io)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public rust-alsa-sys-0.3
  (package
    (name "rust-alsa-sys")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alsa-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/alsa-sys")
    (synopsis
      "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (description
      "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (license license:expat)))

(define-public rust-alsa-0.5
  (package
    (name "rust-alsa")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alsa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "03nmld6vbpxqg22fy07p51x2rmwl7bzsc7rszhd03gyknd5ldaqb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-alsa-sys" ,rust-alsa-sys-0.3)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-nix" ,rust-nix-0.21))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pyo3-macros-backend-0.13
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros-backend" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0rjxayd78l10hnyphk03bcvhm0jpsvnzn07lczhy7jsgv3jrgc47"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description "Code generation for PyO3 package")
    (license license:asl2.0)))

(define-public rust-pyo3-macros-0.13
  (package
    (name "rust-pyo3-macros")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1fxi5lx5dl7xh469gr5xckyjy3r3c5dqypzxcj0fbhzf1hq2qzx4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend-0.13)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description "Proc macros for PyO3 package")
    (license license:asl2.0)))

(define-public rust-inventory-impl-0.1
  (package
    (name "rust-inventory-impl")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory-impl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0j45a7nq4vircnz5m23db34527icxqnvh2si96zc9w662lvvahby"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/inventory")
    (synopsis "Implementation of macros for the `inventory` crate")
    (description "Implementation of macros for the `inventory` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-ghost-0.1
  (package
    (name "rust-ghost")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ghost" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0yalg3g1g3cz63n3phy7cdhh7p2qd220mrpxy96alwxbpqdwynqs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/ghost")
    (synopsis "Define your own PhantomData")
    (description "Define your own PhantomData")
    (license (list license:expat license:asl2.0))))

(define-public rust-inventory-0.1
  (package
    (name "rust-inventory")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m1zdjgs3nzfdxf86mc1i0id56fvk8rfw63rf04yi88bqrh53szh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ctor" ,rust-ctor-0.1)
         ("rust-ghost" ,rust-ghost-0.1)
         ("rust-inventory-impl" ,rust-inventory-impl-0.1))))
    (home-page "https://github.com/dtolnay/inventory")
    (synopsis "Typed distributed plugin registration")
    (description "Typed distributed plugin registration")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-impl-0.3
  (package
    (name "rust-indoc-impl")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc-impl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w58yg249kmzsn75kcj34qaxqh839l1hsaj3bzggy3q03wb6s16f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-unindent" ,rust-unindent-0.1))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-0.3
  (package
    (name "rust-indoc")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1n2fd2wm1h005hd7pjgx4gv5ymyq4sxqn8z0ssw6xchgqs5ilx27"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-indoc-impl" ,rust-indoc-impl-0.3)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-0.13
  (package
    (name "rust-pyo3")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hq965lgi25dn578fpn9hjva6zjr1c8rl7lxywijq44aw7lbhds8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-ctor" ,rust-ctor-0.1)
         ("rust-hashbrown" ,rust-hashbrown-0.9)
         ("rust-indoc" ,rust-indoc-0.3)
         ("rust-inventory" ,rust-inventory-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-num-bigint" ,rust-num-bigint-0.3)
         ("rust-num-complex" ,rust-num-complex-0.3)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-paste" ,rust-paste-0.1)
         ("rust-pyo3-macros" ,rust-pyo3-macros-0.13)
         ("rust-serde" ,rust-serde-1)
         ("rust-unindent" ,rust-unindent-0.1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description "Bindings to Python interpreter")
    (license license:asl2.0)))

(define-public rust-indenter-0.3
  (package
    (name "rust-indenter")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indenter" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/yaahc/indenter")
    (synopsis
      "A formatter wrapper that indents the text, designed for error display impls
")
    (description
      "This package provides a formatter wrapper that indents the text, designed for
error display impls")
    (license (list license:expat license:asl2.0))))

(define-public rust-eyre-0.6
  (package
    (name "rust-eyre")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "eyre" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0awxjsn1bwa43kwv1ycgn1qy9zs66syddjcidxfvz1pasp8kj4i2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-indenter" ,rust-indenter-0.3)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-pyo3" ,rust-pyo3-0.13))))
    (home-page "https://github.com/yaahc/eyre")
    (synopsis
      "Flexible concrete Error Reporting type built on std::error::Error with customizable Reports")
    (description
      "Flexible concrete Error Reporting type built on std::error::Error with
customizable Reports")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-error-0.1
  (package
    (name "rust-tracing-error")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-error" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "092y3357af6058mdw7nmr7sysqdka8b4cyaqz940fl2a7nwc1mxl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for enriching errors with `tracing`.
")
    (description "Utilities for enriching errors with `tracing`.")
    (license license:expat)))

(define-public rust-owo-colors-1
  (package
    (name "rust-owo-colors")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "owo-colors" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0rybl2lvhaycpkpaq45099idp5ny7nv4sqsafz0cvfqw1wjfy9vz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-atty" ,rust-atty-0.2))))
    (home-page "https://github.com/jam1garner/owo-colors")
    (synopsis "Zero-allocation terminal colors that'll make people go owo")
    (description "Zero-allocation terminal colors that'll make people go owo")
    (license license:expat)))

(define-public rust-color-spantrace-0.1
  (package
    (name "rust-color-spantrace")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "color-spantrace" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lb2li71zvpxp80nck98gcqbqm3dnmp43pnlvm52z9x8livy9vmn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-once-cell" ,rust-once-cell-1)
         ("rust-owo-colors" ,rust-owo-colors-1)
         ("rust-tracing-core" ,rust-tracing-core-0.1)
         ("rust-tracing-error" ,rust-tracing-error-0.1))))
    (home-page "https://github.com/yaahc/color-spantrace")
    (synopsis
      "A pretty printer for tracing_error::SpanTrace based on color-backtrace")
    (description
      "This package provides a pretty printer for tracing_error::SpanTrace based on
color-backtrace")
    (license (list license:expat license:asl2.0))))

(define-public rust-color-eyre-0.5
  (package
    (name "rust-color-eyre")
    (version "0.5.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "color-eyre" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1dspj58bk57f9hiqlvbz25rik92i4a95iwa2dl4pg8g8grlqa60z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-color-spantrace" ,rust-color-spantrace-0.1)
         ("rust-eyre" ,rust-eyre-0.6)
         ("rust-indenter" ,rust-indenter-0.3)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-owo-colors" ,rust-owo-colors-1)
         ("rust-tracing-error" ,rust-tracing-error-0.1)
         ("rust-url" ,rust-url-2))
        #:cargo-development-inputs
        (("rust-ansi-parser" ,rust-ansi-parser-0.6)
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

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libdbus-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-0.9.5
  (package
    (name "rust-dbus")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kn2v0w0q2q608xwl42c1yb3m2f7zvsm0f9ap1balb5k4mf782ny"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-executor" ,rust-futures-executor-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libdbus-sys" ,rust-libdbus-sys-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
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
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus-crossroads" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "18wcwqm1qp8yisxj62vn81fdbh07kq4vxba9823fcd4zv6zinn64"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-dbus" ,rust-dbus-0.9.5))))
    (home-page "https://github.com/diwic/dbus-rs/")
    (synopsis "Framework for writing D-Bus method handlers")
    (description "Framework for writing D-Bus method handlers")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libdbus-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))

#;(define-public rust-dbus-0.9
  (package
    (name "rust-dbus")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kn2v0w0q2q608xwl42c1yb3m2f7zvsm0f9ap1balb5k4mf782ny"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-executor" ,rust-futures-executor-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libdbus-sys" ,rust-libdbus-sys-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-crossroads-0.5
  (package
    (name "rust-dbus-crossroads")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus-crossroads" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0hc1j4vgy5k3lf8nqh5s0yk5hs50cmk2i7c5qkgd1izpg15krn55"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9))))
    (home-page "https://github.com/diwic/dbus-rs/")
    (synopsis "Framework for writing D-Bus method handlers")
    (description "Framework for writing D-Bus method handlers")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-tokio-0.7
  (package
    (name "rust-dbus-tokio")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus-tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "10112g227iasjiid7y9wrvnmxypfrczcymj2k5yjvcjk1i5ag88j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-dbus" ,rust-dbus-0.9)
         ("rust-dbus-crossroads" ,rust-dbus-crossroads-0.5)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-tokio" ,rust-tokio-1))
        #:cargo-development-inputs
        (("rust-dbus-tree" ,rust-dbus-tree-0.9)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
      "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
      "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux
for inter-process communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vorbisfile-sys-0.0.8
  (package
    (name "rust-vorbisfile-sys")
    (version "0.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbisfile-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1la2j2zbzdjd93byz21ij58c540bfn1r9pi0bssrjimcw7bhchsg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gcc" ,rust-gcc-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-vorbis-sys" ,rust-vorbis-sys-0.1))))
    (home-page "")
    (synopsis "FFI for the vorbisfile library")
    (description "FFI for the vorbisfile library")
    (license license:expat)))

(define-public rust-vorbis-sys-0.1
  (package
    (name "rust-vorbis-sys")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbis-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zgv7lwa4b2z091g25h83zil8bawk4frc1f0ril5xa31agpxd7mx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "FFI for the libvorbis library")
    (description "FFI for the libvorbis library")
    (license license:expat)))

(define-public rust-vorbis-0.0.14
  (package
    (name "rust-vorbis")
    (version "0.0.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbis" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0xn7diq8qz2zmsmwzg3rcsxmpmm2gj7wgnl2gdan0lq7ax21k2jy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-vorbis-sys" ,rust-vorbis-sys-0.1)
         ("rust-vorbisfile-sys" ,rust-vorbisfile-sys-0.0.8))))
    (home-page "https://github.com/tomaka/vorbis-rs")
    (synopsis "High-level bindings for the official libvorbis library.")
    (description "High-level bindings for the official libvorbis library.")
    (license license:asl2.0)))

(define-public rust-ogg-sys-0.0.9
  (package
    (name "rust-ogg-sys")
    (version "0.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ogg-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1cpx6n5ndh2d59g43l6rj3myzi5jsc0n6rldpx0impqp5qbqqnx9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gcc" ,rust-gcc-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/tomaka/ogg-sys")
    (synopsis "FFI for libogg, the media container.")
    (description "FFI for libogg, the media container.")
    (license license:expat)))

(define-public rust-librespot-tremor-0.2
  (package
    (name "rust-librespot-tremor")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-tremor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zmld16zawvn7ayrf318lwdr2d7awn4bk9s0d6kpim0mz6zjbxcp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "Rust bindings to tremor")
    (description "Rust bindings to tremor")
    (license license:expat)))

#;(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-stream" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qwq0y21xprsql4v9y1cm1ymhgw66rznjmnjrjsii27zxy25852h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.6))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-shannon-0.2
  (package
    (name "rust-shannon")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shannon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qa52zs4y1i87ysr11g9p6shpdagl14bb340gfm6rd97jhfb99by"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1))))
    (home-page "")
    (synopsis "Shannon cipher implementation")
    (description "Shannon cipher implementation")
    (license license:expat)))

(define-public rust-priority-queue-1
  (package
    (name "rust-priority-queue")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "priority-queue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w6a4wkxm7h7qhxqgivgxbixw51czmkd83x1vr0gqg4dq054ifh0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/garro95/priority-queue")
    (synopsis
      "A Priority Queue implemented as a heap with a function to efficiently change the priority of an item.")
    (description
      "This package provides a Priority Queue implemented as a heap with a function to
efficiently change the priority of an item.")
    (license (list license:lgpl3 license:mpl2.0))))

(define-public rust-password-hash-0.2
  (package
    (name "rust-password-hash")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "password-hash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1rr4kd52ld978a2xhcvlc54p1d92yhxl9kvbajba7ia6rs5b5q3p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1)
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

(define-public rust-base64ct-1
  (package
    (name "rust-base64ct")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64ct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1818i0gpg7q35s6yglmhgciwi3jwx65mqc2ipjl54jfbmm288kw7"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/base64ct")
    (synopsis
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (description
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pbkdf2-0.8
  (package
    (name "rust-pbkdf2")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ykgicvyjm41701mzqhrfmiz5sm5y0zwfg6csaapaqaf49a54pyr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1)
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

(define-public rust-protobuf-codegen-pure-2
  (package
    (name "rust-protobuf-codegen-pure")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf-codegen-pure" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0h34gfqlb7bqmgqv1mfgy5wk35z5r2h5ki3p3pdcmw1vqzmly6id"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-protobuf" ,rust-protobuf-2)
         ("rust-protobuf-codegen" ,rust-protobuf-codegen-2))))
    (home-page
      "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-codegen-pure/")
    (synopsis
      "Pure-rust codegen for protobuf using protobuf-parser crate

WIP
")
    (description
      "Pure-rust codegen for protobuf using protobuf-parser crate

WIP")
    (license license:expat)))

(define-public rust-protobuf-codegen-2
  (package
    (name "rust-protobuf-codegen")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf-codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "031bx325lsgcx7wc76vc2cqph6q0b34jgc8nz0g2rkwcfnx3n4fy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
      "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.
")
    (description
      "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.")
    (license license:expat)))

(define-public rust-protobuf-2
  (package
    (name "rust-protobuf")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11bl8hf522s9mbkckivnn9n8s3ss4g41w6jmfdsswmr5adqd71lf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))

(define-public rust-librespot-protocol-0.2
  (package
    (name "rust-librespot-protocol")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-protocol" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0pa3j2csmimf0l2yfdxp9ijmgm8ngxadim801jrh43xxqgj3nx8w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glob" ,rust-glob-0.3)
         ("rust-protobuf" ,rust-protobuf-2)
         ("rust-protobuf-codegen" ,rust-protobuf-codegen-2)
         ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "The protobuf logic for communicating with Spotify servers")
    (license license:expat)))

(define-public rust-hyper-proxy-0.9
  (package
    (name "rust-hyper-proxy")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-proxy" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1k3mpq6d4rhz58dam1757sav14j32n39q8x37wjgpz943f4mm0fa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
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

(define-public rust-librespot-core-0.2
  (package
    (name "rust-librespot-core")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0j1jjqf89dlsrlfjj9yy111dvlfi9zq3789ck0f57x9iw7f70gnz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes" ,rust-aes-0.6)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-form-urlencoded" ,rust-form-urlencoded-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
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
         ("rust-protobuf" ,rust-protobuf-2)
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

(define-public rust-ogg-0.8
  (package
    (name "rust-ogg")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ogg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0vjxmqcv9252aj8byy70iy2krqfjknfcxg11lcyikj11pzlb8lb9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-0.4)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "Ogg container decoder and encoder written in pure Rust")
    (license license:bsd-3)))

(define-public rust-lewton-0.10
  (package
    (name "rust-lewton")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lewton" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0c60fn004awg5c3cvx82d6na2pirf0qdz9w3b93mbcdakbglhyvp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-ogg" ,rust-ogg-0.8)
         ("rust-tinyvec" ,rust-tinyvec-1)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust vorbis decoder")
    (description "Pure Rust vorbis decoder")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-ctr-0.6
  (package
    (name "rust-aes-ctr")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes-ctr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qspjxzrclnb83501595y01dhc0km1ssrbjnwlxhcrsdwp6w6abp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes-soft" ,rust-aes-soft-0.6)
         ("rust-aesni" ,rust-aesni-0.10)
         ("rust-cipher" ,rust-cipher-0.2)
         ("rust-ctr" ,rust-ctr-0.6))))
    (home-page "https://github.com/RustCrypto/block-ciphers/tree/master/aes")
    (synopsis "DEPRECATED: replaced by the `aes` crate")
    (description "DEPRECATED: replaced by the `aes` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-audio-0.2
  (package
    (name "rust-librespot-audio")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-audio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1h25cinnyfw15farjv7lwcyqppd6gv0h5fkyac00dn06w79qj3ps"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-aes-ctr" ,rust-aes-ctr-0.6)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-lewton" ,rust-lewton-0.10)
         ("rust-librespot-core" ,rust-librespot-core-0.2)
         ("rust-librespot-tremor" ,rust-librespot-tremor-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-ogg" ,rust-ogg-0.8)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-vorbis" ,rust-vorbis-0.0.14)
         ("rust-zerocopy" ,rust-zerocopy-0.3))))
    (home-page "")
    (synopsis "The audio fetching and processing logic for librespot")
    (description "The audio fetching and processing logic for librespot")
    (license license:expat)))

(define-public rust-version-compare-0.0.10
  (package
    (name "rust-version-compare")
    (version "0.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version-compare" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "18ack6rx18rp700h1dncljmpzchs3p2dfh76a8ds6vmfbfi5cdfn"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/timvisee/version-compare")
    (synopsis
      "A Rust library to easily compare version numbers, and test them against various comparison operators.")
    (description
      "This package provides a Rust library to easily compare version numbers, and test
them against various comparison operators.")
    (license license:expat)))

(define-public rust-vcpkg-0.2
  (package
    (name "rust-vcpkg")
    (version "0.2.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/mcgoo/vcpkg-rs")
    (synopsis
      "A library to find native dependencies in a vcpkg tree at build
time in order to be used in Cargo build scripts.
")
    (description
      "This package provides a library to find native dependencies in a vcpkg tree at
build time in order to be used in Cargo build scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unidiff-0.3
  (package
    (name "rust-unidiff")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unidiff" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0b13vhp2x7jlvmkm44h5niqcxklyrmz6afmppvykp4zimhcjg9nq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-encoding-rs" ,rust-encoding-rs-0.8)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/messense/unidiff-rs")
    (synopsis "Unified diff parsing/metadata extraction library for Rust")
    (description "Unified diff parsing/metadata extraction library for Rust")
    (license license:expat)))

(define-public rust-sdl2-sys-0.34
  (package
    (name "rust-sdl2-sys")
    (version "0.34.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sdl2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0x4g141ais0k16frypykc1bfxlg5smraavg2lr0mlnqp3yi9m8j1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.53)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-cmake" ,rust-cmake-0.1)
         ("rust-flate2" ,rust-flate2-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-tar" ,rust-tar-0.4)
         ("rust-unidiff" ,rust-unidiff-0.3)
         ("rust-vcpkg" ,rust-vcpkg-0.2)
         ("rust-version-compare" ,rust-version-compare-0.0.10))))
    (home-page "https://github.com/rust-sdl2/rust-sdl2")
    (synopsis "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (description "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (license license:expat)))

(define-public rust-c-vec-2
  (package
    (name "rust-c-vec")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "c_vec" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1s765fviy10q27b0wmkyk4q728z9v8v5pdlxv5k564y0mlks9mzx"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/GuillaumeGomez/c_vec-rs.git")
    (synopsis "Structures to wrap C arrays")
    (description "Structures to wrap C arrays")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sdl2-0.34
  (package
    (name "rust-sdl2")
    (version "0.34.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sdl2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jymfs8ibf1xli4vn562l05bl6zknmff0qz5l7swy2j6m4zvrv6y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-c-vec" ,rust-c-vec-2)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
         ("rust-sdl2-sys" ,rust-sdl2-sys-0.34))))
    (home-page "https://github.com/Rust-SDL2/rust-sdl2")
    (synopsis "SDL2 bindings for Rust")
    (description "SDL2 bindings for Rust")
    (license license:expat)))

(define-public rust-slice-deque-0.3
  (package
    (name "rust-slice-deque")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slice-deque" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "098gvqjw52qw4gac567c9hx3y6hw9al7hjqb5mnvmvydh3i6xvri"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-mach" ,rust-mach-0.3)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/gnzlbg/slice_deque")
    (synopsis "A double-ended queue that Deref's into a slice.")
    (description
      "This package provides a double-ended queue that Deref's into a slice.")
    (license (list license:expat license:asl2.0))))

(define-public rust-minimp3-sys-0.3
  (package
    (name "rust-minimp3-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minimp3-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "144vmf3s89kad0smjprzigcp2c9r5dm95n4ydilrbp399irp6772"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-minimp3-0.5
  (package
    (name "rust-minimp3")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minimp3" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wj3nzj1swnvwsk3a4a3hkfj1d21jsi7babi40wlrxzbbzvkhm4q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-minimp3-sys" ,rust-minimp3-sys-0.3)
         ("rust-slice-deque" ,rust-slice-deque-0.3)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-hound-3
  (package
    (name "rust-hound")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hound" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jbm25p2nc8758dnfjan1yk7hz2i85y89nrbai14zzxfrsr4n5la"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/ruuda/hound")
    (synopsis "A wav encoding and decoding library")
    (description "This package provides a wav encoding and decoding library")
    (license license:asl2.0)))

(define-public rust-claxon-0.4
  (package
    (name "rust-claxon")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "claxon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1206mxvw833ysg10029apcsjjwly8zmsvksgza5cm7ma4ikzbysb"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/ruuda/claxon#readme")
    (synopsis "A FLAC decoding library")
    (description "This package provides a FLAC decoding library")
    (license license:asl2.0)))

(define-public rust-rodio-0.13
  (package
    (name "rust-rodio")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rodio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "03sjp37vb7ss1dwf76sf6yavmndqklx35shjpg8zd49ickd2wp5n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-claxon" ,rust-claxon-0.4)
         ("rust-cpal" ,rust-cpal-0.13)
         ("rust-hound" ,rust-hound-3)
         ("rust-lewton" ,rust-lewton-0.10)
         ("rust-minimp3" ,rust-minimp3-0.5))))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Audio playback library")
    (description "Audio playback library")
    (license (list license:expat license:asl2.0))))

(define-public rust-portaudio-sys-0.1
  (package
    (name "rust-portaudio-sys")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "portaudio-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1xdpywirpr1kqkbak7hnny62gmsc93qgc3ij3j2zskrvjpxa952i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "Bindings to PortAudio")
    (description "Bindings to PortAudio")
    (license license:expat)))

(define-public rust-portaudio-rs-0.3
  (package
    (name "rust-portaudio-rs")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "portaudio-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qnmc7amk0fzbcs985ixv0k4955f0fmpkhrl9ps9pk3cz7pvbdnd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-portaudio-sys" ,rust-portaudio-sys-0.1))))
    (home-page "")
    (synopsis "PortAudio bindings for Rust")
    (description "PortAudio bindings for Rust")
    (license license:expat)))

(define-public rust-librespot-metadata-0.2
  (package
    (name "rust-librespot-metadata")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-metadata" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ixg23ck98rjnshp6xil4lbig1sxpbg0blvyy6pdrnq1v7jpq5c4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-librespot-core" ,rust-librespot-core-0.2)
         ("rust-librespot-protocol" ,rust-librespot-protocol-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata logic for librespot")
    (description "The metadata logic for librespot")
    (license license:expat)))

(define-public rust-vorbisfile-sys-0.0.8
  (package
    (name "rust-vorbisfile-sys")
    (version "0.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbisfile-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1la2j2zbzdjd93byz21ij58c540bfn1r9pi0bssrjimcw7bhchsg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gcc" ,rust-gcc-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-vorbis-sys" ,rust-vorbis-sys-0.1))))
    (home-page "")
    (synopsis "FFI for the vorbisfile library")
    (description "FFI for the vorbisfile library")
    (license license:expat)))

(define-public rust-vorbis-sys-0.1
  (package
    (name "rust-vorbis-sys")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbis-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zgv7lwa4b2z091g25h83zil8bawk4frc1f0ril5xa31agpxd7mx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "FFI for the libvorbis library")
    (description "FFI for the libvorbis library")
    (license license:expat)))

(define-public rust-vorbis-0.0.14
  (package
    (name "rust-vorbis")
    (version "0.0.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vorbis" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0xn7diq8qz2zmsmwzg3rcsxmpmm2gj7wgnl2gdan0lq7ax21k2jy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-vorbis-sys" ,rust-vorbis-sys-0.1)
         ("rust-vorbisfile-sys" ,rust-vorbisfile-sys-0.0.8))))
    (home-page "https://github.com/tomaka/vorbis-rs")
    (synopsis "High-level bindings for the official libvorbis library.")
    (description "High-level bindings for the official libvorbis library.")
    (license license:asl2.0)))

(define-public rust-ogg-sys-0.0.9
  (package
    (name "rust-ogg-sys")
    (version "0.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ogg-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1cpx6n5ndh2d59g43l6rj3myzi5jsc0n6rldpx0impqp5qbqqnx9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-gcc" ,rust-gcc-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/tomaka/ogg-sys")
    (synopsis "FFI for libogg, the media container.")
    (description "FFI for libogg, the media container.")
    (license license:expat)))

(define-public rust-librespot-tremor-0.2
  (package
    (name "rust-librespot-tremor")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-tremor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zmld16zawvn7ayrf318lwdr2d7awn4bk9s0d6kpim0mz6zjbxcp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "Rust bindings to tremor")
    (description "Rust bindings to tremor")
    (license license:expat)))

(define-public rust-ogg-0.8
  (package
    (name "rust-ogg")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ogg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0vjxmqcv9252aj8byy70iy2krqfjknfcxg11lcyikj11pzlb8lb9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-0.4)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "Ogg container decoder and encoder written in pure Rust")
    (license license:bsd-3)))

(define-public rust-lewton-0.10
  (package
    (name "rust-lewton")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lewton" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0c60fn004awg5c3cvx82d6na2pirf0qdz9w3b93mbcdakbglhyvp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-ogg" ,rust-ogg-0.8)
         ("rust-tinyvec" ,rust-tinyvec-1)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust vorbis decoder")
    (description "Pure Rust vorbis decoder")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-audio-0.2
  (package
    (name "rust-librespot-audio")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-audio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1h25cinnyfw15farjv7lwcyqppd6gv0h5fkyac00dn06w79qj3ps"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes-ctr" ,rust-aes-ctr-0.6)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-lewton" ,rust-lewton-0.10)
         ("rust-librespot-core" ,rust-librespot-core-0.2)
         ("rust-librespot-tremor" ,rust-librespot-tremor-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-ogg" ,rust-ogg-0.8)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-vorbis" ,rust-vorbis-0.0.14)
         ("rust-zerocopy" ,rust-zerocopy-0.3))))
    (home-page "")
    (synopsis "The audio fetching and processing logic for librespot")
    (description "The audio fetching and processing logic for librespot")
    (license license:expat)))

(define-public rust-libpulse-simple-sys-1
  (package
    (name "rust-libpulse-simple-sys")
    (version "1.19.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libpulse-simple-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hbws8gj45lanvd0xr8d02m60n1247jcyq275ilhjj53kipzjwvw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libpulse-sys" ,rust-libpulse-sys-1)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis
      "FFI bindings for the PulseAudio libpulse-simple system library.")
    (description
      "FFI bindings for the PulseAudio libpulse-simple system library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pkg-config-0.3
  (package
    (name "rust-pkg-config")
    (version "0.3.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ghcyjp5537r7qigmgl3dj62j01arlpddaq93a3i414v3iskz2aq"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis
      "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.
")
    (description
      "This package provides a library to run the pkg-config system tool at build time
in order to be used in Cargo build scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-sys-1
  (package
    (name "rust-libpulse-sys")
    (version "1.19.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libpulse-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0c3wybzyyarzagz0fy2vwflc3r15d6cyfdp16ijnx8z2xz86n7lr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-num-derive" ,rust-num-derive-0.3)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "FFI bindings for the PulseAudio libpulse system library.")
    (description "FFI bindings for the PulseAudio libpulse system library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-binding-2
  (package
    (name "rust-libpulse-binding")
    (version "2.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libpulse-binding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qrnf088vwgxm26vs5fmz3ijry5nxjsdmgq37jcsxq0p00b45ghp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libpulse-sys" ,rust-libpulse-sys-1)
         ("rust-num-derive" ,rust-num-derive-0.3)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "A Rust language binding for the PulseAudio libpulse library.")
    (description
      "This package provides a Rust language binding for the PulseAudio libpulse
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-simple-binding-2
  (package
    (name "rust-libpulse-simple-binding")
    (version "2.25.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libpulse-simple-binding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mdws3gr5rvvb64hf77mkq17a6vzs6hryf8616v8r939zlfimgvw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libpulse-binding" ,rust-libpulse-binding-2)
         ("rust-libpulse-simple-sys" ,rust-libpulse-simple-sys-1)
         ("rust-libpulse-sys" ,rust-libpulse-sys-1))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis
      "A Rust language binding for the PulseAudio libpulse-simple library.")
    (description
      "This package provides a Rust language binding for the PulseAudio libpulse-simple
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-0.6
  (package
    (name "rust-jack")
    (version "0.6.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jack" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sk8rxiq3h2y33hdq15hnf915l8rv09zl9sgg2vjysvypms4ksrd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-jack-sys" ,rust-jack-sys-0.2)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK.")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-gstreamer-base-0.16
  (package
    (name "rust-gstreamer-base")
    (version "0.16.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-base" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kxgwvv0qh1dgvd08nxfhhkjp36z9fxrf3x1nps11jsrdz2h3zds"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
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
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-base-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1xgf5dl7507hn9mvz46ffjj3y2shpl1gc4l6w8d0l5kf5pfbddx4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glib-sys" ,rust-glib-sys-0.10)
         ("rust-gobject-sys" ,rust-gobject-sys-0.10)
         ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstbase-1.0")
    (description "FFI bindings to libgstbase-1.0")
    (license license:expat)))

(define-public rust-gstreamer-app-sys-0.9
  (package
    (name "rust-gstreamer-app-sys")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-app-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gbyp1jsqqs3x2bfjkbdfsb5kfb4zafwzvxr52w36ywybhkn8gw1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glib-sys" ,rust-glib-sys-0.10)
         ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.9)
         ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.9)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstapp-1.0")
    (description "FFI bindings to libgstapp-1.0")
    (license license:expat)))

(define-public rust-gstreamer-app-0.16
  (package
    (name "rust-gstreamer-app")
    (version "0.16.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-app" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "03mg4ywgba0q02zdqfamzxv7887bab2az32xhzg3x31kf618i06c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
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

(define-public rust-muldiv-0.2
  (package
    (name "rust-muldiv")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "muldiv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "014jlry2l2ph56mp8knw65637hh49q7fmrraim2bx9vz0a638684"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/sdroege/rust-muldiv")
    (synopsis
      "Provides a trait for numeric types to perform combined multiplication and
division with overflow protection
")
    (description
      "This package provides a trait for numeric types to perform combined
multiplication and division with overflow protection")
    (license license:expat)))

(define-public rust-gstreamer-sys-0.9
  (package
    (name "rust-gstreamer-sys")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "07b09f2acaiczjl3725dhraym935yns8x2jziiqza6nhh901a7zw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glib-sys" ,rust-glib-sys-0.10)
         ("rust-gobject-sys" ,rust-gobject-sys-0.10)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-system-deps" ,rust-system-deps-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstreamer-1.0")
    (description "FFI bindings to libgstreamer-1.0")
    (license license:expat)))

(define-public rust-gstreamer-rs-lgpl-docs-0.16
  (package
    (name "rust-gstreamer-rs-lgpl-docs")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer-rs-lgpl-docs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "06k4mr6478463q7hhsl4a252nhzf0b2qjqla3xhlh20ma0hz8912"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "LGPL-licensed docs for gstreamer-rs crates")
    (description "LGPL-licensed docs for gstreamer-rs crates")
    (license license:lgpl2.0)))

(define-public rust-gstreamer-0.16
  (package
    (name "rust-gstreamer")
    (version "0.16.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gstreamer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0crghv0qh0lys26712j3dshdwnvq2znnsyxldrzf72ihzzvx1xcz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
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

(define-public rust-stdweb-0.1
  (package
    (name "rust-stdweb")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gjk7ch31a3kgdc39kj4zqinf10yqaf717wanh9kwwbbwg430m7g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-clippy" ,rust-clippy-0.0)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "A standard library for the client-side Web")
    (description
      "This package provides a standard library for the client-side Web")
    (license (list license:expat license:asl2.0))))

(define-public rust-webpki-roots-0.22
  (package
    (name "rust-webpki-roots")
    (version "0.22.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki-roots" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jbll0ys9jakrvv3l1i216bbgj7jbxr7ad2dihw28xcm7s8fnb2m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla's CA root certificates for use with webpki")
    (description "Mozilla's CA root certificates for use with webpki")
    (license license:mpl2.0)))

(define-public rust-rustls-pemfile-0.2
  (package
    (name "rust-rustls-pemfile")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pemfile" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1jfi97lqnnnnxhmfy6ygrsp0x70m8wsdpaw45svvz1qc6vmymssy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-base64" ,rust-base64-0.13))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description "Basic .pem file parser for keys and certificates")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.6
  (package
    (name "rust-rustls-native-certs")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-native-certs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0hq9h3kri19kv00gvbq61h21rarqadxh6y98wj0c2gvxlbgypaaw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-openssl-probe" ,rust-openssl-probe-0.1)
         ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
         ("rust-schannel" ,rust-schannel-0.1)
         ("rust-security-framework" ,rust-security-framework-2))))
    (home-page "https://github.com/ctz/rustls-native-certs")
    (synopsis
      "rustls-native-certs allows rustls to use the platform native certificate store")
    (description
      "rustls-native-certs allows rustls to use the platform native certificate store")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-webpki-0.22
  (package
    (name "rust-webpki")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1gd1gxip5kgdwmrvhj5gjxij2mgg2mavq1ych4q1h272ja0xg5gh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ring" ,rust-ring-0.16)
         ("rust-untrusted" ,rust-untrusted-0.7))))
    (home-page "https://github.com/briansmith/webpki")
    (synopsis "Web PKI X.509 Certificate Verification.")
    (description "Web PKI X.509 Certificate Verification.")
    (license (list (license:non-copyleft "file://LICENSE")
                   (license:non-copyleft "file://third-party/chromium/LICENSE")))))

(define-public rust-sct-0.7
  (package
    (name "rust-sct")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "193w3dg2pcn7138ab4c586pl76nkryn4h6wqlwvqj5gqr6vwsgfm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ring" ,rust-ring-0.16)
         ("rust-untrusted" ,rust-untrusted-0.7))))
    (home-page "https://github.com/ctz/sct.rs")
    (synopsis "Certificate transparency SCT verification library")
    (description "Certificate transparency SCT verification library")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-0.20
  (package
    (name "rust-rustls")
    (version "0.20.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "111z24faq1i1z3gbznfgl7qp3in2fx1y1adijjsl1q7kj0i5wznk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-ring" ,rust-ring-0.16)
         ("rust-sct" ,rust-sct-0.7)
         ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust.")
    (description "Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.0.72")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vl50h2qh0nh0iddzj6gd1pnxnxpvwmbfxc30578c1pajmxi7a92"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-jobserver" ,rust-jobserver-0.1))))
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis
      "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
    (description
      "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cloudflare-zlib-sys-0.3
  (package
    (name "rust-cloudflare-zlib-sys")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudflare-zlib-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0vyd0l0vprvh9hc1ikllybrk8xc0lz9f509d2xgxgrpyxp8vch10"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/cloudflare/zlib")
    (synopsis "Cloudflare fork of zlib with massive performance improvements")
    (description
      "Cloudflare fork of zlib with massive performance improvements")
    (license (list license:expat license:zlib license:asl2.0 license:zlib))))

(define-public rust-flate2-1
  (package
    (name "rust-flate2")
    (version "1.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gy5iwfqylb2f0dd9n7r8w2xwbzlrqlsairvyj2w9jf1jzl8hs8y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-cloudflare-zlib-sys" ,rust-cloudflare-zlib-sys-0.3)
         ("rust-crc32fast" ,rust-crc32fast-1)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libz-sys" ,rust-libz-sys-1)
         ("rust-miniz-sys" ,rust-miniz-sys-0.1)
         ("rust-miniz-oxide" ,rust-miniz-oxide-0.4)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis
      "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.
Supports miniz_oxide, miniz.c, and multiple zlib implementations. Supports
zlib, gzip, and raw deflate streams.
")
    (description
      "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.
Supports miniz_oxide, miniz.c, and multiple zlib implementations.  Supports
zlib, gzip, and raw deflate streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros-0.1
  (package
    (name "rust-time-macros")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wg24yxpxcfmim6dgblrf8p321m7cyxpdivzvp8bcb7i4rp9qzlm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
         ("rust-time-macros-impl" ,rust-time-macros-impl-0.1))))
    (home-page "https://github.com/time-rs/time")
    (synopsis "Procedural macros for the time crate.")
    (description "Procedural macros for the time crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-standback-0.2
  (package
    (name "rust-standback")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "standback" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zr8zy3kzryaggz3k0j4135m3zbd31pyqmja8cyj8yp07mpzn4z1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/jhpratt/standback")
    (synopsis "New standard library, old compiler.")
    (description "New standard library, old compiler.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.2
  (package
    (name "rust-time")
    (version "0.2.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0hm209d078jfgxzjhi5xqim64q31rlj1h70zz57qbmpbirzsjlj7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-const-fn" ,rust-const-fn-0.4)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-serde" ,rust-serde-1)
         ("rust-standback" ,rust-standback-0.2)
         ("rust-stdweb" ,rust-stdweb-0.4)
         ("rust-time-macros" ,rust-time-macros-0.1)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://time-rs.github.io")
    (synopsis
      "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-normalization-0.1
  (package
    (name "rust-unicode-normalization")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-normalization" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1yabhmg8zlcksda3ajly9hpbzqgbhknxwch8dwkfkaa1569r0ifm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "https://github.com/unicode-rs/unicode-normalization")
    (synopsis
      "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.
")
    (description
      "This crate provides functions for normalization of Unicode strings, including
Canonical and Compatible Decomposition and Recomposition, as described in
Unicode Standard Annex #15.")
    (license (list license:expat license:asl2.0))))

(define-public rust-idna-0.2
  (package
    (name "rust-idna")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1y7ca2w5qp9msgl57n03zqp78gq1bk2crqzg6kv7a542mdphm2j1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-matches" ,rust-matches-0.1)
         ("rust-unicode-bidi" ,rust-unicode-bidi-0.3)
         ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "https://github.com/servo/rust-url/")
    (synopsis
      "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cookie-store-0.15
  (package
    (name "rust-cookie-store")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0z0navy9k0ivrdvz492q8c4nhd3iv5l77hwfppskdp1j15607xxk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cookie" ,rust-cookie-0.15)
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

(define-public rust-subtle-2
  (package
    (name "rust-subtle")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "subtle" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00b6jzh9gzb0h9n25g06nqr90z3xzqppfhhb260s1hjhh4pg7pkb"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://dalek.rs/")
    (synopsis
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-hkdf-0.10
  (package
    (name "rust-hkdf")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0kwn3scjvv2x8zc6nz3wrnzxp9shpsdxnjqiyv2r65r3kiijzasi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-digest" ,rust-digest-0.9) ("rust-hmac" ,rust-hmac-0.10))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-gcm-0.8
  (package
    (name "rust-aes-gcm")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes-gcm" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1nl8iwlh209y1vj9n2lm1a70i69clvg2z6x69bi4dgdrpgxbay2j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aead" ,rust-aead-0.3)
         ("rust-aes" ,rust-aes-0.6)
         ("rust-cipher" ,rust-cipher-0.2)
         ("rust-ctr" ,rust-ctr-0.6)
         ("rust-ghash" ,rust-ghash-0.3)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis
      "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)
Authenticated Encryption with Associated Data (AEAD) Cipher
with optional architecture-specific hardware acceleration
")
    (description
      "Pure Rust implementation of the AES-GCM (Galois/Counter Mode) Authenticated
Encryption with Associated Data (AEAD) Cipher with optional
architecture-specific hardware acceleration")
    (license (list license:asl2.0 license:expat))))

(define-public rust-cookie-0.15
  (package
    (name "rust-cookie")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "03gql9c2l0wg3hpfp67wg2ns21wysk0xsjxwdbjrf0s6grrcgwfm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes-gcm" ,rust-aes-gcm-0.8)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-hkdf" ,rust-hkdf-0.10)
         ("rust-hmac" ,rust-hmac-0.10)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-time" ,rust-time-0.2)
         ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/SergioBenitez/cookie-rs")
    (synopsis
      "HTTP cookie parsing and cookie jar management. Supports signed and private
(encrypted, authenticated) jars.
")
    (description
      "HTTP cookie parsing and cookie jar management.  Supports signed and private
(encrypted, authenticated) jars.")
    (license (list license:expat license:asl2.0))))

(define-public rust-brotli-decompressor-2
  (package
    (name "rust-brotli-decompressor")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli-decompressor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "102v89h9z0p45j5fsjna97761nxx9nxz9ccpwxma6p5zad32vbar"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
         ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis
      "A brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib's allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli ")
    (description
      "This package provides a brotli decompressor that with an interface avoiding the
rust stdlib.  This makes it suitable for embedded devices and kernels.  It is
designed with a pluggable allocator so that the standard lib's allocator may be
employed.  The default build also includes a stdlib allocator and stream
interface.  Disable this with --features=no-stdlib.  Alternatively,
--features=unsafe turns off array bounds checks and memory initialization but
provides a safe interface for the caller.  Without adding the --features=unsafe
argument, all included code is safe.  For compression in addition to this
library, download https://github.com/dropbox/rust-brotli ")
    (license (list license:bsd-3 license:expat))))

(define-public rust-ureq-2
  (package
    (name "rust-ureq")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ureq" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m8nzx683iph4zfpfg2xmkkbwmgf1i403lnbhxqk4gbsj8pzm6ck"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-brotli-decompressor" ,rust-brotli-decompressor-2)
         ("rust-chunked-transfer" ,rust-chunked-transfer-1)
         ("rust-cookie" ,rust-cookie-0.15)
         ("rust-cookie-store" ,rust-cookie-store-0.15)
         ("rust-encoding-rs" ,rust-encoding-rs-0.8)
         ("rust-flate2" ,rust-flate2-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-rustls" ,rust-rustls-0.20)
         ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-socks" ,rust-socks-0.3)
         ("rust-url" ,rust-url-2)
         ("rust-webpki" ,rust-webpki-0.22)
         ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/algesten/ureq")
    (synopsis "Simple, safe HTTP client")
    (description "Simple, safe HTTP client")
    (license (list license:expat license:asl2.0))))

(define-public rust-fetch-unroll-0.3
  (package
    (name "rust-fetch-unroll")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fetch_unroll" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1l3cf8fhcrw354hdmjf03f5v4bxgn2wkjna8n0fn8bgplh8b3666"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libflate" ,rust-libflate-1)
         ("rust-tar" ,rust-tar-0.4)
         ("rust-ureq" ,rust-ureq-2))))
    (home-page "https://github.com/katyo/fetch_unroll")
    (synopsis "Simple utilities for fetching and unrolling .tar.gz archives")
    (description
      "Simple utilities for fetching and unrolling .tar.gz archives")
    (license license:asl2.0)))

(define-public rust-oboe-sys-0.4
  (package
    (name "rust-oboe-sys")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "oboe-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1gcl494yy880h2gfgsbdd32g2h0s1n94v58j5hil9mrf6yvsnw1k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.59)
         ("rust-cc" ,rust-cc-1)
         ("rust-fetch-unroll" ,rust-fetch-unroll-0.3))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
      "Unsafe bindings for oboe an android library for low latency audio IO")
    (description
      "Unsafe bindings for oboe an android library for low latency audio IO")
    (license license:asl2.0)))

(define-public rust-darling-macro-0.13
  (package
    (name "rust-darling-macro")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jzljnd0y7idi5lb7lhvymh3nkhaf32ksx0d38hv7zjjfcxipi3j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-darling-core" ,rust-darling-core-0.13)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
      "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
      "Internal support for a proc-macro library for reading attributes into structs
when implementing custom derives.  Use https://crates.io/crates/darling in your
code.")
    (license license:expat)))

(define-public rust-fnv-1
  (package
    (name "rust-fnv")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
    (description "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
    (license (list license:asl2.0 license:expat))))

(define-public rust-darling-core-0.13
  (package
    (name "rust-darling-core")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0933k2avb6xk9j4ryr0bvp3pww5j8i0nrqvsnkgd3vic3lj0yd3s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-fnv" ,rust-fnv-1)
         ("rust-ident-case" ,rust-ident-case-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-strsim" ,rust-strsim-0.10)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
      "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
    (description
      "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.")
    (license license:expat)))

(define-public rust-darling-0.13
  (package
    (name "rust-darling")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1x7pgzjswg28798zd5gk5g6nifhcaqq0apqmclydi39zd2w21myh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-darling-core" ,rust-darling-core-0.13)
         ("rust-darling-macro" ,rust-darling-macro-0.13))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
      "A proc-macro library for reading attributes into structs when
implementing custom derives.
")
    (description
      "This package provides a proc-macro library for reading attributes into structs
when implementing custom derives.")
    (license license:expat)))

(define-public rust-ndk-macro-0.3
  (package
    (name "rust-ndk-macro")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0v3sxc11kq3d5vdwfml62l7y5dr0flsf6kp5xid9sbv7qh0arxqd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-darling" ,rust-darling-0.13)
         ("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Helper macros for android ndk")
    (description "Helper macros for android ndk")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-logger-0.10
  (package
    (name "rust-android-logger")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android_logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0rigzgkaik2y7pvsilpjdy19mdq1kkamw2rdf9fjkvb5hfqhkvfr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-android-log-sys" ,rust-android-log-sys-0.2)
         ("rust-env-logger" ,rust-env-logger-0.8)
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

(define-public rust-ndk-glue-0.6
  (package
    (name "rust-ndk-glue")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk-glue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0yaxwp953c0b5fpn8zalwjkrvw8rkf00physlmi9kssq115x3h04"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-android-logger" ,rust-android-logger-0.10)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-ndk" ,rust-ndk-0.6)
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
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "15zsq4p6k5asf4mc0rknd8cz9wxrwvi50qdspgf87qcfgkknlnkf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-jni-sys" ,rust-jni-sys-0.3))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "FFI bindings for the Android NDK")
    (description "FFI bindings for the Android NDK")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndk-0.6
  (package
    (name "rust-ndk")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m1dfjw35qpys1hr4qib6mm3zacd01k439l7cx5f7phd0dzcfci0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
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

(define-public rust-oboe-0.4
  (package
    (name "rust-oboe")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "oboe" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lh20l8b4lx5h9a7lpf9n66z47sh2508w7x2203hsklvw7rchqr4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-jni" ,rust-jni-0.19)
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

(define-public rust-android-log-sys-0.2
  (package
    (name "rust-android-log-sys")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android_log-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0bhhs1cgzp9vzjvkn2q31ppc7w4am5s273hkvl5iac5475kmp5l5"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/nercury/android_log-sys-rs")
    (synopsis "FFI bindings to Android log Library.
")
    (description "FFI bindings to Android log Library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-logger-0.9
  (package
    (name "rust-android-logger")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android_logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wspwzkn3fakpyz3ka0lh6h4pa66zk9kkvic2q9n70jx30y37hif"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-android-log-sys" ,rust-android-log-sys-0.2)
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

(define-public rust-ndk-glue-0.3
  (package
    (name "rust-ndk-glue")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk-glue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11cksaj1f2sy4dwqyssrvvhbnd86zajfvm17bj81rb2i9p1g1jn5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-android-logger" ,rust-android-logger-0.9)
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

(define-public rust-num-enum-derive-0.5
  (package
    (name "rust-num-enum-derive")
    (version "0.5.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_enum_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08dvxpa7l5hx5fcdr0bdv9bzajbcbxsbbnc6hl6zxmwhhiv2p68d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/illicitonion/num_enum")
    (synopsis
      "Internal implementation details for ::num_enum (Procedural macros to make inter-operation between primitives and enums easier)")
    (description
      "Internal implementation details for ::num_enum (Procedural macros to make
inter-operation between primitives and enums easier)")
    (license (list license:bsd-3 license:expat license:asl2.0))))

(define-public rust-num-enum-0.5
  (package
    (name "rust-num-enum")
    (version "0.5.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_enum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1b96nmbhn2gadlh4hna6mz6w892gzp1zic60q1s4akjy0nhkw3bj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-enum-derive" ,rust-num-enum-derive-0.5))))
    (home-page "https://github.com/illicitonion/num_enum")
    (synopsis
      "Procedural macros to make inter-operation between primitives and enums easier.")
    (description
      "Procedural macros to make inter-operation between primitives and enums easier.")
    (license (list license:bsd-3 license:expat license:asl2.0))))

(define-public rust-ndk-0.3
  (package
    (name "rust-ndk")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndk" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1avk39s8w21inkzq09x83yghgq3v8rmhrycba8prg6rif8hk5547"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-jni" ,rust-jni-0.14)
         ("rust-jni-glue" ,rust-jni-glue-0.0)
         ("rust-jni-sys" ,rust-jni-sys-0.3)
         ("rust-ndk-sys" ,rust-ndk-sys-0.2)
         ("rust-num-enum" ,rust-num-enum-0.5)
         ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/rust-windowing/android-ndk-rs")
    (synopsis "Safe Rust bindings to the Android NDK")
    (description "Safe Rust bindings to the Android NDK")
    (license (list license:expat license:asl2.0))))

(define-public rust-jni-0.19
  (package
    (name "rust-jni")
    (version "0.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jni" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1v0pn0i1wb8zp4wns4l8hz9689hqsplv7iba7hylaznvwg11ipy6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cesu8" ,rust-cesu8-1)
         ("rust-combine" ,rust-combine-4)
         ("rust-jni-sys" ,rust-jni-sys-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/jni-rs/jni-rs")
    (synopsis "Rust bindings to the JNI")
    (description "Rust bindings to the JNI")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-sys-0.2
  (package
    (name "rust-jack-sys")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jack-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1h9c9za19nyr1prx77gkia18ia93f73lpyjdiyrvmhhbs79g54bv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libloading" ,rust-libloading-0.6)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API.")
    (description "Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-0.7
  (package
    (name "rust-jack")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jack" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1r7bgfpbph3fl9xyp4i9qffcc4h923dcs7d967mpir13lxg216yp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-jack-sys" ,rust-jack-sys-0.2)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK.")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-bindgen-0.56
  (package
    (name "rust-bindgen")
    (version "0.56.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fajmgk2064ca1z9iq1jjkji63qwwz38z3d67kv6xdy0xgdpk8rd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-cexpr" ,rust-cexpr-0.4)
         ("rust-clang-sys" ,rust-clang-sys-1)
         ("rust-clap" ,rust-clap-2)
         ("rust-env-logger" ,rust-env-logger-0.8)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-lazycell" ,rust-lazycell-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-regex" ,rust-regex-1)
         ("rust-rustc-hash" ,rust-rustc-hash-1)
         ("rust-shlex" ,rust-shlex-0.1)
         ("rust-which" ,rust-which-3))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-coreaudio-sys-0.2
  (package
    (name "rust-coreaudio-sys")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "coreaudio-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1yiipfq8gni2fkh62kzzprqgnfb77046d392p9mb82bapr3k6zib"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.56))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
      "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
      "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (license license:expat)))

(define-public rust-coreaudio-rs-0.10
  (package
    (name "rust-coreaudio-rs")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "coreaudio-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "125d4zr3n363ybga4629p41ym7iqjfb2alnwrc1zj7zyxch4p28i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "A friendly rust interface for Apple's CoreAudio API.")
    (description
      "This package provides a friendly rust interface for Apple's CoreAudio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-asio-sys-0.2
  (package
    (name "rust-asio-sys")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "asio-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m8v2jsa4n57s7a7142vs23dkz63dhjxgcjxykd17kvq66v9qqj7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.54)
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

(define-public rust-cpal-0.13
  (package
    (name "rust-cpal")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0614gbjg29n817m97595974pbw331dqyz2c8g0ncs5zn4455zx4q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-alsa" ,rust-alsa-0.5)
         ("rust-asio-sys" ,rust-asio-sys-0.2)
         ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
         ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.10)
         ("rust-jack" ,rust-jack-0.7)
         ("rust-jni" ,rust-jni-0.19)
         ("rust-js-sys" ,rust-js-sys-0.3)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-mach" ,rust-mach-0.3)
         ("rust-ndk" ,rust-ndk-0.3)
         ("rust-ndk-glue" ,rust-ndk-glue-0.3)
         ("rust-nix" ,rust-nix-0.20)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-oboe" ,rust-oboe-0.4)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-stdweb" ,rust-stdweb-0.1)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-web-sys" ,rust-web-sys-0.3)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust.")
    (description "Low-level cross-platform audio I/O library in pure Rust.")
    (license license:asl2.0)))

(define-public rust-alsa-sys-0.3
  (package
    (name "rust-alsa-sys")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alsa-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/alsa-sys")
    (synopsis
      "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (description
      "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (license license:expat)))

(define-public rust-alsa-0.5
  (package
    (name "rust-alsa")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alsa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "03nmld6vbpxqg22fy07p51x2rmwl7bzsc7rszhd03gyknd5ldaqb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-alsa-sys" ,rust-alsa-sys-0.3)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-nix" ,rust-nix-0.21))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-librespot-playback-0.2
  (package
    (name "rust-librespot-playback")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-playback" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mksjvbwq7y01x54i7aqdbl7g48m7s060xki10qa9qd7hvwkvjjr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-alsa" ,rust-alsa-0.5)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-cpal" ,rust-cpal-0.13)
         ("rust-futures-executor" ,rust-futures-executor-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
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

#;(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-stream" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qwq0y21xprsql4v9y1cm1ymhgw66rznjmnjrjsii27zxy25852h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.6))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-shannon-0.2
  (package
    (name "rust-shannon")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shannon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qa52zs4y1i87ysr11g9p6shpdagl14bb340gfm6rd97jhfb99by"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1))))
    (home-page "")
    (synopsis "Shannon cipher implementation")
    (description "Shannon cipher implementation")
    (license license:expat)))

(define-public rust-priority-queue-1
  (package
    (name "rust-priority-queue")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "priority-queue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w6a4wkxm7h7qhxqgivgxbixw51czmkd83x1vr0gqg4dq054ifh0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/garro95/priority-queue")
    (synopsis
      "A Priority Queue implemented as a heap with a function to efficiently change the priority of an item.")
    (description
      "This package provides a Priority Queue implemented as a heap with a function to
efficiently change the priority of an item.")
    (license (list license:lgpl3 license:mpl2.0))))

(define-public rust-password-hash-0.2
  (package
    (name "rust-password-hash")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "password-hash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1rr4kd52ld978a2xhcvlc54p1d92yhxl9kvbajba7ia6rs5b5q3p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1)
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

(define-public rust-base64ct-1
  (package
    (name "rust-base64ct")
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64ct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1818i0gpg7q35s6yglmhgciwi3jwx65mqc2ipjl54jfbmm288kw7"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/base64ct")
    (synopsis
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support
")
    (description
      "Pure Rust implementation of Base64 (RFC 4648) which avoids any usages of
data-dependent branches/LUTs and thereby provides portable \"best effort\"
constant-time operation and embedded-friendly no_std support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pbkdf2-0.8
  (package
    (name "rust-pbkdf2")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ykgicvyjm41701mzqhrfmiz5sm5y0zwfg6csaapaqaf49a54pyr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1)
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

(define-public rust-protobuf-codegen-pure-2
  (package
    (name "rust-protobuf-codegen-pure")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf-codegen-pure" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0h34gfqlb7bqmgqv1mfgy5wk35z5r2h5ki3p3pdcmw1vqzmly6id"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-protobuf" ,rust-protobuf-2)
         ("rust-protobuf-codegen" ,rust-protobuf-codegen-2))))
    (home-page
      "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-codegen-pure/")
    (synopsis
      "Pure-rust codegen for protobuf using protobuf-parser crate

WIP
")
    (description
      "Pure-rust codegen for protobuf using protobuf-parser crate

WIP")
    (license license:expat)))

(define-public rust-protobuf-codegen-2
  (package
    (name "rust-protobuf-codegen")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf-codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "031bx325lsgcx7wc76vc2cqph6q0b34jgc8nz0g2rkwcfnx3n4fy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
      "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.
")
    (description
      "Code generator for rust-protobuf.

Includes a library and `protoc-gen-rust` binary.

See `protoc-rust` and `protobuf-codegen-pure` crates.")
    (license license:expat)))

(define-public rust-protobuf-2
  (package
    (name "rust-protobuf")
    (version "2.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "protobuf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11bl8hf522s9mbkckivnn9n8s3ss4g41w6jmfdsswmr5adqd71lf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))

(define-public rust-librespot-protocol-0.2
  (package
    (name "rust-librespot-protocol")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-protocol" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0pa3j2csmimf0l2yfdxp9ijmgm8ngxadim801jrh43xxqgj3nx8w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glob" ,rust-glob-0.3)
         ("rust-protobuf" ,rust-protobuf-2)
         ("rust-protobuf-codegen" ,rust-protobuf-codegen-2)
         ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "The protobuf logic for communicating with Spotify servers")
    (license license:expat)))

(define-public rust-hyper-proxy-0.9
  (package
    (name "rust-hyper-proxy")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-proxy" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1k3mpq6d4rhz58dam1757sav14j32n39q8x37wjgpz943f4mm0fa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
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

(define-public rust-librespot-core-0.2
  (package
    (name "rust-librespot-core")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0j1jjqf89dlsrlfjj9yy111dvlfi9zq3789ck0f57x9iw7f70gnz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes" ,rust-aes-0.6)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-form-urlencoded" ,rust-form-urlencoded-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
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
         ("rust-protobuf" ,rust-protobuf-2)
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

(define-public rust-if-addrs-sys-0.3
  (package
    (name "rust-if-addrs-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "if-addrs-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1skrzs79rafv185064p44r0k1va9ig4bfnpbwlvyhxh4g3fvjx6y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1) ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "if_addrs sys crate")
    (description "if_addrs sys crate")
    (license (list license:expat license:bsd-3))))

(define-public rust-if-addrs-0.6
  (package
    (name "rust-if-addrs")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "if-addrs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1pkkkwm9znn07xq9s6glf8lxzn2rdxvy8kwkw6czrw64ywhy8wr2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-if-addrs-sys" ,rust-if-addrs-sys-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "Return interface IP addresses on Posix and windows systems")
    (description "Return interface IP addresses on Posix and windows systems")
    (license (list license:expat license:bsd-3))))

(define-public rust-libmdns-0.6
  (package
    (name "rust-libmdns")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libmdns" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0siaarjhds4dxrxn7qgz4gnfb2hzcmsdcndd3rnkr1rfs2j8bhgs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-hostname" ,rust-hostname-0.3)
         ("rust-if-addrs" ,rust-if-addrs-0.6)
         ("rust-log" ,rust-log-0.4)
         ("rust-multimap" ,rust-multimap-0.8)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-socket2" ,rust-socket2-0.4)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/libmdns")
    (synopsis
      "mDNS Responder library for building discoverable LAN services in Rust")
    (description
      "mDNS Responder library for building discoverable LAN services in Rust")
    (license license:expat)))

(define-public rust-dns-sd-0.1
  (package
    (name "rust-dns-sd")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dns-sd" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11r0jymjshfnn3sh2nqjhrikk4r5rr1g36sip9iqy8i0xafm0j6p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/plietar/rust-dns-sd")
    (synopsis "Rust binding for dns-sd")
    (description "Rust binding for dns-sd")
    (license license:expat)))

(define-public rust-aes-ctr-0.6
  (package
    (name "rust-aes-ctr")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes-ctr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0qspjxzrclnb83501595y01dhc0km1ssrbjnwlxhcrsdwp6w6abp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes-soft" ,rust-aes-soft-0.6)
         ("rust-aesni" ,rust-aesni-0.10)
         ("rust-cipher" ,rust-cipher-0.2)
         ("rust-ctr" ,rust-ctr-0.6))))
    (home-page "https://github.com/RustCrypto/block-ciphers/tree/master/aes")
    (synopsis "DEPRECATED: replaced by the `aes` crate")
    (description "DEPRECATED: replaced by the `aes` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-connect-0.2
  (package
    (name "rust-librespot-connect")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "librespot-connect" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1272nnzk9xhyc2jvakfrl3451pql65mnxcc6xkqpjiypr3b5yhk9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-aes-ctr" ,rust-aes-ctr-0.6)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-dns-sd" ,rust-dns-sd-0.1)
         ("rust-form-urlencoded" ,rust-form-urlencoded-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-hyper" ,rust-hyper-0.14)
         ("rust-libmdns" ,rust-libmdns-0.6)
         ("rust-librespot-core" ,rust-librespot-core-0.2)
         ("rust-librespot-playback" ,rust-librespot-playback-0.2)
         ("rust-librespot-protocol" ,rust-librespot-protocol-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-protobuf" ,rust-protobuf-2)
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

(define-public rust-rspotify-0.8
  (package
    (name "rust-rspotify")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rspotify" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "14xdic0zhalmvk32y1ffanvgwdqki91qw549kj6mqcdirxka2959"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64-0.10)
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

(define-public rust-tokio-timer-0.2
  (package
    (name "rust-tokio-timer")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-timer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "15pjjj6daks3sii8p24a509b0dapl2kyk740nwfgz59w64nly14k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Timer facilities for Tokio
")
    (description "Timer facilities for Tokio")
    (license license:expat)))

(define-public rust-tokio-reactor-0.1
  (package
    (name "rust-tokio-reactor")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-reactor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0l8klnd41q55f3ialzz0lb7s5bfwa38nh86sa9vai2xsqh75kg09"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-mio" ,rust-mio-0.6)
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

(define-public rust-tokio-executor-0.1
  (package
    (name "rust-tokio-executor")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-executor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0w8n78d2vixs1vghqc4wy9w0d1h6qkli51c1yzhzbns88n7inbgv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Future execution primitives
")
    (description "Future execution primitives")
    (license license:expat)))

(define-public rust-tokio-compat-0.1
  (package
    (name "rust-tokio-compat")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-compat" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08n7lkf5l2drb7hph1r3s6jj9cbarbbcr69dvnbr4yxa6m8n4yqh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1)
         ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
         ("rust-tokio-timer" ,rust-tokio-timer-0.2))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Compatibility between `tokio` 0.2 and legacy versions.
")
    (description "Compatibility between `tokio` 0.2 and legacy versions.")
    (license license:expat)))

(define-public rust-tokio-macros-0.2
  (package
    (name "rust-tokio-macros")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ni60vnrf32r3wfhlahmnds1phx5d1xfbmyq9j0mz8kkzh5s0kg4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio's proc macros.
")
    (description "Tokio's proc macros.")
    (license license:expat)))

(define-public rust-net2-0.2
  (package
    (name "rust-net2")
    (version "0.2.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "net2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1bk8jp0i12gvhrlaqbfq19ancja70r1rg3sywbhjl0385g8k05ir"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/deprecrated/net2-rs")
    (synopsis
      "Extensions to the standard library's networking types as proposed in RFC 1158.
")
    (description
      "Extensions to the standard library's networking types as proposed in RFC 1158.")
    (license (list license:expat license:asl2.0))))

(define-public rust-miow-0.2
  (package
    (name "rust-miow")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0kcl8rnv0bhiarcdakik670w8fnxzlxhi1ys7152sck68510in7b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
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

(define-public rust-mio-0.6
  (package
    (name "rust-mio")
    (version "0.6.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1i2c1vl8lr45apkh8xbh9k56ihfsmqff5l7s2fya7whvp7sndzaa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-fuchsia-zircon" ,rust-fuchsia-zircon-0.3)
         ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys-0.3)
         ("rust-iovec" ,rust-iovec-0.1)
         ("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-miow" ,rust-miow-0.2)
         ("rust-net2" ,rust-net2-0.2)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO")
    (license license:expat)))

(define-public rust-tokio-0.2
  (package
    (name "rust-tokio")
    (version "0.2.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "14l0rll6y1dyzh6qcd8rma2ch3wx0dxzxq8b54di744sjirs40v7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-0.5)
         ("rust-fnv" ,rust-fnv-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-iovec" ,rust-iovec-0.1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mio" ,rust-mio-0.6)
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

(define-public rust-tokio-compat-02-0.2
  (package
    (name "rust-tokio-compat-02")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-compat-02" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01vyvimvyip9m779a3v5nvf3bm7sc9s949vs1bzqzgmp49w27m77"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes-0.5)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1))
        #:cargo-development-inputs
        (("rust-hyper" ,rust-hyper-0.13) ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio 0.2 compat utilities")
    (description "Tokio 0.2 compat utilities")
    (license license:expat)))

(define-public rust-pasts-0.4
  (package
    (name "rust-pasts")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pasts" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11rdczdhpazclhkbbjafv5nd9ybll9a110crhh67si0p5rdc6mz7"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/AldaronLau/pasts/blob/main/CHANGELOG.md")
    (synopsis "Minimal and simpler alternative to the futures crate.")
    (description "Minimal and simpler alternative to the futures crate.")
    (license (list license:asl2.0 license:zlib))))

(define-public rust-cala-core-0.1
  (package
    (name "rust-cala-core")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cala_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17939zm80lxi0mqsvi98wv2hjasbbh132j5i2m201x30j8dkx4wx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pasts" ,rust-pasts-0.4)
         ("rust-stdweb" ,rust-stdweb-0.4)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/libcala/cala_core/blob/master/CHANGELOG.md")
    (synopsis "Low-level platform glue for Cala")
    (description "Low-level platform glue for Cala")
    (license (list license:asl2.0 license:zlib))))

(define-public rust-whoami-0.9
  (package
    (name "rust-whoami")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "whoami" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "012mw2q72gpmf354yw2qc5w105ziac75shpqp1f62x4hnqx7g13q"))))
    (build-system cargo-build-system)
    (arguments `(#:cargo-inputs (("rust-cala-core" ,rust-cala-core-0.1))))
    (home-page "https://github.com/libcala/whoami/blob/main/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description "Retrieve the current user and environment.")
    (license (list license:expat license:boost1.0))))

(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0g5xp1xmyfibyscynig2m5gvp5smgg7xvcwr0p3yzc7zvxx99gbd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.
")
    (description "The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-io-0.3
  (package
    (name "rust-futures-io")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ckir41haa2hs9znrwavgh33hv3l23jmywqg73xwdam1ym5d7ydi"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.
")
    (description
      "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the
futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0r3i29hhfhv69qjdxh3j4ffxji4hl0yc1gmim1viy9vsni0czdfr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
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
      "Common utilities and extension traits for the futures-rs library.
")
    (description
      "Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-task" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wmd3b70sgp1dr3q24439hkm7zj9m1lcafmqvzj7q5ihbi4cdrvf"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-executor-0.3
  (package
    (name "rust-futures-executor")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-executor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0alxxnjbi6jjsjkj6mkmvizmwprfi99ldkmqhmfbj3xibgzx5mi9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-task" ,rust-futures-task-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "Executors for asynchronous tasks based on the futures-rs library.
")
    (description
      "Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "026m2x353l7x7apa3hdx26ma7kwgxgbghl0393v4zmv8rfn5n1g3"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mw34nxzggvr2jvk4ljygy077wy32lrdxkyw1j0mj9dqc42gzj6h"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02vzdkc1n25nliwa2758pni7fyn1ch2msrzw18v5ycw8cl5xlgds"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.
")
    (description "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-0.3.15
  (package
    (name "rust-futures")
    (version "0.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "09zwmmfi8d1glhi0bz2didagjyqz3q9gxa7nq4vzmqns0fl46zhf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-executor" ,rust-futures-executor-0.3)
         ("rust-futures-io" ,rust-futures-io-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
         ("rust-futures-task" ,rust-futures-task-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3))
        #:cargo-development-inputs
        (("rust-assert-matches" ,rust-assert-matches-1)
         ("rust-pin-project" ,rust-pin-project-1)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-static-assertions" ,rust-static-assertions-1)
         ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.
")
    (description
      "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zeroize-0.4
  (package
    (name "rust-zeroize")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zeroize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fbwsinb9yp6n7gh7h9b24mycwk7vkqcp6rmssi8pwnykpngzpfp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1) ("rust-semver" ,rust-semver-0.9))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis
      "Securely clear secrets from memory with a simple trait built on
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

(define-public rust-keychain-services-0.1
  (package
    (name "rust-keychain-services")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "keychain-services" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1a5hw0nd49wxrgfmxd1scq80q9zw8lsd7ddcl7grgzri7filnvbh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-core-foundation" ,rust-core-foundation-0.6)
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

(define-public rust-security-framework-sys-0.4
  (package
    (name "rust-security-framework-sys")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01c4fkj78frz6kz9vri1am2pbkz52raxws5lbrli5xajjbci3gqp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description "Apple `Security.framework` low-level FFI bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-0.4
  (package
    (name "rust-security-framework")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0dg504y0ys27qksdcnn8qcyvrfhkjx8f5d6jvpm7i7yrsw18k034"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-core-foundation" ,rust-core-foundation-0.7)
         ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-security-framework-sys" ,rust-security-framework-sys-0.4))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description "Security.framework bindings for macOS and iOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-hkdf-0.10
  (package
    (name "rust-hkdf")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0kwn3scjvv2x8zc6nz3wrnzxp9shpsdxnjqiyv2r65r3kiijzasi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-digest" ,rust-digest-0.9) ("rust-hmac" ,rust-hmac-0.10))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-dbus-0.2
  (package
    (name "rust-dbus")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0b85dl7y396g8xh1xh89wxnb1fvvf840dar9axavfhhhlq7c385l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-secret-service-1
  (package
    (name "rust-secret-service")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "secret-service" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12hxz35i7sw5xsdldz1c6776fmz98z4dwh5779jis98w61020xbd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes" ,rust-aes-0.6)
         ("rust-block-modes" ,rust-block-modes-0.7)
         ("rust-dbus" ,rust-dbus-0.2)
         ("rust-hkdf" ,rust-hkdf-0.10)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-num" ,rust-num-0.3)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/hwchen/secret-service-rs.git")
    (synopsis "Library to interface with Secret Service API")
    (description "Library to interface with Secret Service API")
    (license (list license:expat license:asl2.0))))

(define-public rust-keyring-0.10
  (package
    (name "rust-keyring")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "keyring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ylz593bd2xw45839iivp6frclrfi1v2zz85qy9rkxlrh7s69k9b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-secret-service" ,rust-secret-service-1)
         ("rust-security-framework" ,rust-security-framework-0.4)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap-2)
         ("rust-keychain-services" ,rust-keychain-services-0.1)
         ("rust-rpassword" ,rust-rpassword-2)
         ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/hwchen/keyring-rs")
    (synopsis "Cross-platform library for managing passwords/credentials")
    (description "Cross-platform library for managing passwords/credentials")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-log-0.1
  (package
    (name "rust-tracing-log")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qv1cwvdqrgvizkszbff4fvkw0m3nn5yz68r3yaw2hcflivk94m6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-env-logger" ,rust-env-logger-0.7)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Provides compatibility between `tracing` and the `log` crate.
")
    (description
      "This package provides compatibility between `tracing` and the `log` crate.")
    (license license:expat)))

(define-public rust-matchers-0.1
  (package
    (name "rust-matchers")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matchers" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex-automata" ,rust-regex-automata-0.1))))
    (home-page "https://github.com/hawkw/matchers")
    (synopsis "Regex matching on character and byte streams.
")
    (description "Regex matching on character and byte streams.")
    (license license:expat)))

(define-public rust-tracing-subscriber-0.3
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-subscriber" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kxfkxf00n2s1ak0131z7lbkicz7pwkz4h3k9a9vpvjfbi26dgkp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ansi-term" ,rust-ansi-term-0.12)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-matchers" ,rust-matchers-0.1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-regex" ,rust-regex-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sharded-slab" ,rust-sharded-slab-0.1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-thread-local" ,rust-thread-local-1)
         ("rust-time" ,rust-time-0.3)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-core" ,rust-tracing-core-0.1)
         ("rust-tracing-log" ,rust-tracing-log-0.1)
         ("rust-tracing-serde" ,rust-tracing-serde-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Utilities for implementing and composing `tracing` subscribers.
")
    (description
      "Utilities for implementing and composing `tracing` subscribers.")
    (license license:expat)))

(define-public rust-tracing-core-0.1
  (package
    (name "rust-tracing-core")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1r262wskhm6wmc5i2bxz44nglyzqaq3x50s0h5q0ffdq6xbdckhz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.
")
    (description "Core primitives for application-level tracing.")
    (license license:expat)))

(define-public rust-tracing-attributes-0.1
  (package
    (name "rust-tracing-attributes")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "13izzsgrmcg2076ksdibvyxx7d8y9klm3b9pycjyh4hmz2w81x7l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Procedural macro attributes for automatically instrumenting functions.
")
    (description
      "Procedural macro attributes for automatically instrumenting functions.")
    (license license:expat)))

(define-public rust-tracing-0.1
  (package
    (name "rust-tracing")
    (version "0.1.29")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0191zcbnkn8wy0b7xbz7jd9m2xf3sjr8k3cfqzghxwya6a966nip"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
         ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.
")
    (description "Application-level tracing for Rust.")
    (license license:expat)))

(define-public rust-generator-0.7
  (package
    (name "rust-generator")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vhj3f0rf4mlh5vz7pz5rxmgry1cc62x21mf9ld1r292m2f2gnf1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-rustversion" ,rust-rustversion-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Xudong-Huang/generator-rs.git")
    (synopsis "Stackfull Generator Library in Rust")
    (description "Stackfull Generator Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-loom-0.5
  (package
    (name "rust-loom")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02a30cv9l2afjq5bg42hgcjspx8fgwyij0cf9saw8b73539wgigd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-generator" ,rust-generator-0.7)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/tokio-rs/loom")
    (synopsis "Permutation testing for concurrent code")
    (description "Permutation testing for concurrent code")
    (license license:expat)))

(define-public rust-tokio-1.6
  (package
    (name "rust-tokio")
    (version "1.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0mhhmbxhdwaw699wvxhs4mi9a5g8yb6zw4i2p3cyrn9gdgdv1cd4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mio" ,rust-mio-0.7)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
         ("rust-tokio-macros" ,rust-tokio-macros-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-async-stream" ,rust-async-stream-0.3)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-loom" ,rust-loom-0.5)
         ("rust-nix" ,rust-nix-0.19)
         ("rust-proptest" ,rust-proptest-1)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://tokio.rs")
    (synopsis
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-tokio-stream-0.1.7
  (package
    (name "rust-tokio-stream")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-stream" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zvj8d2i1147s4i3ml3lh3h6fypncjmbw0h1mhck3w2kh9lkybvv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1.8)
         ("rust-tokio-util" ,rust-tokio-util-0.6))
        #:cargo-development-inputs
        (("rust-async-stream" ,rust-async-stream-0.3)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-proptest" ,rust-proptest-1)
         ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-tracing-log-0.1
  (package
    (name "rust-tracing-log")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qv1cwvdqrgvizkszbff4fvkw0m3nn5yz68r3yaw2hcflivk94m6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-env-logger" ,rust-env-logger-0.7)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Provides compatibility between `tracing` and the `log` crate.
")
    (description
      "This package provides compatibility between `tracing` and the `log` crate.")
    (license license:expat)))

(define-public rust-matchers-0.1
  (package
    (name "rust-matchers")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matchers" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-regex-automata" ,rust-regex-automata-0.1))))
    (home-page "https://github.com/hawkw/matchers")
    (synopsis "Regex matching on character and byte streams.
")
    (description "Regex matching on character and byte streams.")
    (license license:expat)))

(define-public rust-tracing-subscriber-0.3
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-subscriber" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kxfkxf00n2s1ak0131z7lbkicz7pwkz4h3k9a9vpvjfbi26dgkp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ansi-term" ,rust-ansi-term-0.12)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-matchers" ,rust-matchers-0.1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-regex" ,rust-regex-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sharded-slab" ,rust-sharded-slab-0.1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-thread-local" ,rust-thread-local-1)
         ("rust-time" ,rust-time-0.3)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-core" ,rust-tracing-core-0.1)
         ("rust-tracing-log" ,rust-tracing-log-0.1)
         ("rust-tracing-serde" ,rust-tracing-serde-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Utilities for implementing and composing `tracing` subscribers.
")
    (description
      "Utilities for implementing and composing `tracing` subscribers.")
    (license license:expat)))

(define-public rust-tracing-core-0.1
  (package
    (name "rust-tracing-core")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1r262wskhm6wmc5i2bxz44nglyzqaq3x50s0h5q0ffdq6xbdckhz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.
")
    (description "Core primitives for application-level tracing.")
    (license license:expat)))

(define-public rust-tracing-attributes-0.1
  (package
    (name "rust-tracing-attributes")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "13izzsgrmcg2076ksdibvyxx7d8y9klm3b9pycjyh4hmz2w81x7l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Procedural macro attributes for automatically instrumenting functions.
")
    (description
      "Procedural macro attributes for automatically instrumenting functions.")
    (license license:expat)))

(define-public rust-tracing-0.1
  (package
    (name "rust-tracing")
    (version "0.1.29")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0191zcbnkn8wy0b7xbz7jd9m2xf3sjr8k3cfqzghxwya6a966nip"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
         ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.
")
    (description "Application-level tracing for Rust.")
    (license license:expat)))

(define-public rust-generator-0.7
  (package
    (name "rust-generator")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vhj3f0rf4mlh5vz7pz5rxmgry1cc62x21mf9ld1r292m2f2gnf1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-rustversion" ,rust-rustversion-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Xudong-Huang/generator-rs.git")
    (synopsis "Stackfull Generator Library in Rust")
    (description "Stackfull Generator Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-loom-0.5
  (package
    (name "rust-loom")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02a30cv9l2afjq5bg42hgcjspx8fgwyij0cf9saw8b73539wgigd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-generator" ,rust-generator-0.7)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/tokio-rs/loom")
    (synopsis "Permutation testing for concurrent code")
    (description "Permutation testing for concurrent code")
    (license license:expat)))

(define-public rust-tokio-1.8
  (package
    (name "rust-tokio")
    (version "1.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1pakdmf6w3bzwddjswn8dj2kksfkxsfyli2vvl1h96xwh4wfinjh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mio" ,rust-mio-0.7)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
         ("rust-tokio-macros" ,rust-tokio-macros-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-async-stream" ,rust-async-stream-0.3)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-loom" ,rust-loom-0.5)
         ("rust-mockall" ,rust-mockall-0.10)
         ("rust-nix" ,rust-nix-0.22)
         ("rust-ntapi" ,rust-ntapi-0.3)
         ("rust-proptest" ,rust-proptest-1)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-socket2" ,rust-socket2-0.4)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://tokio.rs")
    (synopsis
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

