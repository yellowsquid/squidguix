(define-module (yellowsquid packages games)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public minecraft-launcher
  (package
    (name "minecraft-launcher")
    (version "1.0.1221")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launcher.mojang.com/download/linux/x86_64/minecraft-launcher_"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1aqlcb9sil0l6nb5anviqws3v7srxr2i3rxjkj145v4wzi20p7yd"))))
    (build-system copy-build-system)
    (inputs `(("alsa-lib" ,alsa-lib)
              ("at-spi2-atk" ,at-spi2-atk)
              ("at-spi2-core" ,at-spi2-core)
              ("atk" ,atk)
              ("cups" ,cups-minimal)
              ("dbus" ,dbus)
              ("expat" ,expat)
              ("gcc" ,gcc "lib")
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("glibc" ,glibc)
              ("gtk" ,gtk+)
              ("libdrm" ,libdrm)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("mesa" ,mesa)
              ("nspr" ,nspr)
              ("nss" ,nss)
              ("openjdk17" ,openjdk17)
              ("pango" ,pango)
              ("util-linux" ,util-linux "lib")
              ("xrandr" ,xrandr)
              ("zlib" ,zlib)))
    (native-inputs (list patchelf))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-elf
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libs '("libgtk-3.so.0" "libgdk-3.so.0" "libgobject-2.0.so.0" "libstdc++.so.6" "libgcc_s.so.1" "ld-linux-x86-64.so.2"))
                    (rpath (string-join (map (lambda (lib) (dirname (search-input-file inputs (string-append "/lib/" lib)))) libs) ":")))
               (invoke "patchelf" "--set-rpath" rpath "minecraft-launcher")
               (invoke "patchelf" "--set-interpreter" (search-input-file inputs "/lib/ld-linux-x86-64.so.2") "minecraft-launcher"))))
         (add-after 'install 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/applications"))
               (with-output-to-file (string-append out
                                     "/share/applications/minecraft-launcher.desktop")
                 (lambda _
                   (format #t "[Desktop Entry]
Type=Application
Version=~a
Name=Minecraft Launcher
GenericName=Minecraft Launcher
Comment=Official Minecraft Launcher
Exec=~a
Terminal=false
Categories=Game;Application;
StartupNotify=true
"
                           ,version
                           (string-append out "/bin/minecraft-launcher"))))))))
       #:install-plan '(("minecraft-launcher" "bin/minecraft-launcher"))))
    (home-page "https://www.minecraft.net/")
    (synopsis "Official Minecraft launcher.")
    (description "The official launcher for Minecraft.")
    (license 'nil)))
