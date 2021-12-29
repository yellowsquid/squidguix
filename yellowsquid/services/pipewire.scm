(define-module (yellowsquid services pipewire)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (yellowsquid packages rtkit)
  #:export (rtkit-service-type
            home-pipewire-service-type
            home-wireplumber-service-type))

(define %rtkit-accounts
  (list (user-group (name "rtkit") (system? #t))
        (user-account
         (name "rtkit")
         (group "rtkit")
         (system? #t)
         (comment "RealtimeKit daemon user")
         (home-directory "/var/empty")
         (shell "/run/current-system/profile/sbin/nologin"))))

(define rtkit-service-type
  (service-type (name 'rtkit)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          list)
                       (service-extension account-service-type
                                          (const %rtkit-accounts))))
                (default-value rtkit)
                (description "Realtime Kit scheduling policy daemon.")))

;; TODO: add dependency on rtkit-service-type.
(define (home-pipewire-shepherd-service pipewire)
  "Return a shepherd service for PipeWire."
  (list (shepherd-service
         (documentation "PipeWire PulseAudio")
         (provision '(pipewire-pulse pulseaudio))
         (requirement '(pipewire pipewire-media-session))
         (start #~(make-forkexec-constructor
                   (list #$(file-append pipewire "/bin/pipewire-pulse"))))
         (stop #~(make-kill-destructor)))
        (shepherd-service
         (documentation "PipeWire Multimedia Service")
         (provision '(pipewire))
         (start #~(make-forkexec-constructor
                   (list #$(file-append pipewire "/bin/pipewire"))))
         (stop #~(make-kill-destructor)))))

(define home-pipewire-service-type
  (service-type (name 'home-pipewire)
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-pipewire-shepherd-service)
                       (service-extension home-profile-service-type
                                          (lambda (pkg) `(,pkg)))))
                (default-value pipewire-0.3)
                (description "Userland PipeWire daemon.")))

(define (home-wireplumber-shepherd-service wireplumber)
  "Return a shepherd service for WirePlumber."
  (list (shepherd-service
         (documentation "Multimedia Service Session Manager")
         (provision '(wireplumber pipewire-media-session))
         (requirement '(pipewire))
         (start #~(make-forkexec-constructor
                   (list #$(file-append wireplumber "/bin/wireplumber"))))
         (stop #~(make-kill-destructor)))))

(define home-wireplumber-service-type
  (service-type (name 'home-wireplumber)
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-wireplumber-shepherd-service)
                       (service-extension home-profile-service-type
                                          (lambda (pkg) `(,pkg)))))
                (default-value wireplumber)
                (description "Userland WirePlumber daemon.")))
