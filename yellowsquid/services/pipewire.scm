(define-module (yellowsquid services pipewire)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-pipewire-service-type
            home-wireplumber-service-type))

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
