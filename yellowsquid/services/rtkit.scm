(define-module (yellowsquid services rtkit)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (yellowsquid packages rtkit)
  #:export (rtkit-service-type))

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
