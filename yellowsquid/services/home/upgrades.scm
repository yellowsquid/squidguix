(define-module (yellowsquid services home upgrades)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:export (<home-unattended-upgrade-configuration>
            home-unattended-upgrade-configuration
            make-home-unattended-upgrade-configuration
            home-unattended-upgrade-configuration?
            home-unattended-upgrade-configuration-file
            home-unattended-upgrade-configuration-schedule
            home-unattended-upgrade-configuration-channels
            home-unattended-upgrade-configuration-services-to-restart
            home-unattended-upgrade-home-expiration
            home-unattended-upgrade-maximum-duration
            home-unattended-upgrade-configuration-log-file

            home-unattended-upgrade-service-type))

(define-record-type* <home-unattended-upgrade-configuration>
  home-unattended-upgrade-configuration make-home-unattended-upgrade-configuration
  home-unattended-upgrade-configuration?
  (configuration-file home-unattended-upgrade-configuration-file
                      (default #~(string-append (getenv "XDG_CONFIG_HOME") "/guix/home.scm")))
  (schedule home-unattended-upgrade-configuration-schedule
            (default "30 01 * * 0"))
  (channels home-unattended-upgrade-configuration-channels
            (default #~%default-channels))
  (services-to-restart home-unattended-upgrade-configuration-services-to-restart
                       (default '(mcron)))
  (home-expiration home-unattended-upgrade-home-expiration
                   (default (* 3 30 24 3600)))
  (maximum-duration home-unattended-upgrade-maximum-duration
                    (default 3600))
  (log-file home-unattended-upgrade-configuration-log-file
            (default %home-unattended-upgrade-log-file)))

(define %home-unattended-upgrade-log-file
  #~(string-append (getenv "XDG_LOG_HOME") "/unattended-upgrade.log"))

(define (home-unattended-upgrade-mcron-jobs config)
  (define channels
    (scheme-file "channels.scm"
                 (home-unattended-upgrade-configuration-channels config)))

  (define log
    (home-unattended-upgrade-configuration-log-file config))

  (define services
    (home-unattended-upgrade-configuration-services-to-restart config))

  (define expiration
    (home-unattended-upgrade-home-expiration config))

  (define config-file
    (home-unattended-upgrade-configuration-file config))

  (define code
    (with-extensions (list shepherd)
      (with-imported-modules (source-module-closure '((guix build utils)
                                                      (gnu services herd)))
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 match)
                         (shepherd comm)
                         (srfi srfi-19)
                         (srfi srfi-34))

            (define log
              (open-file #$log "a0"))

            (define (timestamp)
              (date->string (time-utc->date (current-time time-utc))
                            "[~4]"))

            (define (alarm-handler . _)
              (format #t "~a time is up, aborting upgrade~%"
                      (timestamp))
              (exit 1))

            (define (display-message message)
              (format #t "~a shepherd: ~a~%" (timestamp) message))

            (define (restart-service sock service)
              (write `(shepherd-command (version 0)
                                        (action restart)
                                        (service ,service)
                                        (arguments '())
                                        (directory ,(getcwd)))
                     sock)
              (force-output sock)

              (match (read sock)
                (('reply ('version 0 _ ...) ('result result) ('error #f) ('messages messages))
                 (for-each display-message messages)
                 result)
                (('reply ('version 0 x ...) ('result y) ('error error) ('messages messages))
                 (for-each display-message messages)
                 ((@@ (gnu services herd) raise-shepherd-error) error)
                 #f)
                (x
                 #f)))

            ;; 'guix time-machine' needs X.509 certificates to authenticate the
            ;; Git host.
            (setenv "SSL_CERT_DIR"
                    #$(file-append nss-certs "/etc/ssl/certs"))

            ;; Make sure the upgrade doesn't take too long.
            (sigaction SIGALRM alarm-handler)
            (alarm #$(home-unattended-upgrade-maximum-duration config))

            ;; Redirect stdout/stderr to LOG to save the output of 'guix' below.
            (redirect-port log (current-output-port))
            (redirect-port log (current-error-port))

            (format #t "~a starting upgrade...~%" (timestamp))
            (guard (c ((invoke-error? c)
                       (report-invoke-error c)))
              (invoke #$(file-append guix "/bin/guix")
                      "time-machine" "-C" #$channels
                      "--" "home" "reconfigure" #$config-file)

              ;; 'guix system delete-generations' fails when there's no
              ;; matching generation.  Thus, catch 'invoke-error?'.
              (guard (c ((invoke-error? c)
                         (report-invoke-error c)))
                (invoke #$(file-append guix "/bin/guix")
                        "home" "delete-generations"
                        #$(string-append (number->string expiration)
                                         "s")))

              (format #t "~a restarting services...~%" (timestamp))
              (let ((connection (open-connection)))
                (dynamic-wind
                  (const #t)
                  (lambda ()
                    (for-each (lambda (service) (restart-service connection service))
                              '#$services))
                  (lambda () (close-port connection))))


              ;; XXX: If 'mcron' has been restarted, perhaps this isn't
              ;; reached.
              (format #t "~a upgrade complete~%" (timestamp)))))))

  (define upgrade
    (program-file "home-unattended-upgrade" code))

  (list #~(job #$(home-unattended-upgrade-configuration-schedule config)
               #$upgrade)))

(define home-unattended-upgrade-service-type
  (service-type
   (name 'home-unattended-upgrade)
   (extensions
    (list (service-extension home-mcron-service-type
                             home-unattended-upgrade-mcron-jobs)))
   (description
    "Periodically upgrade the home profile from the current configuration.")
   (default-value (home-unattended-upgrade-configuration))))
