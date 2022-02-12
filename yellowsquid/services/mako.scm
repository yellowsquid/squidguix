(define-module (yellowsquid services mako)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (mako-configuration
            mako-service-type))

(define-configuration mako-configuration
  (package
   (package mako)
   "Package to use")
  (config-parts
   (text-config '())
   "Configuration file parts"))

(define (mako-files config)
  `(("config/mako/config"
     ,(serialize-configuration
       config
       (filter-configuration-fields
        mako-configuration-fields
        'config-parts)))))

(define mako-service-type
  (service-type (name 'mako)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        mako-files)
                       (service-extension
                        home-profile-service-type
                        (lambda (config)
                          (list (mako-configuration-package config))))))
                (default-value (mako-configuration))
                (description "Install and configure mako.")))
