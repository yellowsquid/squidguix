(define-module (yellowsquid services spotifyd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (yellowsquid packages rust-apps)
  #:export (spotifyd-configuration
            spotifyd-configuration?
            spotifyd-service-type))

(define (label-raw-or-cmd value)
  (cond ((file-like? value) (list 'cmd value))
        ((string? value) (list 'raw value))
        ((symbol? value) (list 'raw (symbol->string value)))
        (else (throw 'bad! value))))

(define (maybe-label-raw-or-cmd value)
  (if value
      (label-raw-or-cmd value)
      #f))

(define-record-type* <spotifyd-configuration>
  spotifyd-configuration make-spotifyd-configuration
  spotifyd-configuration?
  (package spotifyd-package (default spotifyd))
  (requires spotifyd-requires (default '(pulseaudio)))
  (username spotifyd-username (sanitize label-raw-or-cmd))
  (password spotifyd-password (sanitize maybe-label-raw-or-cmd) (default #f))
  (use-keyring? spotifyd-use-keyring? (default #f))
  (use-mpris? spotifyd-use-mpris? (default #t))
  (backend spotifyd-backend (default "pulseaudio"))
  (alsa-device spotifyd-alsa-device (default #f))
  (alsa-control spotifyd-alsa-control (default #f))
  (alsa-mixer spotifyd-alsa-mixer (default #f))
  (volume-controller spotifyd-volume-controller (default #f))
  (on-song-change-hook spotifyd-on-song-change-hook (default #f))
  (device-name spotifyd-device-name)
  (bitrate spotifyd-bitrate (default #f))
  (cache-path spotifyd-cachepath (default #f))
  (no-audio-cache? spotifyd-no-audio-cache? (default #f))
  (initial-volume spotifyd-initial-volume (default #f))
  (volume-normalisation? spotifyd-volume-normalisation? (default #f))
  (normalisation-pregain spotifyd-normalisation-pregain (default #f))
  (zeroconf-port spotifyd-zeroconf-port (default #f))
  (proxy spotifyd-proxy (default #f))
  (device-type spotifyd-device-type (default "computer")))

(define (spotifyd-config-file config)
  (define* (put-string key value #:optional (quote? #t))
    (list key
          (if quote? " = \"" " = ")
          value
          (if quote? "\"\n" "\n")))

  (define* (put-maybe key value #:optional (quote? #t))
    (if value (put-string key value quote?) '()))

  (define (put-boolean key value)
    (list key " = " (if value "true" "false") "\n"))

  (match-record config <spotifyd-configuration>
    (username password use-keyring? use-mpris? backend alsa-device
              alsa-control alsa-mixer volume-controller on-song-change-hook
              device-name bitrate cache-path no-audio-cache? initial-volume
              volume-normalisation? normalisation-pregain zeroconf-port proxy
              device-type)
    (apply
     mixed-text-file
     `("spotifyd.conf"
       "[global]\n"
       ,@(match username
           (('raw value) (put-string "username" value))
           (('cmd value) (put-string "username_cmd" value)))
       ,@(if use-keyring?
             (put-boolean "use_keyring" #t)
             (match password
               (('raw value) (put-string "password" value))
               (('cmd value) (put-string "password_cmd" value))
               (_ (throw 'bad! "need to either use keyring or supply password"))))
       ,@(put-boolean "use_mpris" use-mpris?)
       ,@(put-string "backend" backend)
       ,@(put-maybe "device" alsa-device)
       ,@(put-maybe "control" alsa-control)
       ,@(put-maybe "mixer" alsa-mixer)
       ,@(put-maybe "volume_controller" volume-controller)
       ,@(put-maybe "on_song_change_hook" on-song-change-hook)
       ,@(put-string "device_name" device-name)
       ,@(put-maybe "bitrate" bitrate #f)
       ,@(put-maybe "cache_path" cache-path)
       ,@(put-boolean "no_audio_cache" no-audio-cache?)
       ,@(put-maybe "initial_volume" initial-volume)
       ,@(put-boolean "volume_normalisation" volume-normalisation?)
       ,@(put-maybe "normalisation_pregain" normalisation-pregain #f)
       ,@(put-maybe "zeroconf_port" zeroconf-port #f)
       ,@(put-maybe "proxy" proxy #f)
       ,@(put-string "device_type" device-type)))))

(define (spotifyd-shepherd-service config)
  (list (shepherd-service
         (documentation "A Spotify-playing daemon")
         (provision '(spotifyd))
         (requirement (spotifyd-requires config))
         (start #~(make-forkexec-constructor
                   '(#$(file-append (spotifyd-package config) "/bin/spotifyd")
                     "--no-daemon"
                     "--config-path" #$(spotifyd-config-file config))))
         (stop #~(make-kill-destructor)))))

(define spotifyd-service-type
  (service-type
   (name 'spotifyd)
   (extensions (list (service-extension home-shepherd-service-type
                                        spotifyd-shepherd-service)))
   (description "A spotify-playing daemon")))
