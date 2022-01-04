(define-module (yellowsquid services btrfs)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (<snapshot-location>
            snapshot-location
            make-snapshot-location
            snapshot-location?
            snapshot-location-name
            snapshot-location-path

            <snapshot-frequency>
            snapshot-frequency
            make-snapshot-frequency
            snapshot-frequency?
            snapshot-frequency-name
            snapshot-frequency-min-period
            snapshot-frequency-max-keep

            list-of-locations?
            list-of-frequencies?

            <btrfs-snapshot-configuration>
            btrfs-snapshot-configuration
            make-btrfs-snapshot-configuration
            btrfs-snapshot-configuration?
            btrfs-snapshot-configuration-btrfs-progs
            btrfs-snapshot-configuration-destination
            btrfs-snapshot-configuration-frequencies
            btrfs-snapshot-configuration-locations
            btrfs-snapshot-configuration-ts-format

            btrfs-snapshot-service-type
            home-btrfs-snapshot-service-type))

(define-record-type* <snapshot-location>
  snapshot-location make-snapshot-location
  snapshot-location?
  (name snapshot-location-name)
  (path snapshot-location-path))

(define-record-type* <snapshot-frequency>
  snapshot-frequency make-snapshot-frequency
  snapshot-frequency?
  (name snapshot-frequency-name)
  ;; in seconds
  (min-period snapshot-frequency-min-period)
  (max-keep snapshot-frequency-max-keep))

(define list-of-locations?
  (list-of snapshot-location?))

(define list-of-frequencies?
  (list-of snapshot-frequency?))

(define-configuration/no-serialization btrfs-snapshot-configuration
  (btrfs-progs
   (package btrfs-progs)
   "The btrfs package to use.")
  (destination
   (string "/backup")
   "Root directory where backups are stored.")
  (frequencies
   (list-of-frequencies '())
   "List of backup frequencies.")
  (locations
   (list-of-locations '())
   "List of locations to backup.")
  (ts-format
   (string "%FT%T")
   "File timestamp format."))

(define (frobnicate destination freq loc)
  (string-append (if (string-suffix? "/" destination)
                     destination
                     (string-append destination "/"))
                 (symbol->string (snapshot-location-name loc))
                 "/"
                 (symbol->string (snapshot-frequency-name freq))
                 "/"))

(define (btrfs-snapshot-one-job destination btrfs ts-format freq loc)
  (define dest (frobnicate destination freq loc))

  (define name->path+date
    #~(lambda (name)
        "Converts the NAME of a snapshot into the path to the snapshot and the
date it was created."
        (let ((strptimed
               (false-if-exception (strptime #$ts-format name))))
          (and strptimed
               (list (string-append #$dest name)
                     (car (mktime (car strptimed) "GMT")))))))

  (define name->date
    #~(lambda (name)
        "Converts the NAME of a snapshot into the date it was created."
        (let ((path+date (#$name->path+date name)))
          (and path+date (cadr path+date)))))

  (define next-create
    #~(lambda (date)
        "Returns the next time after DATE that a new snapshot can be created."
        (+ date #$(snapshot-frequency-min-period freq))))

  (define first-delete
    #~(lambda (date)
        "Returns the first time after DATE that this snapshot can be deleted."
        (+ date #$(snapshot-frequency-max-keep freq))))

  (define make-time
    #~(lambda (date)
        "Returns DATE as a string."
        (strftime #$ts-format (gmtime date))))

  (list
   #~(job (lambda (now)
            (use-modules (ice-9 ftw))
            (let* ((scanned (or (scandir #$dest) '()))
                   (dates (filter-map
                           #$name->date
                           scanned)))
              (if (null? dates)
                  (next-minute-from now)
                  (max (if (< (abs (- now (current-time))) 5)
                           (next-minute-from (1+ now))
                           (let ((time (localtime now)))
                             (set-tm:year time (+ 1000 (tm:year time)))
                             (car (mktime time))))
                       (apply max (map #$next-create dates))))))
          #$(program-file
             (string-append "backup-create-"
                            (symbol->string (snapshot-location-name loc))
                            "-"
                            (symbol->string (snapshot-frequency-name freq))
                            ".scm")
             (with-imported-modules
                 (source-module-closure '((guix build utils)))
               #~(begin
                   (use-modules (guix build utils)
                                (ice-9 ftw)
                                (ice-9 match)
                                (srfi srfi-1))

                   (let* ((now (current-time))
                          (scanned (or (scandir #$dest)
                                       (begin (mkdir-p #$dest) '())))
                          (dates (filter-map #$name->date scanned))
                          (skip? (any (lambda (date)
                                        (and (< now (#$next-create date))
                                             date))
                                      dates)))
                     (if skip?
                         (format
                          #t
                          "Skipping ~a backup of ~a as last success was at ~a.\n"
                          #$(symbol->string (snapshot-frequency-name freq))
                          #$(symbol->string (snapshot-location-name loc))
                          (#$make-time skip?))
                         (invoke
                          #$btrfs
                          "subvolume"
                          "snapshot"
                          "-r"
                          #$(snapshot-location-path loc)
                          (string-append
                           #$dest
                           (#$make-time (current-time)))))))))
          #$(format #f
                    "Create a ~a backup of ~a."
                    (symbol->string (snapshot-frequency-name freq))
                    (symbol->string (snapshot-location-name loc))))
   #~(job (lambda (now)
            (use-modules (ice-9 ftw))
            (let* ((scanned (or (scandir #$dest) '()))
                   (dates (filter-map
                           #$name->date
                           scanned)))
              (if (null? dates)
                  (next-minute-from now)
                  (max (if (< (abs (- now (current-time))) 5)
                           (next-minute-from (1+ now))
                           (let ((time (localtime now)))
                             (set-tm:year time (+ 1000 (tm:year time)))
                             (car (mktime time))))
                       (apply min (map #$first-delete dates))))))
          #$(program-file
             (string-append "backup-delete-"
                            (symbol->string (snapshot-location-name loc))
                            "-"
                            (symbol->string (snapshot-frequency-name freq))
                            ".scm")
             (with-imported-modules
                 (source-module-closure '((guix build utils)))
               #~(begin
                   (use-modules (guix build utils)
                                (ice-9 ftw)
                                (ice-9 match)
                                (srfi srfi-1))

                   (let* ((now (current-time))
                          (scanned (or (scandir #$dest)
                                       (begin (mkdir-p #$dest) '())))
                          (path-dates (filter-map #$name->path+date scanned))
                          (del (filter-map
                                (match-lambda
                                  ((path date)
                                   (and (> now (#$first-delete date))
                                        path)))
                                path-dates)))
                     (for-each
                      (lambda (path)
                        (invoke #$btrfs
                                "property"
                                "set"
                                path
                                "ro"
                                "false"))
                      del)
                     (unless (null? del)
                       (apply invoke
                              #$btrfs
                              (cons* "subvolume" "delete" "-c" del)))))))
          #$(format #f
                    "Delete a ~a backup of ~a."
                    (symbol->string (snapshot-frequency-name freq))
                    (symbol->string (snapshot-location-name loc))))))

(define (btrfs-snapshot-jobs config)
  "Returns a list of mcron jobs to manage local backups."
  (define (product xs ys)
    (match xs
      (() '())
      ((x xs ...) (append (map (lambda (y) (list x y)) ys) (product xs ys)))))

  (match-record config <btrfs-snapshot-configuration>
    (btrfs-progs destination frequencies locations ts-format)
    (append-map
     (match-lambda
       ((freq loc)
        (btrfs-snapshot-one-job
         destination
         (file-append btrfs-progs "/bin/btrfs")
         ts-format
         freq
         loc)))
     (product frequencies locations))))

(define (btrfs-snapshot-activation config)
  "Returns a list of mcron jobs to manage local backups."
  (match-record config <btrfs-snapshot-configuration>
    (btrfs-progs destination frequencies locations ts-format)
    #~(begin
        #@(append-map
           (lambda (loc)
             (map
              (lambda (freq)
                #~(mkdir-p #$(frobnicate destination loc freq)))
              frequencies))
           locations))))

(define home-btrfs-snapshot-service-type
  (service-type (name 'home-btrfs-snapshot-service)
                (extensions
                 (list (service-extension
                        home-mcron-service-type
                        btrfs-snapshot-jobs)
                       (service-extension
                        home-activation-service-type
                        btrfs-snapshot-activation)))
                (default-value (btrfs-snapshot-configuration))
                (description "Create jobs to keep a rolling set of backups for
some btrfs subvolumes.")))

(define btrfs-snapshot-service-type
  (service-type (name 'btrfs-snapshot-service)
                (extensions
                 (list (service-extension
                        mcron-service-type
                        btrfs-snapshot-jobs)
                       (service-extension
                        activation-service-type
                        btrfs-snapshot-activation)))
                (default-value (btrfs-snapshot-configuration))
                (description "Create jobs to keep a rolling set of backups for
some btrfs subvolumes.")))
