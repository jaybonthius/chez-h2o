(define-record-type listener
  (nongenerative) (sealed #t)
  (fields address port certificate key))

(define (check-listener who address port)
  (unless (string? address) (assertion-violationf who "~s is not an address" address))
  (unless (and (fixnum? port) (fx<= 1 port 65535))
    (assertion-violationf who "~s is not a port number" port)))

(define (http-listener address port)
  (check-listener 'http-listener address port)
  (make-listener address port #f #f))

(define (https-listener address port certificate key)
  (check-listener 'https-listener address port)
  (unless (and (string? certificate) (string? key))
    (assertion-violationf 'https-listener "~s and ~s are not file names" certificate key))
  (make-listener address port certificate key))

;; H2O stores a host without brackets and 65535 as "the scheme's default
;; port".
(define (split-host-name who name)
  (define (port-of s)
    (let ([n (string->number s 10)])
      (unless (and (fixnum? n) (fx<= 1 n 65534))
        (assertion-violationf who "~s has no valid port" name))
      n))
  (define (after-host host i)
    (cond
      [(fx= i (string-length name)) (values host 65535)]
      [(char=? (string-ref name i) #\:)
       (values host (port-of (substring name (fx+ i 1) (string-length name))))]
      [else (assertion-violationf who "~s is not a host name" name)]))
  (let ([n (string-length name)])
    (if (and (fx> n 0) (char=? (string-ref name 0) #\[))
        (let loop ([i 1])
          (cond
            [(fx= i n) (assertion-violationf who "~s has no closing bracket" name)]
            [(char=? (string-ref name i) #\]) (after-host (substring name 1 i) (fx+ i 1))]
            [else (loop (fx+ i 1))]))
        (let loop ([i 0])
          (if (or (fx= i n) (char=? (string-ref name i) #\:))
              (if (fx= i 0)
                  (assertion-violationf who "~s is not a host name" name)
                  (after-host (substring name 0 i) i))
              (loop (fx+ i 1)))))))

(define-record-type host
  (nongenerative) (sealed #t)
  (fields names routes)
  (protocol
    (lambda (new)
      (lambda (names routes)
        (unless (and (list? names) (pair? names) (for-all string? names))
          (assertion-violationf 'make-host "~s is not a list of host names" names))
        (for-each
          (lambda (name) (call-with-values (lambda () (split-host-name 'make-host name)) list))
          names)
        (unless (and (list? routes) (for-all route? routes))
          (assertion-violationf 'make-host "~s is not a list of routes" routes))
        (let loop ([paths (map route-path routes)])
          (when (pair? paths)
            (when (member (car paths) (cdr paths))
              (assertion-violationf 'make-host "path ~s has two routes" (car paths)))
            (loop (cdr paths))))
        (new names routes)))))

(define-record-type handler-item
  (nongenerative) (sealed #t)
  (fields kind procedure))

(define-record-type files-item
  (nongenerative) (sealed #t)
  (fields directory))

(define-record-type compress-item
  (nongenerative) (sealed #t)
  (fields encodings))

(define-record-type max-age-item
  (nongenerative) (sealed #t)
  (fields seconds))

(define (handler-of who kind proc)
  (unless (procedure? proc) (assertion-violationf who "~s is not a procedure" proc))
  (make-handler-item kind proc))

(define (handler proc) (handler-of 'handler 'worker proc))
(define (streaming-handler proc) (handler-of 'streaming-handler 'streaming proc))
(define (loop-handler proc) (handler-of 'loop-handler 'loop proc))

(define (files directory)
  (unless (and (string? directory) (fx> (string-length directory) 0))
    (assertion-violationf 'files "~s is not a directory name" directory))
  (make-files-item directory))

(define-enumeration encoding (zstd br gzip) encodings)

(define compress
  (case-lambda
    [() (compress (encodings zstd br gzip))]
    [(set)
     (unless (and (enum-set? set)
                  (enum-set-subset? set (encodings zstd br gzip))
                  (pair? (enum-set->list set)))
       (assertion-violationf 'compress "~s is not a non-empty set of encodings" set))
     (make-compress-item set)]))

(define (encoding-enabled? set name)
  (enum-set-member? name set))

(define (max-age seconds)
  (unless (and (integer? seconds) (exact? seconds) (<= 0 seconds) (< seconds (expt 2 64)))
    (assertion-violationf 'max-age "~s is not a number of seconds" seconds))
  (make-max-age-item seconds))

(define-record-type (route-spec route route?)
  (nongenerative) (sealed #t)
  (fields (immutable path route-path) (immutable handlers route-handlers)
          (immutable compress route-compress) (immutable max-age route-max-age))
  (protocol
    (lambda (new)
      (lambda (path . items)
        (unless (and (string? path) (fx> (string-length path) 0)
                     (char=? (string-ref path 0) #\/))
          (assertion-violationf 'route "~s is not an absolute path" path))
        (let loop ([items items] [handlers '()] [compression #f] [expiry #f])
          (cond
            [(null? items)
             (when (null? handlers)
               (assertion-violationf 'route "route ~s has no handler" path))
             (new path (reverse handlers) (and compression (compress-item-encodings compression))
               (and expiry (max-age-item-seconds expiry)))]
            ;; H2O tries the handlers in order and only files passes a
            ;; request on.
            [(or (handler-item? (car items)) (files-item? (car items)))
             (when (and (pair? handlers) (handler-item? (car handlers)))
               (assertion-violationf 'route "~s follows a handler in route ~s and is never reached"
                 (car items) path))
             (when (and (pair? handlers) (handler-item? (car items))
                        (eq? (handler-item-kind (car items)) 'streaming))
               (assertion-violationf 'route
                 "~s follows files in route ~s and would get the whole body at once: H2O streams a request body only to a route's first handler"
                 (car items) path))
             (loop (cdr items) (cons (car items) handlers) compression expiry)]
            [(compress-item? (car items))
             (when compression (assertion-violationf 'route "route ~s compresses twice" path))
             (loop (cdr items) handlers (car items) expiry)]
            [(max-age-item? (car items))
             (when expiry (assertion-violationf 'route "route ~s has two max-ages" path))
             (loop (cdr items) handlers compression (car items))]
            [else (assertion-violationf 'route "~s is not a route item" (car items))]))))))
