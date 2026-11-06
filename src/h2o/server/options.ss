;; An H2O field left #f keeps H2O's own default.
(define-record-type options
  (nongenerative) (sealed #t)
  (fields workers strict-hosts on-error shutdown-timeout max-request-body-size
          max-stream-buffer handshake-timeout http1-request-timeout http1-request-io-timeout
          http2-idle-timeout http2-graceful-shutdown-timeout http2-max-streams
          http2-max-concurrent-requests http2-max-concurrent-streaming-requests))

(define omitted (list 'omitted))

(define (count? n) (and (integer? n) (exact? n) (>= n 0)))
(define (u32? n) (and (count? n) (< n (expt 2 32))))
(define (u64? n) (and (count? n) (< n (expt 2 64))))
(define (positive-fixnum? n) (and (fixnum? n) (fx> n 0)))

;; Chez closes a port's descriptor when the collector reclaims the port,
;; so the one port on descriptor 2 is kept for the life of the process.
(define error-port (standard-error-port (buffer-mode block) (native-transcoder)))
(define error-port-mutex (make-mutex))

(define (report-to-standard-error condition)
  (with-mutex error-port-mutex
    (put-string error-port "h2o: ")
    (if (condition? condition)
        (display-condition condition error-port)
        (write condition error-port))
    (newline error-port)
    (flush-output-port error-port)))

(define (option name value valid? default)
  (cond
    [(eq? value omitted) default]
    [(valid? value) value]
    [else (assertion-violationf 'server-options "~s is not a valid value for ~s" value name)]))

(define (checked-options workers strict-hosts on-error shutdown-timeout max-request-body-size
          max-stream-buffer handshake-timeout http1-request-timeout http1-request-io-timeout
          http2-idle-timeout http2-graceful-shutdown-timeout http2-max-streams
          http2-max-concurrent-requests http2-max-concurrent-streaming-requests)
  (make-options
    (option 'workers workers positive-fixnum? 16)
    (option 'strict-hosts strict-hosts boolean? #f)
    (option 'on-error on-error procedure? report-to-standard-error)
    (option 'shutdown-timeout shutdown-timeout count? 5000)
    (option 'max-request-body-size max-request-body-size u64? #f)
    (option 'max-stream-buffer max-stream-buffer positive-fixnum? (* 4 1024 1024))
    (option 'handshake-timeout handshake-timeout u64? #f)
    (option 'http1-request-timeout http1-request-timeout u64? #f)
    (option 'http1-request-io-timeout http1-request-io-timeout u64? #f)
    (option 'http2-idle-timeout http2-idle-timeout u64? #f)
    (option 'http2-graceful-shutdown-timeout http2-graceful-shutdown-timeout u64? #f)
    (option 'http2-max-streams http2-max-streams u32? #f)
    (option 'http2-max-concurrent-requests http2-max-concurrent-requests u64? #f)
    (option 'http2-max-concurrent-streaming-requests
      http2-max-concurrent-streaming-requests u64? #f)))

(define-syntax server-options
  (lambda (x)
    (define names
      '(workers strict-hosts on-error shutdown-timeout max-request-body-size max-stream-buffer
        handshake-timeout http1-request-timeout http1-request-io-timeout http2-idle-timeout
        http2-graceful-shutdown-timeout http2-max-streams http2-max-concurrent-requests
        http2-max-concurrent-streaming-requests))
    (define (check-names given)
      (let loop ([given given] [seen '()])
        (unless (null? given)
          (let ([name (syntax->datum (car given))])
            (unless (memq name names)
              (syntax-violation 'server-options "unknown option" x (car given)))
            (when (memq name seen)
              (syntax-violation 'server-options "option given twice" x (car given)))
            (loop (cdr given) (cons name seen))))))
    (syntax-case x ()
      [(_ [name value] ...)
       (for-all identifier? #'(name ...))
       (begin
         (check-names #'(name ...))
         (with-syntax ([(temporary ...) (generate-temporaries #'(name ...))])
           (with-syntax ([(argument ...)
                          (let ([given (map cons (syntax->datum #'(name ...)) #'(temporary ...))])
                            (map (lambda (name)
                                   (cond [(assq name given) => cdr] [else #'omitted]))
                                 names))])
             #'(let* ([temporary value] ...) (checked-options argument ...)))))]
      [_ (syntax-violation 'server-options "expected (server-options [name value] ...)" x)])))
