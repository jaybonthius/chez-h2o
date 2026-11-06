(import (chezscheme) (check) (support) (h2o))

(define tls-port 18421)
(define plain-port 18422)
(define certificate (string-append scratch "/cert.pem"))
(define key (string-append scratch "/key.pem"))

(run "openssl" "req" "-x509" "-newkey" "ec" "-pkeyopt" "ec_paramgen_curve:prime256v1"
  "-nodes" "-days" "1" "-subj" "/CN=localhost"
  "-addext" "subjectAltName=DNS:localhost,IP:127.0.0.1"
  "-keyout" key "-out" certificate)

(define streams (make-log))

(define (describe request)
  (response 200 '(("content-type" . "text/plain"))
    (format "~s ~a ~a ~a ~a" (request-version request) (request-scheme request)
      (request-authority request) (request-method request) (utf8->string (request-body request)))))

(define uploads (make-log))

(define (upload request)
  (let ([port (request-body-port request)])
    (let loop ([reads 0] [bytes 0])
      (let ([part (get-bytevector-some port)])
        (if (eof-object? part)
            (begin
              (log! uploads reads)
              (response 200 '() (format "~s ~a bytes" (request-version request) bytes)))
            (loop (+ reads 1) (+ bytes (bytevector-length part))))))))

(define (events request)
  (let ([stream (make-stream)])
    (log! streams stream)
    (stream-response 200 '(("content-type" . "text/event-stream")) stream)))

(define (names p) (list (format "localhost:~a" p) (format "127.0.0.1:~a" p)))

(define server
  (start-server
    (list (https-listener "127.0.0.1" tls-port certificate key)
          (http-listener "127.0.0.1" plain-port))
    (list (make-host (append (names tls-port) (names plain-port))
            (list (route "/describe" (handler describe))
                  (route "/upload" (streaming-handler upload))
                  (route "/events" (loop-handler events))
                  (route "/cookies"
                    (loop-handler (lambda (r) (response 200 '() (format "~s" (request-cookies r)))))))))))

(define tls-base (format "https://localhost:~a" tls-port))
(define plain-base (format "http://127.0.0.1:~a" plain-port))

(define (tls-curl . arguments) (apply curl "--cacert" certificate arguments))

(test "curl --http2 over TLS negotiates HTTP/2 through ALPN"
  (let ([r (tls-curl "--http2" (string-append tls-base "/describe"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "2" (reply-version r))
    (check-equal? (format "http/2 https localhost:~a GET " tls-port) (reply-body r))))

(test "HTTP/1.1 over TLS still works"
  (let ([r (tls-curl "--http1.1" (string-append tls-base "/describe"))])
    (check-equal? "1.1" (reply-version r))
    (check-equal? (format "http/1.1 https localhost:~a GET " tls-port) (reply-body r))))

(test "a POST body over HTTP/2 arrives whole"
  (check-equal? (format "http/2 https localhost:~a POST over h2" tls-port)
    (reply-body (tls-curl "--http2" "--data-binary" "over h2" (string-append tls-base "/describe")))))

(test "a streamed request body over HTTP/2 arrives in parts"
  (let ([big (string-append scratch "/big")])
    (write-file! big (make-bytevector 3000000 67))
    (check-equal? "http/2 3000000 bytes"
      (reply-body (tls-curl "--http2" "--limit-rate" "5M" "--data-binary" (string-append "@" big)
                    (string-append tls-base "/upload")))))
  (check (> (car (log-items uploads)) 1)))

(test "a stream over HTTP/2 gets pushes from another thread"
  (let* ([client (in-background (lambda () (tls-curl "--http2" "-N" (string-append tls-base "/events"))))]
         [stream (and (wait-until 5000 (lambda () (pair? (log-items streams))))
                      (car (log-items streams)))])
    (fork-thread
      (lambda ()
        (for-each (lambda (i) (stream-send! stream (format "data: ~a\n\n" i)) (sleep-ms 2)) (iota 20))
        (stream-close! stream)))
    (let ([r (client)])
      (check-equal? "2" (reply-version r))
      (check-equal? (apply string-append (map (lambda (i) (format "data: ~a\n\n" i)) (iota 20)))
        (reply-body r)))))

(test "a client that resets its HTTP/2 stream is noticed without a write"
  (let* ([before (length (log-items streams))]
         [client (in-background
                   (lambda () (tls-curl "--http2" "-N" "--max-time" "1" (string-append tls-base "/events"))))])
    (check (wait-until 5000 (lambda () (> (length (log-items streams)) before))))
    (let ([stream (list-ref (log-items streams) before)])
      (client)
      (check (wait-until 1000 (lambda () (not (stream-open? stream))))))))

(test "cookies split across HTTP/2 header fields are all read"
  (check-equal? "((\"a\" . \"1\") (\"b\" . \"2\") (\"c\" . \"3\"))"
    (reply-body (tls-curl "--http2" "-H" "Cookie: a=1; b=2" "-H" "Cookie: c=3"
                  (string-append tls-base "/cookies")))))

(test "cleartext HTTP/2 is not offered: upgrade requests stay on HTTP/1.1"
  (check-equal? "1.1" (reply-version (curl "--http2" (string-append plain-base "/describe"))))
  (check-equal? "http/1.1" (car (split-words (reply-body (curl "--http2" (string-append plain-base "/describe")))))))

(test "a missing certificate fails the start with the file named"
  (check-raises
    (lambda (c) (and (error? c) (string-contains? (condition-message c) "cannot load TLS certificate")))
    (start-server (list (https-listener "127.0.0.1" 18423 (string-append scratch "/none.pem") key))
      (list (make-host '("localhost") '())))))

(server-stop! server)

(test-exit)
