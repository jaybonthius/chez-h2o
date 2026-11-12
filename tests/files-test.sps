(import (chezscheme) (check) (support) (h2o))

(define port 18471)
(define base (format "http://127.0.0.1:~a" port))
(define www (string-append scratch "/www"))
(define streams (make-log))

(make-directories! (string-append www "/sub"))
(write-file! (string-append www "/index.html") (string->utf8 "<p>index</p>\n"))
(write-file! (string-append www "/sub/plain.txt") (string->utf8 "plain\n"))
(for-each
  (lambda (suffix) (write-file! (string-append www "/p.txt" suffix) (string->utf8 (string-append "variant" suffix))))
  '("" ".br" ".zst" ".gz"))

(define long-text
  (apply string-append (map (lambda (i) (format "line ~a of a long text body\n" i)) (iota 200))))

;; A text that repeats a pattern can come out smaller at a lower quality,
;; so these words follow a pseudo-random sequence.
(define varied-text
  (let ([words '#("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa"
                  "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma" "tau" "upsilon" "phi"
                  "chi" "psi" "omega")])
    (let loop ([i 0] [x 1] [acc '()])
      (if (= i 3200)
          (apply string-append (reverse acc))
          (let ([x (mod (+ (* x 1103515245) 12345) 2147483648)])
            (loop (+ i 1) x
              (cons* (if (= (mod i 8) 7) "\n" " ")
                     (vector-ref words (mod (quotient x 65536) (vector-length words)))
                     acc)))))))

(define (text s) (response 200 '(("content-type" . "text/plain")) s))

(define (events request)
  (let ([stream (make-stream)])
    (log! streams stream)
    (stream-response 200 '(("content-type" . "text/event-stream")) stream)))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list
      (make-host (list (format "127.0.0.1:~a" port) (format "localhost:~a" port))
        (list
          (route "/static" (files www) (handler (lambda (r) (text (string-append "fallback " (request-path r))))))
          (route "/only-files" (files www))
          (route "/text" (handler (lambda (r) (text long-text))) (compress))
          (route "/gzip-only" (handler (lambda (r) (text long-text))) (compress (encodings gzip)))
          (route "/varied" (handler (lambda (r) (text varied-text))) (compress))
          (route "/best" (handler (lambda (r) (text varied-text)))
            (compress (encodings zstd br gzip) (qualities [zstd 19] [br 11] [gzip 9])))
          (route "/best-br" (handler (lambda (r) (text varied-text)))
            (compress (encodings zstd br gzip) (qualities (br 11))))
          (route "/plain-text" (handler (lambda (r) (text long-text))))
          (route "/events" (loop-handler events) (compress))
          (route "/cached" (handler (lambda (r) (text "cached"))) (max-age 60))
          (route "/cached-missing"
            (handler (lambda (r) (response 404 '() "missing"))) (max-age 60))
          (route "/cached-files" (files www) (max-age 3600)))))))

(test "files serves a directory with its index.html"
  (let ([r (curl (string-append base "/static/sub/plain.txt"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "plain\n" (reply-body r))
    (check-equal? "text/plain" (reply-header r "content-type")))
  (check-equal? "<p>index</p>\n" (reply-body (curl (string-append base "/static/")))))

(test "a missing file falls through to the next handler"
  (check-equal? "fallback /static/missing.txt" (reply-body (curl (string-append base "/static/missing.txt"))))
  (check-equal? "fallback /static/sub/" (reply-body (curl (string-append base "/static/sub/")))))

(test "a missing file with nothing after files is a 404"
  (check-equal? 404 (reply-status (curl (string-append base "/only-files/missing.txt")))))

(test "a precompressed file is chosen by Accept-Encoding"
  (define (fetch encoding)
    (let ([r (curl "-H" (string-append "Accept-Encoding: " encoding)
               (string-append base "/static/p.txt"))])
      (list (reply-header r "content-encoding") (reply-body r))))
  (check-equal? '("br" "variant.br") (fetch "br"))
  (check-equal? '("zstd" "variant.zst") (fetch "zstd"))
  (check-equal? '("gzip" "variant.gz") (fetch "gzip"))
  (check-equal? '(#f "variant") (fetch "identity"))
  (check-equal? "accept-encoding"
    (reply-header (curl "-H" "Accept-Encoding: gzip" (string-append base "/static/p.txt")) "vary")))

(define (compressed path encoding)
  (curl "--compressed" "-H" (string-append "Accept-Encoding: " encoding) (string-append base path)))

(test "compress encodes a body with gzip, br or zstd, and curl decodes each"
  (for-each
    (lambda (encoding)
      (let ([r (compressed "/text" encoding)])
        (check-equal? encoding (reply-header r "content-encoding"))
        (check-equal? long-text (reply-body r))))
    '("gzip" "br" "zstd")))

(test "compress uses only the encodings it is given"
  (check-equal? #f (reply-header (compressed "/gzip-only" "br") "content-encoding"))
  (check-equal? "gzip" (reply-header (compressed "/gzip-only" "gzip, br") "content-encoding"))
  (check-equal? #f (reply-header (compressed "/plain-text" "gzip") "content-encoding")))

(define (size-sent path encoding)
  (string->number
    (run "curl" "-s" "--max-time" "10" "-o" "/dev/null" "-w" "%{size_download}"
      "-H" (string-append "Accept-Encoding: " encoding) (string-append base path))))

(test "compress uses the quality given for each encoding and the default for the rest"
  (for-each
    (lambda (encoding)
      (check (< (size-sent "/best" encoding) (size-sent "/varied" encoding)))
      (let ([r (compressed "/best" encoding)])
        (check-equal? encoding (reply-header r "content-encoding"))
        (check-equal? varied-text (reply-body r))))
    '("gzip" "br" "zstd"))
  (check-equal? (size-sent "/best" "br") (size-sent "/best-br" "br"))
  (check-equal? (size-sent "/varied" "gzip") (size-sent "/best-br" "gzip"))
  (check-equal? (size-sent "/varied" "zstd") (size-sent "/best-br" "zstd")))

(test "compress encodes an event stream as it is sent"
  (for-each
    (lambda (encoding)
      (let* ([before (length (log-items streams))]
             [client (in-background (lambda () (curl "-N" "--compressed" "-H"
                                                  (string-append "Accept-Encoding: " encoding)
                                                  (string-append base "/events"))))])
        (check (wait-until 5000 (lambda () (> (length (log-items streams)) before))))
        (let ([stream (list-ref (log-items streams) before)])
          (for-each (lambda (i) (stream-send! stream (format "data: event ~a\n\n" i)) (sleep-ms 2))
            (iota 20))
          (stream-close! stream))
        (let ([r (client)])
          (check-equal? encoding (reply-header r "content-encoding"))
          (check-equal? (apply string-append (map (lambda (i) (format "data: event ~a\n\n" i)) (iota 20)))
            (reply-body r)))))
    '("gzip" "br" "zstd")))

(test "max-age adds Cache-Control to successful responses of its route"
  (check-equal? "max-age=60" (reply-header (curl (string-append base "/cached")) "cache-control"))
  (check-equal? #f (reply-header (curl (string-append base "/cached-missing")) "cache-control"))
  (check-equal? "max-age=3600"
    (reply-header (curl (string-append base "/cached-files/sub/plain.txt")) "cache-control")))

(server-stop! server)

(test-exit)
