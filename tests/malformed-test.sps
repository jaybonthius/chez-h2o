(import (chezscheme) (check) (support) (h2o))

(define port 18451)
(define host (format "127.0.0.1:~a" port))
(define handled (make-log))

(define (count! request)
  (log! handled (request-authority request))
  (response 204 '() #vu8()))

(define (read-all request)
  (get-bytevector-all (request-body-port request))
  (response 204 '() #vu8()))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list host (format "localhost:~a" port))
            (list (route "/commands" (handler count!)) (route "/stats" (handler count!))
                  (route "/slow" (streaming-handler read-all)))))
    (server-options [strict-hosts #t] [http1-request-timeout 1000] [on-error (lambda (c) (void))])))

(define (crlf . lines)
  (string->utf8 (apply string-append (map (lambda (line) (string-append line "\r\n")) lines))))

(define (post-with . headers)
  (bytevector-append (apply crlf "POST /commands HTTP/1.1" (append headers '(""))) (string->utf8 "abc")))

(define (bytevector-append . bvs)
  (let ([out (make-bytevector (apply + (map bytevector-length bvs)))])
    (let loop ([bvs bvs] [at 0])
      (if (null? bvs)
          out
          (begin
            (bytevector-copy! (car bvs) 0 out at (bytevector-length (car bvs)))
            (loop (cdr bvs) (+ at (bytevector-length (car bvs)))))))))

(define h (string-append "Host: " host))

;; Cases marked "accepted" are framing H2O lets through under every
;; setting.
(define cases
  `(("valid POST" ,(post-with h "Content-Length: 3") "HTTP/1.1 204 No Content" #t)
    ("missing Host (HTTP/1.1)" ,(post-with "Content-Length: 3") "HTTP/1.1 404 Not Found" #f)
    ("duplicate Host, accepted" ,(post-with h h "Content-Length: 3") "HTTP/1.1 204 No Content" #t)
    ("valid Host then foreign Host" ,(post-with h "Host: evil.example" "Content-Length: 3")
     "HTTP/1.1 404 Not Found" #f)
    ("foreign Host then valid Host, accepted" ,(post-with "Host: evil.example" h "Content-Length: 3")
     "HTTP/1.1 204 No Content" #t)
    ("foreign Host" ,(post-with "Host: evil.example" "Content-Length: 3") "HTTP/1.1 404 Not Found" #f)
    ("Content-Length: abc" ,(post-with h "Content-Length: abc") "HTTP/1.1 400 Invalid Request" #f)
    ("Content-Length: -1" ,(post-with h "Content-Length: -1") "HTTP/1.1 400 Invalid Request" #f)
    ("Content-Length: 3, 3" ,(post-with h "Content-Length: 3, 3") "HTTP/1.1 400 Invalid Request" #f)
    ("two Content-Lengths 3 and 4, accepted" ,(post-with h "Content-Length: 3" "Content-Length: 4")
     "HTTP/1.1 204 No Content" #t)
    ("Content-Length with chunked Transfer-Encoding, accepted"
     ,(bytevector-append (crlf "POST /commands HTTP/1.1" h "Content-Length: 3" "Transfer-Encoding: chunked" "")
        (crlf "3" "abc" "0" ""))
     "HTTP/1.1 204 No Content" #t)
    ("Content-Length of 10 GB" ,(post-with h "Content-Length: 10000000000") "HTTP/1.1 413 Request Entity Too Large" #f)
    ("folded header line" ,(post-with h "X-A: a" " b" "Content-Length: 3") "HTTP/1.1 400 Bad Request" #f)
    ("space before colon" ,(post-with (string-append "Host : " host) "Content-Length: 3") "HTTP/1.1 400 Bad Request" #f)
    ("bare LF line ends, accepted"
     ,(string->utf8 (format "POST /commands HTTP/1.1\nHost: ~a\nContent-Length: 3\n\nabc" host))
     "HTTP/1.1 204 No Content" #t)
    ("NUL in a header value" ,(post-with h "X-A: a\x0;b" "Content-Length: 3") "HTTP/1.1 400 Bad Request" #f)
    ("HTTP/1.0 without Host" ,(bytevector-append (crlf "POST /commands HTTP/1.0" "Content-Length: 3" "") (string->utf8 "abc"))
     "HTTP/1.1 404 Not Found" #f)
    ("HTTP/2.0 request line" ,(crlf "GET /stats HTTP/2.0" h "") "HTTP/1.1 400 Bad Request" #f)
    ("garbage request line" ,(crlf "HELLO" "") "HTTP/1.1 400 Bad Request" #f)
    ("one 100 KB header, accepted" ,(crlf "GET /stats HTTP/1.1" h (string-append "X-Big: " (make-string 100000 #\a)) "")
     "HTTP/1.1 204 No Content" #t)
    ("1,000 small headers"
     ,(apply crlf "GET /stats HTTP/1.1" h (append (map (lambda (i) (format "X-~a: v" i)) (iota 1000)) '("")))
     "HTTP/1.1 400 Bad Request" #f)
    ("cleartext HTTP/2 preface" ,(string->utf8 "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n") "HTTP/1.1 400 Bad Request" #f)))

(for-each
  (lambda (c)
    (let ([name (car c)] [bytes (cadr c)] [answer (caddr c)] [ran? (cadddr c)])
      (test name
        (let ([before (length (log-items handled))])
          (check-equal? answer (raw-request port bytes))
          (check-equal? ran? (wait-until 100 (lambda () (> (length (log-items handled)) before))))))))
  cases)

(test "the handler sees only the last of two valid Hosts"
  (let ([before (length (log-items handled))])
    (raw-request port (post-with (format "Host: localhost:~a" port) h "Content-Length: 3"))
    (check-equal? (list host) (list-tail (log-items handled) before))))

(define (slow-client mode seconds)
  (let ([words (split-words (run "python3" "tests/slow-client.py" (number->string port) mode seconds))])
    (values (car words) (string->number (caddr words)))))

(test "a head sent one byte at a time is cut off at http1-request-timeout"
  (let-values ([(end seconds) (slow-client "head" "5")])
    (check-equal? "closed" end)
    (check (< 0.9 seconds 1.5))))

(test "a body sent one byte at a time is not cut off: each part restarts the timers"
  (let-values ([(end seconds) (slow-client "body" "2")])
    (check-equal? "open" end)))

(server-stop! server)

(test-exit)
