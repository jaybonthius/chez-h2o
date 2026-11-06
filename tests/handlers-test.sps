(import (chezscheme) (check) (support) (h2o))

(define port 18461)
(define tls-port 18462)
(define v6-port 18463)
(define base (format "http://127.0.0.1:~a" port))
(define tls-base (format "https://localhost:~a" tls-port))
(define certificate (string-append scratch "/cert.pem"))
(define key (string-append scratch "/key.pem"))

(run "openssl" "req" "-x509" "-newkey" "ec" "-pkeyopt" "ec_paramgen_curve:prime256v1"
  "-nodes" "-days" "1" "-subj" "/CN=localhost"
  "-addext" "subjectAltName=DNS:localhost,IP:127.0.0.1"
  "-keyout" key "-out" certificate)

(define (tls-curl . arguments) (apply curl "--cacert" certificate arguments))

(define errors (make-log))
(define arrivals (make-log))
(define late-arrivals (make-log))
(define closes (make-log))
(define streams (make-log))
(define bodies (make-log))
(define slept (make-log))

(define hints '(("link" . "</style.css>; rel=preload; as=style")))

(define (text s) (response 200 '(("content-type" . "text/plain")) s))

(define (late-stream request)
  (log! late-arrivals 'late)
  (sleep-ms 500)
  (let ([stream (make-stream (lambda () (log! closes 'late)))])
    (stream-send! stream "data: late\n\n")
    (stream-response 200 '(("content-type" . "text/event-stream")) stream)))

(define (read-body-outcome request)
  (guard (c [(i/o-read-error? c) 'aborted])
    (bytevector-length (get-bytevector-all (request-body-port request)))))

(define (outcome thunk)
  (guard (c [(assertion-violation? c) "raised"]) (thunk) "returned"))

(define routes
  (list
    (route "/sleep" (handler (lambda (r) (sleep-ms 1000) (log! slept 'done) (text "slept"))))
    (route "/quick" (handler (lambda (r) (text "quick"))))
    (route "/order" (loop-handler (lambda (r) (log! arrivals (request-query r)) (text "ok"))))
    (route "/late" (handler late-stream))
    (route "/client"
      (loop-handler
        (lambda (r)
          (text (format "~a ~a ~a" (request-client-address r) (request-client-port r)
                  (request-scheme r))))))
    (route "/hints" (handler (lambda (r) (send-early-hints! r hints) (text "final"))))
    (route "/hints-on-loop" (loop-handler (lambda (r) (send-early-hints! r hints) (text "final"))))
    (route "/stuck"
      (loop-handler
        (lambda (r)
          (let ([stream (make-stream (lambda () (log! closes 'stuck)))])
            (log! streams stream)
            (stream-response 200 '(("content-type" . "text/event-stream")) stream)))))
    (route "/over-limit"
      (handler
        (lambda (r)
          (let ([stream (make-stream (lambda () (log! closes 'over-limit)))])
            (stream-send! stream (make-bytevector 100000 65))
            (stream-response 200 '() stream)))))
    (route "/lazy"
      (streaming-handler
        (lambda (r)
          (sleep-ms 1000)
          (text (number->string (bytevector-length (get-bytevector-all (request-body-port r))))))))
    (route "/first-read"
      (streaming-handler
        (lambda (r)
          (get-bytevector-some (request-body-port r))
          (sleep-ms 5)
          (text "first part read"))))
    (route "/abort"
      (streaming-handler (lambda (r) (log! bodies (read-body-outcome r)) (text "never seen"))))
    (route "/body-in-streaming"
      (streaming-handler (lambda (r) (text (outcome (lambda () (request-body r)))))))
    (route "/port-in-handler"
      (handler (lambda (r) (text (outcome (lambda () (request-body-port r)))))))))

(define (names p) (list (format "127.0.0.1:~a" p) (format "localhost:~a" p)))

(define server
  (start-server
    (list (http-listener "127.0.0.1" port) (https-listener "127.0.0.1" tls-port certificate key))
    (list (make-host (append (names port) (names tls-port)) routes))
    (server-options
      [workers 4]
      [max-stream-buffer 65536]
      [max-request-body-size 100000000]
      [on-error (lambda (c) (log! errors c))])))

(test "a handler that sleeps 1 s does not delay another request"
  (let ([sleeper (in-background (lambda () (curl (string-append base "/sleep"))))])
    (sleep-ms 100)
    (let ([start (now-ms)])
      (check-equal? "quick" (reply-body (curl (string-append base "/quick"))))
      (check (< (- (now-ms) start) 500)))
    (check-equal? "slept" (reply-body (sleeper)))))

(test "200 requests on one HTTP/2 connection reach a loop-handler in order"
  (let ([config (scratch-file "curl-config")] [before (length (log-items arrivals))])
    (write-file! config
      (string->utf8
        (apply string-append
          (map (lambda (i) (format "url = \"~a/order?~a\"\noutput = /dev/null\n" tls-base i))
               (iota 200)))))
    (let ([connects (map string->number
                      (split-words
                        (run "curl" "-s" "--http2" "--parallel" "--parallel-max" "100"
                          "--cacert" certificate "-w" "%{num_connects}\n" "-K" config)))])
      (check-equal? 200 (length connects))
      (check-equal? 1 (apply + connects)))
    (check-equal? (map number->string (iota 200)) (list-tail (log-items arrivals) before))))

(define (start-killable . words)
  (let ([pid-file (scratch-file "pid")])
    (system (format "~{~a~^ ~} > /dev/null 2>&1 & echo $! > ~a" (map shell-quote words) pid-file))
    (string->number (string-trim (read-text pid-file)))))

(test "a response whose client has gone is dropped, and its stream is closed"
  (let ([before (length (log-items closes))]
        [client (start-killable "curl" "-s" "--http2" "--cacert" certificate "--max-time" "10"
                  (string-append tls-base "/late"))])
    (check (wait-until 5000 (lambda () (pair? (log-items late-arrivals)))))
    (system (format "kill ~a" client))
    (check (wait-until 2000 (lambda () (> (length (log-items closes)) before))))
    (check-equal? 'late (car (last-pair (log-items closes))))
    (check-equal? "quick" (reply-body (tls-curl "--http2" (string-append tls-base "/quick"))))
    (check-equal? '() (log-items errors))))

(test "when on-error raises, both conditions are printed to standard error, in order"
  (let* ([output (run (getenv "SCHEME") "--libdirs" "src::build/obj/src" "--script" "tests/run.ss"
                   "build/obj/tests/on-error-child.so" "18464")]
         [first (string-search output "the handler failed")]
         [second (string-search output "on-error failed too")])
    (check first)
    (check second)
    (check (and first second (< first second)))))

(test "a misspelled option fails to compile, naming the form"
  (let ([good (scratch-file "good-options.ss")] [bad (scratch-file "bad-options.ss")])
    (write-file! good
      (string->utf8 "(import (chezscheme) (h2o))\n(server-options [workers 4] [strict-hosts #t])\n"))
    (write-file! bad (string->utf8 "(import (chezscheme) (h2o))\n(server-options [wokers 4])\n"))
    (parameterize ([compile-file-message #f])
      (compile-program good (scratch-file "good-options.so"))
      (check-raises
        (lambda (c)
          (and (syntax-violation? c)
               (eq? 'server-options (condition-who c))
               (string=? "unknown option" (condition-message c))
               (eq? 'wokers (syntax->datum (syntax-violation-subform c)))))
        (compile-program bad (scratch-file "bad-options.so"))))))

(test "an option given twice fails to compile"
  (check-raises syntax-violation?
    (expand '(server-options [workers 4] [workers 5]) (environment '(chezscheme) '(h2o)))))

(test "the client's address, port and scheme come with the request"
  (let ([words (split-words (reply-body (curl (string-append base "/client"))))])
    (check-equal? "127.0.0.1" (car words))
    (check (< 0 (string->number (cadr words)) 65536))
    (check (not (= port (string->number (cadr words)))))
    (check-equal? "http" (caddr words)))
  (check-equal? "https" (caddr (split-words (reply-body (tls-curl (string-append tls-base "/client")))))))

(test "a client's IPv6 address comes with the request, where ::1 is available"
  (let ([v6 (guard (c [(error? c) #f])
              (start-server (list (http-listener "::1" v6-port))
                (list (make-host (list (format "[::1]:~a" v6-port)) routes))))])
    (cond
      [v6
       (check-equal? "::1"
         (car (split-words (reply-body (curl (format "http://[::1]:~a/client" v6-port))))))
       (server-stop! v6)]
      [else (printf "skipped: cannot listen on ::1\n")])))

(define (hints-request path)
  (string->utf8 (format "GET ~a HTTP/1.1\r\nHost: 127.0.0.1:~a\r\nConnection: close\r\n\r\n" path port)))

(test "early hints arrive before the final response, from a worker or the loop"
  (check-equal? "HTTP/1.1 103 Early Hints, then 200" (raw-request port (hints-request "/hints")))
  (check-equal? "HTTP/1.1 103 Early Hints, then 200" (raw-request port (hints-request "/hints-on-loop")))
  (let ([head (scratch-file "head")])
    (run "curl" "-s" "--http2" "--cacert" certificate "-D" head "-o" "/dev/null"
      (string-append tls-base "/hints"))
    (let ([lines (split-lines (read-text head))])
      (check-equal? '("HTTP/2 103" "HTTP/2 200")
        (map string-trim (filter (lambda (line) (string-contains? line "HTTP/2")) lines)))
      (check (exists (lambda (line) (string-contains? line "rel=preload")) lines)))))

(test "send-early-hints! checks its request"
  (check-raises assertion-violation? (send-early-hints! 'not-a-request hints)))

(test "a stream belongs to one response"
  (let ([stream (make-stream)])
    (stream-response 200 '() stream)
    (check-raises assertion-violation? (stream-response 200 '() stream))))

(test "a stream whose unsent data passes max-stream-buffer closes"
  (let* ([before (length (log-items streams))]
         [client (in-background
                   (lambda () (run "python3" "tests/body-client.py" (number->string port) "stuck" "2")))]
         [stream (and (wait-until 5000 (lambda () (> (length (log-items streams)) before)))
                      (list-ref (log-items streams) before))]
         [chunk (make-string 32768 #\x)])
    (let ([refused-at (let loop ([i 0])
                        (cond
                          [(= i 4000) #f]
                          [(stream-send! stream chunk) (sleep-ms 1) (loop (+ i 1))]
                          [else i]))])
      (printf "refused after ~a sends of 32 KB\n" refused-at)
      (check (and refused-at (< refused-at 200))))
    (check (not (stream-open? stream)))
    (check-equal? #f (stream-send! stream "more"))
    (client)
    (check (wait-until 3000 (lambda () (memq 'stuck (log-items closes)))))))

(test "a stream already over max-stream-buffer when its response is made is refused"
  (let ([before (length (log-items errors))])
    (check-equal? 500 (reply-status (curl (string-append base "/over-limit"))))
    (check (memq 'over-limit (log-items closes)))
    (check-equal? "the stream of a stream-response holds more than max-stream-buffer"
      (condition-message (car (last-pair (log-items errors)))))
    (check-equal? (+ before 1) (length (log-items errors)))))

(test "a streaming handler that does not read holds the client back"
  (let ([words (split-words (run "python3" "tests/body-client.py" (number->string port)
                              "push" "64000000"))])
    (printf "client sent ~a bytes in the first 0.5 s of 64000000\n" (car words))
    (check (< (string->number (car words)) 32000000))
    (check-equal? "64000000" (cadr words))
    (check-equal? "200" (cadddr words))))

(test "malformed chunked bodies to a streaming handler get 200 or 400 and the server keeps serving"
  (let ([answers (map (lambda (entry) (car (split-on entry (lambda (c) (char=? c #\:)))))
                      (split-on (run "python3" "tests/body-client.py" (number->string port)
                                  "bad-chunk" "200")
                        (lambda (c) (char=? c #\|))))])
    (printf "answers to 200 malformed chunked bodies: ~s\n" answers)
    (check (pair? answers))
    (check (for-all (lambda (a) (member (string-trim a) '("HTTP/1.1 200 OK" "HTTP/1.1 400 Invalid Request")))
             answers)))
  (check-equal? "quick" (reply-body (curl (string-append base "/quick")))))

(test "a body the client aborts makes the next read raise an i/o condition"
  (let ([before (length (log-items bodies))])
    (run "python3" "tests/body-client.py" (number->string port) "abort" "1000")
    (check (wait-until 2000 (lambda () (> (length (log-items bodies)) before))))
    (check-equal? 'aborted (car (last-pair (log-items bodies))))))

(test "request-body raises in a streaming handler; request-body-port raises elsewhere"
  (check-equal? "raised" (reply-body (curl "--data-binary" "x" (string-append base "/body-in-streaming"))))
  (check-equal? "raised" (reply-body (curl "--data-binary" "x" (string-append base "/port-in-handler")))))

(server-stop! server)

(test-exit)
