(import (chezscheme) (check) (support) (h2o))

(define port 18441)
(define base (format "http://127.0.0.1:~a" port))
(define streams (make-log))
(define closes (make-log))
(define errors (make-log))

(define (read-all request)
  (get-bytevector-all (request-body-port request))
  (response 204 '() #vu8()))

(define (hosts)
  (list (make-host (list (format "127.0.0.1:~a" port) (format "localhost:~a" port))
          (list
            (route "/hello" (handler (lambda (r) (response 200 '() "hello"))))
            (route "/events"
              (loop-handler
                (lambda (r)
                  (let ([stream (make-stream (lambda () (log! closes 'closed)))])
                    (log! streams stream)
                    (stream-response 200 '(("content-type" . "text/event-stream")) stream)))))
            (route "/stop-self" (handler (lambda (r) (server-stop! the-server))))
            (route "/stop-self-on-loop" (loop-handler (lambda (r) (server-stop! the-server))))
            (route "/sleep" (handler (lambda (r) (log! arrivals 'sleep) (sleep-ms 1000)
                                       (log! slept 'done) (response 200 '() "slept"))))
            (route "/late-stream"
              (handler
                (lambda (r)
                  (log! arrivals 'late-stream)
                  (sleep-ms 1000)
                  (let ([stream (make-stream (lambda () (log! closes 'late-closed)))])
                    (log! streams stream)
                    (stream-response 200 '() stream)))))
            (route "/slow" (streaming-handler read-all))))))

(define the-server #f)
(define slept (make-log))
(define arrivals (make-log))

(define (arrived? route)
  (define (count) (length (filter (lambda (r) (eq? r route)) (log-items arrivals))))
  (let ([before (count)])
    (lambda () (> (count) before))))

(define (start . options)
  (set! the-server
    (start-server (list (http-listener "127.0.0.1" port)) (hosts)
      (if (null? options)
          (server-options [on-error (lambda (c) (log! errors c))])
          (car options))))
  the-server)

(define (slow-client mode seconds)
  (in-background
    (lambda () (run "python3" "tests/slow-client.py" (number->string port) mode seconds))))

(test "stopping ends open streams after their pending data and closes the port"
  (let* ([server (start)]
         [client (in-background (lambda () (curl "-N" (string-append base "/events"))))])
    (check (wait-until 5000 (lambda () (pair? (log-items streams)))))
    (let ([stream (car (log-items streams))])
      (stream-send! stream "data: one\n\n")
      (stream-send! stream "data: two\n\n")
      (server-stop! server)
      (let ([r (client)])
        (check-equal? 200 (reply-status r))
        (check-equal? "data: one\n\ndata: two\n\n" (reply-body r)))
      (check-equal? '(closed) (log-items closes))
      (check (not (stream-open? stream)))
      (check-equal? #f (stream-send! stream "after")))
    (check-equal? 0 (reply-status (curl (string-append base "/hello"))))))

(test "a stopped server's port can be listened on again"
  (let ([server (start)])
    (check-equal? "hello" (reply-body (curl (string-append base "/hello"))))
    (server-stop! server)
    (server-stop! server)))

(test "idle keep-alive connections are closed at once"
  (let* ([server (start)] [client (slow-client "idle" "10")])
    (sleep-ms 300)
    (let ([start (now-ms)])
      (server-stop! server)
      (check (< (- (now-ms) start) 500)))
    (check (string-contains? (client) "closed"))))

(test "a slow request body is cut off after shutdown-timeout"
  (let* ([server (start (server-options [on-error (lambda (c) (log! errors c))]
                                    [shutdown-timeout 300]))] [client (slow-client "body" "10")])
    (sleep-ms 300)
    (let ([start (now-ms)])
      (server-stop! server)
      (let ([elapsed (- (now-ms) start)])
        (printf "slow body cut off after ~a ms\n" elapsed)
        (check (< 250 elapsed 1500))))
    (check (string-contains? (client) "closed"))))

(test "a handler cannot stop its own server, on a worker or on the loop thread"
  (let ([server (start)])
    (check-equal? 500 (reply-status (curl (string-append base "/stop-self"))))
    (check (assertion-violation? (car (last-pair (log-items errors)))))
    (check-equal? 500 (reply-status (curl (string-append base "/stop-self-on-loop"))))
    (check (assertion-violation? (car (last-pair (log-items errors)))))
    (server-stop! server)))

(test "stopping waits for a running handler and joins its worker"
  (let* ([server (start)]
         [arrived (arrived? 'sleep)]
         [client (in-background (lambda () (curl (string-append base "/sleep"))))])
    (check (wait-until 5000 arrived))
    (server-stop! server)
    (check-equal? '(done) (log-items slept))
    (check-equal? "slept" (reply-body (client)))))

(define (forced-start)
  (start (server-options [on-error (lambda (c) (log! errors c))] [shutdown-timeout 200])))

(test "a handler running past shutdown-timeout loses its connection; every stop still joins it"
  (let* ([server (forced-start)]
         [before (length (log-items slept))]
         [arrived (arrived? 'sleep)]
         [client (in-background (lambda () (curl (string-append base "/sleep"))))])
    (check (wait-until 5000 arrived))
    (let* ([start (now-ms)]
           [stop (lambda () (server-stop! server) (list (- (now-ms) start) (length (log-items slept))))]
           [other (in-background stop)]
           [outcomes (let* ([mine (stop)] [theirs (other)]) (list mine theirs))])
      (printf "concurrent stops returned after ~a ms\n" (map car outcomes))
      (for-each
        (lambda (outcome)
          (check (>= (car outcome) 900))
          (check-equal? (+ before 1) (cadr outcome)))
        outcomes))
    (check-equal? 0 (reply-status (client)))))

(test "a stream-response returned after a forced stop is closed"
  (let* ([server (forced-start)]
         [arrived (arrived? 'late-stream)]
         [client (in-background (lambda () (curl (string-append base "/late-stream"))))])
    (check (wait-until 5000 arrived))
    (server-stop! server)
    (client)
    (check (memq 'late-closed (log-items closes)))
    (check (not (stream-open? (car (last-pair (log-items streams))))))))

(test "a port in use fails the start"
  (let ([server (start)])
    (check-raises
      (lambda (c) (and (error? c) (string-contains? (condition-message c) "cannot listen")))
      (start-server (list (http-listener "127.0.0.1" port)) (hosts)))
    (check-equal? "hello" (reply-body (curl (string-append base "/hello"))))
    (server-stop! server)))

(test-exit)
