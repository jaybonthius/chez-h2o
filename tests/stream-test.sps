(import (chezscheme) (check) (support) (h2o))

(define port 18431)
(define idle-port 18432)
(define base (format "http://127.0.0.1:~a" port))

(define streams (make-log))
(define closes (make-log))

(define (events request)
  (let ([stream (make-stream (lambda () (log! closes (now-ms))))])
    (stream-send! stream ": open\n\n")
    (log! streams stream)
    (stream-response 200 '(("content-type" . "text/event-stream") ("cache-control" . "no-cache"))
      stream)))

(define (hosts p)
  (list (make-host (list (format "127.0.0.1:~a" p) (format "localhost:~a" p))
          (list (route "/events" (handler events))
                (route "/closed-at-once"
                  (loop-handler
                    (lambda (r)
                      (let ([stream (make-stream)])
                        (stream-close! stream)
                        (stream-response 200 '() stream)))))))))

(define server (start-server (list (http-listener "127.0.0.1" port)) (hosts port)))

(define (newest-stream count)
  (and (wait-until 5000 (lambda () (>= (length (log-items streams)) count)))
       (list-ref (log-items streams) (- count 1))))

(define (sse-event i) (format "id: ~a\ndata: event ~a\n\n" i i))

(define (pusher stream n ms)
  (fork-thread
    (lambda ()
      (let loop ([i 0])
        (cond
          [(= i n) (stream-close! stream)]
          [else (stream-send! stream (sse-event i)) (sleep-ms ms) (loop (+ i 1))])))))

(define (expected-events n)
  (apply string-append ": open\n\n" (map sse-event (iota n))))

(test "events pushed from another thread arrive in order and the stream ends"
  (let* ([seen (length (log-items streams))]
         [client (in-background (lambda () (curl "-N" (string-append base "/events"))))]
         [stream (newest-stream (+ seen 1))])
    (pusher stream 50 5)
    (let ([r (client)])
      (check-equal? 200 (reply-status r))
      (check-equal? "text/event-stream" (reply-header r "content-type"))
      (check-equal? "chunked" (reply-header r "transfer-encoding"))
      (check-equal? (expected-events 50) (reply-body r)))
    (check (not (stream-open? stream)))
    (check-equal? #f (stream-send! stream "late"))))

(test "each of 5 streams gets every push"
  (let* ([seen (length (log-items streams))]
         [clients (map (lambda (i) (in-background (lambda () (curl "-N" (string-append base "/events")))))
                       (iota 5))]
         [opened (map (lambda (i) (newest-stream (+ seen i 1))) (iota 5))])
    (fork-thread
      (lambda ()
        (for-each (lambda (i) (for-each (lambda (s) (stream-send! s (sse-event i))) opened) (sleep-ms 2))
          (iota 100))
        (for-each stream-close! opened)))
    (for-each (lambda (client) (check-equal? (expected-events 100) (reply-body (client)))) clients)))

(test "a 32 KB event arrives whole"
  (let* ([seen (length (log-items streams))]
         [client (in-background (lambda () (curl "-N" (string-append base "/events"))))]
         [stream (newest-stream (+ seen 1))]
         [data (string-append "data: " (make-string 32768 #\x) "\n\n")])
    (stream-send! stream data)
    (stream-close! stream)
    (check-equal? (string-append ": open\n\n" data) (reply-body (client)))))

(test "a client that goes away is noticed at the next writes"
  (let* ([seen (length (log-items streams))]
         [closes-before (length (log-items closes))]
         [client (in-background (lambda () (curl "-N" "--max-time" "1" (string-append base "/events"))))]
         [stream (newest-stream (+ seen 1))])
    (client)
    (let ([gone (now-ms)])
      (let loop () (when (stream-send! stream (sse-event 0)) (sleep-ms 20) (loop)))
      (check (wait-until 2000 (lambda () (> (length (log-items closes)) closes-before))))
      (let ([noticed (- (car (last-pair (log-items closes))) gone)])
        (printf "disconnect noticed after ~a ms of 20 ms pushes\n" noticed)
        (check (< noticed 1000))))
    (check (not (stream-open? stream)))))

(test "a stream closed at once sends an empty body"
  (let ([r (curl (string-append base "/closed-at-once"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "" (reply-body r))))

(test "H2O closes an idle HTTP/1.1 stream after http1-request-io-timeout"
  (let* ([idle (start-server (list (http-listener "127.0.0.1" idle-port)) (hosts idle-port)
                 (server-options [http1-request-io-timeout 300]))]
         [seen (length (log-items streams))]
         [start (now-ms)]
         [client (in-background
                   (lambda () (curl "-N" (format "http://127.0.0.1:~a/events" idle-port))))])
    (newest-stream (+ seen 1))
    (let* ([r (client)] [elapsed (- (now-ms) start)])
      (check-equal? ": open\n\n" (reply-body r))
      (printf "idle stream closed after ~a ms\n" elapsed)
      (check (< 250 elapsed 2000)))
    (server-stop! idle)))

(test "stream arguments are checked"
  (check-raises assertion-violation? (stream-send! 'not-a-stream "x"))
  (let ([stream (newest-stream 1)])
    (check-raises assertion-violation? (stream-send! stream 42))))

(server-stop! server)

(test-exit)
