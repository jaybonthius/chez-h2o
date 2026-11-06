(import (chezscheme) (h2o))

(define port (string->number (car (command-line-arguments))))
(define commands-on-loop?
  (and (pair? (cdr (command-line-arguments))) (string=? (cadr (command-line-arguments)) "loop")))

(load-shared-object "libc.so.6")

(define clock-gettime (foreign-procedure "clock_gettime" (int u8*) int))
(define getpid (foreign-procedure "getpid" () int))
(define clock-monotonic 1)

(define (monotonic-ns)
  (let ([ts (make-bytevector 16)])
    (clock-gettime clock-monotonic ts)
    (+ (* (bytevector-s64-native-ref ts 0) 1000000000) (bytevector-s64-native-ref ts 8))))

(define streams-mutex (make-mutex))
(define requests-mutex (make-mutex))
(define streams '())
(define requests 0)
(define last-close-ns 0)

(define inputs-mutex (make-mutex))
(define inputs-ready (make-condition))
(define inputs '())

(define (submit! bv)
  (with-mutex inputs-mutex
    (set! inputs (append inputs (list bv)))
    (condition-signal inputs-ready)))

(define (take!)
  (with-mutex inputs-mutex
    (let wait ()
      (if (null? inputs)
          (begin (condition-wait inputs-ready inputs-mutex) (wait))
          (let ([bv (car inputs)]) (set! inputs (cdr inputs)) bv)))))

(define (count-request!)
  (with-mutex requests-mutex (set! requests (+ requests 1))))

(define (commands request)
  (count-request!)
  (submit! (request-body request))
  (response 204 '() #vu8()))

(define (events request)
  (count-request!)
  (letrec ([stream (make-stream
                     (lambda ()
                       (with-mutex streams-mutex (set! streams (remq stream streams)))
                       (set! last-close-ns (monotonic-ns))))])
    (stream-send! stream ": open\n\n")
    (with-mutex streams-mutex (set! streams (cons stream streams)))
    (stream-response 200 '(("content-type" . "text/event-stream") ("cache-control" . "no-cache"))
      stream)))

(define (stats request)
  (count-request!)
  (response 200 '()
    (format "~a 0 ~a ~a" (with-mutex requests-mutex requests)
      (with-mutex streams-mutex (length streams)) last-close-ns)))

(define (publish! n size ms)
  (do ([i 0 (+ i 1)]) ((= i n))
    (sleep (make-time 'time-duration (* ms 1000000) 0))
    (let ([event (string->utf8
                   (string-append
                     "event: datastar-patch-elements\ndata: elements <div id=\"t\" data-ns=\""
                     (number->string (monotonic-ns)) "\">" (make-string size #\x) "</div>\n\n"))])
      (for-each (lambda (s) (stream-send! s event)) (with-mutex streams-mutex streams)))))

(define (editor)
  (let ([words (let ([p (open-input-string (utf8->string (take!)))])
                 (let loop ([acc '()])
                   (let ([x (read p)]) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))])
    (when (and (pair? words) (eq? (car words) 'push))
      (apply publish! (cdr words))))
  (editor))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port) (format "localhost:~a" port))
            (list (route "/commands" ((if commands-on-loop? loop-handler handler) commands))
                  (route "/events" (loop-handler events))
                  (route "/stats" (loop-handler stats)))))
    (server-options [strict-hosts #t])))

(printf "ready ~a\n" (getpid))
(flush-output-port)
(editor)
