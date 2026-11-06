(import (chezscheme) (h2o))

(define port (string->number (car (command-line-arguments))))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list (make-host (list (format "127.0.0.1:~a" port) (format "localhost:~a" port))
            (list (route "/raise" (handler (lambda (r) (error 'handler "the handler failed")))))))
    (server-options [on-error (lambda (c) (error 'on-error "on-error failed too"))])))

(system (format "curl -s -o /dev/null --max-time 5 http://127.0.0.1:~a/raise" port))
(server-stop! server)
