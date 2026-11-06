(library (h2o request)
  (export
    make-request request? request-method request-path request-query request-authority
    request-scheme request-version request-headers request-header request-client-address
    request-client-port request-body request-body-port request-exchange request-buffered-body)
  (import (chezscheme))

  ;; Exactly one of body and body-port is set. Only the server touches
  ;; exchange.
  (define-record-type request
    (nongenerative) (sealed #t)
    (fields method path query authority scheme version headers client-address client-port
            (immutable body request-whole-body) (immutable body-port request-port)
            exchange))

  (define (check-request who request)
    (unless (request? request) (assertion-violationf who "~s is not a request" request)))

  (define (request-header request name)
    (check-request 'request-header request)
    (unless (string? name) (assertion-violationf 'request-header "~s is not a header name" name))
    (let ([h (assp (lambda (n) (string-ci=? n name)) (request-headers request))])
      (and h (cdr h))))

  (define (request-buffered-body request who)
    (or (request-whole-body request)
        (assertion-violationf who "~s is read by a streaming handler; use request-body-port" request)))

  (define (request-body request)
    (check-request 'request-body request)
    (request-buffered-body request 'request-body))

  (define (request-body-port request)
    (check-request 'request-body-port request)
    (or (request-port request)
        (assertion-violationf 'request-body-port
          "~s has its whole body; only a streaming handler's request has a port" request))))
