;; H2O writes the framing headers itself; a second content-length or
;; transfer-encoding would make the response ambiguous. HTTP/2 forbids
;; the connection-specific headers (RFC 9113 8.2.2), and H2O does not
;; remove them from a response.
(define (forbidden-header? name)
  (member (string-downcase name)
    '("content-length" "transfer-encoding" "connection" "keep-alive" "proxy-connection"
      "upgrade")))

(define (valid-header? h)
  (and (pair? h) (token? (car h)) (string? (cdr h))
       (not (forbidden-header? (car h)))
       (not (exists (lambda (c) (memv c '(#\return #\newline #\nul))) (string->list (cdr h))))))

(define (check-headers who headers)
  (unless (and (list? headers) (for-all valid-header? headers))
    (assertion-violationf who "~s is not a list of header names and values" headers)))

(define (check-status who status)
  (unless (and (fixnum? status) (fx<= 200 status 599))
    (assertion-violationf who "~s is not a final status code" status)))

(define (bodiless-status? status)
  (or (fx= status 204) (fx= status 304)))

(define-record-type (whole-response response response?)
  (nongenerative) (sealed #t)
  (fields (immutable status response-status) (immutable headers response-headers)
          (immutable body response-body))
  (protocol
    (lambda (new)
      (lambda (status headers body)
        (check-status 'response status)
        (check-headers 'response headers)
        (let ([bv (cond
                    [(bytevector? body) body]
                    [(string? body) (string->utf8 body)]
                    [else (assertion-violationf 'response "~s is not a bytevector or string" body)])])
          (when (and (bodiless-status? status) (not (fx= 0 (bytevector-length bv))))
            (assertion-violationf 'response "status ~s has no body" status))
          (new status headers bv))))))

(define-record-type (streamed-response stream-response stream-response?)
  (nongenerative) (sealed #t)
  (fields (immutable status stream-response-status) (immutable headers stream-response-headers)
          (immutable stream stream-response-stream))
  (protocol
    (lambda (new)
      (lambda (status headers stream)
        (check-status 'stream-response status)
        (check-headers 'stream-response headers)
        (when (bodiless-status? status)
          (assertion-violationf 'stream-response "status ~s has no body" status))
        (unless (stream? stream)
          (assertion-violationf 'stream-response "~s is not a stream" stream))
        (stream-claim! 'stream-response stream)
        (new status headers stream)))))
