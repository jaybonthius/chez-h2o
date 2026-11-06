;; H2O delivers no further part until proceed is called, so a reader that
;; does not read holds the client back. current and offset belong to the
;; reader.
(define-record-type feed
  (nongenerative) (sealed #t)
  (fields mutex condition proceed
          (mutable incoming) (mutable last?) (mutable aborted?) (mutable finished?)
          (mutable current) (mutable offset))
  (protocol
    (lambda (new)
      (lambda (proceed)
        (new (make-mutex) (make-condition) proceed #f #f #f #f #vu8() 0)))))

(define (feed-deliver! feed part last?)
  (with-mutex (feed-mutex feed)
    (cond
      [(feed-finished? feed) (unless last? ((feed-proceed feed)))]
      [else
       (feed-incoming-set! feed part)
       (feed-last?-set! feed last?)
       (condition-broadcast (feed-condition feed))])))

(define (feed-abort! feed)
  (with-mutex (feed-mutex feed)
    (feed-aborted?-set! feed #t)
    (condition-broadcast (feed-condition feed))))

;; Once the handler has returned, the rest of the body is read and
;; dropped, so that the connection can carry the next request.
(define (feed-finish! feed)
  (with-mutex (feed-mutex feed)
    (feed-finished?-set! feed #t)
    (when (and (feed-incoming feed) (not (feed-last? feed)))
      (feed-incoming-set! feed #f)
      ((feed-proceed feed)))))

(define (aborted-body)
  (condition (make-i/o-read-error) (make-who-condition 'request-body-port)
    (make-message-condition "the client ended the request before its body was complete")))

(define (read! feed bv start count)
  (with-mutex (feed-mutex feed)
    (let loop ()
      (let* ([current (feed-current feed)]
             [available (fx- (bytevector-length current) (feed-offset feed))])
        (cond
          [(fx> available 0)
           (let ([n (fxmin available count)])
             (bytevector-copy! current (feed-offset feed) bv start n)
             (feed-offset-set! feed (fx+ (feed-offset feed) n))
             n)]
          [(feed-incoming feed)
           => (lambda (part)
                (feed-current-set! feed part)
                (feed-offset-set! feed 0)
                (feed-incoming-set! feed #f)
                (unless (feed-last? feed) ((feed-proceed feed)))
                (loop))]
          [(feed-last? feed) 0]
          [(feed-aborted? feed) (raise (aborted-body))]
          [else (condition-wait (feed-condition feed) (feed-mutex feed)) (loop)])))))

(define (feed-port feed)
  (make-custom-binary-input-port "request body"
    (lambda (bv start count) (read! feed bv start count))
    #f #f #f))
