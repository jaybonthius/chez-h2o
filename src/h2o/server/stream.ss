;; Any thread may send or close, so the mutex guards every field. H2O
;; reads the parts in in-flight until it calls proceed or stop, so they
;; stay referenced here until then, and count against the limit.
(define-record-type stream
  (nongenerative) (sealed #t)
  (fields mutex on-close
          (mutable pending) (mutable buffered) (mutable in-flight)
          (mutable closing?) (mutable failed?) (mutable state)
          (mutable claimed?) (mutable scheduled?) (mutable wake) (mutable limit))
  (protocol
    (lambda (new)
      (define (make on-close) (new (make-mutex) on-close '() 0 '() #f #f 'new #f #f #f #f))
      (case-lambda
        [() (make #f)]
        [(on-close)
         (unless (procedure? on-close)
           (assertion-violationf 'make-stream "~s is not a procedure" on-close))
         (make on-close)]))))

(define (check-stream who stream)
  (unless (stream? stream) (assertion-violationf who "~s is not a stream" stream)))

;; H2O reads the parts after h2o_send returns, so they must not move.
(define (immobile-copy who data)
  (let ([bv (cond
              [(bytevector? data) data]
              [(string? data) (string->utf8 data)]
              [else (assertion-violationf who "~s is not a bytevector or string" data)])])
    (let ([copy (make-immobile-bytevector (bytevector-length bv))])
      (bytevector-copy! bv 0 copy 0 (bytevector-length bv))
      copy)))

(define (accepting? stream)
  (not (or (stream-closing? stream) (stream-failed? stream) (eq? (stream-state stream) 'done))))

(define (schedule! stream)
  (when (and (stream-wake stream) (not (stream-scheduled? stream)))
    (stream-scheduled?-set! stream #t)
    ((stream-wake stream))))

(define (bytes-of parts)
  (fold-left (lambda (n bv) (fx+ n (bytevector-length bv))) 0 parts))

(define (fail! stream)
  (stream-failed?-set! stream #t)
  (stream-buffered-set! stream (fx- (stream-buffered stream) (bytes-of (stream-pending stream))))
  (stream-pending-set! stream '())
  (schedule! stream))

(define (over-limit? stream)
  (let ([limit (stream-limit stream)])
    (and limit (fx> (stream-buffered stream) limit))))

(define (stream-send! stream data)
  (check-stream 'stream-send! stream)
  (let ([bv (immobile-copy 'stream-send! data)])
    (with-mutex (stream-mutex stream)
      (cond
        [(not (accepting? stream)) #f]
        [(fx= 0 (bytevector-length bv)) #t]
        [else
         (stream-pending-set! stream (cons bv (stream-pending stream)))
         (stream-buffered-set! stream (fx+ (stream-buffered stream) (bytevector-length bv)))
         (cond
           [(over-limit? stream) (fail! stream) #f]
           [else (schedule! stream) #t])]))))

(define (stream-close! stream)
  (check-stream 'stream-close! stream)
  (with-mutex (stream-mutex stream)
    (when (accepting? stream)
      (stream-closing?-set! stream #t)
      (schedule! stream))))

(define (stream-open? stream)
  (check-stream 'stream-open? stream)
  (with-mutex (stream-mutex stream)
    (accepting? stream)))

(define (stream-claim! who stream)
  (with-mutex (stream-mutex stream)
    (when (stream-claimed? stream)
      (assertion-violationf who "~s already belongs to a response" stream))
    (stream-claimed?-set! stream #t)))

;; The state changes here, before the caller's h2o_send, which may call
;; proceed or stop before it returns.
(define (next-action! stream head?)
  (cond
    [(not (eq? (stream-state stream) 'idle)) (values 'wait '())]
    [(stream-failed? stream) (stream-state-set! stream 'done) (values 'abort '())]
    [(pair? (stream-pending stream))
     (let ([parts (reverse (stream-pending stream))])
       (stream-pending-set! stream '())
       (stream-in-flight-set! stream parts)
       (stream-state-set! stream 'sending)
       (values 'send parts))]
    [(stream-closing? stream) (stream-state-set! stream 'done) (values 'final '())]
    [head? (stream-state-set! stream 'sending) (values 'send '())]
    [else (values 'wait '())]))

(define (stream-limit! stream limit)
  (with-mutex (stream-mutex stream)
    (stream-limit-set! stream limit)
    (not (over-limit? stream))))

(define (stream-start! stream wake)
  (with-mutex (stream-mutex stream)
    (stream-wake-set! stream wake)
    (stream-state-set! stream 'idle)
    (stream-scheduled?-set! stream #f)
    (next-action! stream #t)))

(define (stream-pump! stream)
  (with-mutex (stream-mutex stream)
    (stream-scheduled?-set! stream #f)
    (next-action! stream #f)))

(define (stream-proceeded! stream)
  (with-mutex (stream-mutex stream)
    (stream-buffered-set! stream (fx- (stream-buffered stream) (bytes-of (stream-in-flight stream))))
    (stream-in-flight-set! stream '())
    (unless (eq? (stream-state stream) 'done) (stream-state-set! stream 'idle))
    (next-action! stream #f)))

(define (stream-ended! stream)
  (with-mutex (stream-mutex stream)
    (stream-state-set! stream 'done)
    (stream-pending-set! stream '())
    (stream-in-flight-set! stream '())
    (stream-buffered-set! stream 0)))
