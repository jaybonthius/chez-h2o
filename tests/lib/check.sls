(library (check)
  (export test check check-equal? check-raises test-exit)
  (import (chezscheme))

  (define checks 0)
  (define failures 0)
  (define tests 0)
  (define failed-tests '())
  (define current-test (make-thread-parameter #f))

  (define-syntax source-of
    (lambda (x)
      (syntax-case x ()
        [(_ form)
         (let ([a (syntax->annotation #'form)])
           #`'#,(datum->syntax #'form
                  (and a
                       (call-with-values
                         (lambda ()
                           (locate-source-object-source (annotation-source a) #t #t))
                         (case-lambda
                           [(path line column) (list path line column)]
                           [other #f])))))])))

  (define (source->string source)
    (if source
        (apply format "~a:~a:~a" source)
        "unknown location"))

  (define (fail! source fmt . args)
    (set! failures (+ failures 1))
    (printf "~a: FAIL in ~s: ~?\n" (source->string source) (current-test) fmt args))

  (define (condition->string c)
    (if (condition? c)
        (call-with-string-output-port (lambda (p) (display-condition c p)))
        (format "~s" c)))

  (define (check-procedure source quoted thunk)
    (set! checks (+ checks 1))
    (unless (thunk) (fail! source "~s is false" quoted)))

  (define (check-equal-procedure source quoted expected actual)
    (set! checks (+ checks 1))
    (unless (equal? expected actual)
      (fail! source "~s: expected ~s, got ~s" quoted expected actual)))

  (define (check-raises-procedure source quoted pred thunk)
    (set! checks (+ checks 1))
    ;; Fresh objects, so no value the code under test raises can be
    ;; mistaken for an outcome.
    (let* ([raised (list 'raised)] [returned (list 'returned)]
           [outcome (guard (c [#t (if (pred c) raised c)]) (thunk) returned)])
      (cond
        [(eq? outcome raised) (void)]
        [(eq? outcome returned) (fail! source "~s did not raise" quoted)]
        [else (fail! source "~s raised the wrong condition: ~a" quoted
                (condition->string outcome))])))

  (define-syntax check
    (lambda (x)
      (syntax-case x ()
        [(k e) #'(check-procedure (source-of k) 'e (lambda () e))])))

  (define-syntax check-equal?
    (lambda (x)
      (syntax-case x ()
        [(k expected actual)
         #'(check-equal-procedure (source-of k) 'actual expected actual)])))

  (define-syntax check-raises
    (lambda (x)
      (syntax-case x ()
        [(k pred e) #'(check-raises-procedure (source-of k) 'e pred (lambda () e))])))

  (define (raise-source c)
    (and (continuation-condition? c)
         (let loop ([i (inspect/object (condition-continuation c))] [n 0])
           (and (< n 50) (eq? (i 'type) 'continuation)
                (call-with-values (lambda () (i 'source-path))
                  (case-lambda
                    [(path line column) (list path line column)]
                    [other (and (> (i 'depth) 1) (loop (i 'link) (+ n 1)))]))))))

  (define (run-test source name thunk)
    (set! tests (+ tests 1))
    (let ([failures-before failures])
      (parameterize ([current-test name])
        (guard (c [#t (fail! (or (raise-source c) source) "uncaught: ~a"
                        (condition->string c))])
          (thunk)))
      (unless (= failures failures-before)
        (set! failed-tests (cons name failed-tests)))))

  (define-syntax test
    (lambda (x)
      (syntax-case x ()
        [(k name body ...) #'(run-test (source-of k) name (lambda () body ...))])))

  (define (test-exit-procedure source)
    (printf "~a: ~a tests, ~a checks, ~a failures~a\n"
      (if source (car source) (car (command-line)))
      tests checks failures
      (if (null? failed-tests)
          ""
          (format " (failed: ~{~s~^ ~})" (reverse failed-tests))))
    (exit (if (= failures 0) 0 1)))

  ;; A macro so the summary names the test's source file rather than the
  ;; compiled object the runner loads.
  (define-syntax test-exit
    (lambda (x)
      (syntax-case x ()
        [(k) #'(test-exit-procedure (source-of k))]))))
