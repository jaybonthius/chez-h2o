(library (support)
  (export
    scratch scratch-file make-directories! read-file read-text write-file! shell-quote run
    reply? reply-status reply-version reply-headers reply-body reply-header curl raw-request
    string-index string-trim split-on split-lines split-words string-search string-contains?
    sleep-ms now-ms in-background wait-until make-log log! log-items)
  (import (chezscheme))

  (define (make-directories! dir)
    (system (format "mkdir -p '~a'" dir)))

  (define scratch
    (let ([dir (format "build/tests/~a" (path-root (path-last (car (command-line)))))])
      (system (format "rm -rf '~a'" dir))
      (make-directories! dir)
      dir))

  (define file-counter 0)
  (define file-mutex (make-mutex))

  (define (scratch-file name)
    (with-mutex file-mutex
      (set! file-counter (+ file-counter 1))
      (format "~a/~a-~a" scratch file-counter name)))

  (define (read-file file)
    (if (file-exists? file)
        (call-with-port (open-file-input-port file) get-bytevector-all)
        (eof-object)))

  (define (read-text file)
    (let ([bv (read-file file)])
      (if (eof-object? bv) "" (utf8->string bv))))

  (define (write-file! file bv)
    (call-with-port (open-file-output-port file (file-options no-fail)) (lambda (p) (put-bytevector p bv))))

  (define (shell-quote s)
    (string-append "'"
      (apply string-append
        (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s)))
      "'"))

  (define (run . words)
    (let ([out (scratch-file "out")])
      (system (format "~{~a~^ ~} > ~a 2>&1" (map shell-quote words) out))
      (read-text out)))

  (define-record-type reply (nongenerative) (fields status version headers body))

  (define (parse-head text)
    (let loop ([lines (split-lines text)] [headers '()])
      (cond
        [(null? lines) (reverse headers)]
        [(string-index (car lines) #\:)
         => (lambda (i)
              (loop (cdr lines)
                (cons (cons (string-downcase (substring (car lines) 0 i))
                            (string-trim (substring (car lines) (+ i 1) (string-length (car lines)))))
                      headers)))]
        [(and (> (string-length (car lines)) 5) (string=? (substring (car lines) 0 5) "HTTP/"))
         (loop (cdr lines) '())]
        [else (loop (cdr lines) headers)])))

  (define (curl . arguments)
    (let ([head (scratch-file "head")] [body (scratch-file "body")])
      (let ([w (apply run "curl" "-s" "--max-time" "10" "-o" body "-D" head
                 "-w" "%{http_code} %{http_version}" arguments)])
        (let ([words (split-words w)])
          (make-reply (string->number (car words)) (cadr words)
            (parse-head (read-text head)) (read-text body))))))

  (define (reply-header reply name)
    (let ([h (assoc name (reply-headers reply))]) (and h (cdr h))))

  (define (raw-request port bytes)
    (let ([file (scratch-file "request")])
      (write-file! file bytes)
      (string-trim (run "python3" "tests/raw-client.py" (number->string port) file))))

  (define (string-index s c)
    (let loop ([i 0])
      (cond
        [(= i (string-length s)) #f]
        [(char=? (string-ref s i) c) i]
        [else (loop (+ i 1))])))

  (define (string-trim s)
    (let loop ([start 0] [end (string-length s)])
      (cond
        [(and (< start end) (char-whitespace? (string-ref s start))) (loop (+ start 1) end)]
        [(and (< start end) (char-whitespace? (string-ref s (- end 1)))) (loop start (- end 1))]
        [else (substring s start end)])))

  (define (split-on s separator?)
    (let loop ([chars (string->list s)] [word '()] [words '()])
      (define (flush) (if (null? word) words (cons (list->string (reverse word)) words)))
      (cond
        [(null? chars) (reverse (flush))]
        [(separator? (car chars)) (loop (cdr chars) '() (flush))]
        [else (loop (cdr chars) (cons (car chars) word) words)])))

  (define (split-lines s) (split-on s (lambda (c) (memv c '(#\newline #\return)))))
  (define (split-words s) (split-on s char-whitespace?))

  (define (string-search s part)
    (let ([n (string-length s)] [m (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i m) n)
             (if (string=? (substring s i (+ i m)) part) i (loop (+ i 1)))))))

  (define (string-contains? s part) (and (string-search s part) #t))

  (define (sleep-ms ms)
    (sleep (make-time 'time-duration (* (remainder ms 1000) 1000000) (quotient ms 1000))))

  (define (now-ms)
    (let ([t (current-time 'time-monotonic)])
      (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

  (define (in-background thunk)
    (let ([m (make-mutex)] [c (make-condition)] [done #f] [value #f])
      (fork-thread
        (lambda ()
          (let ([v (guard (e [#t e]) (thunk))])
            (with-mutex m (set! value v) (set! done #t) (condition-broadcast c)))))
      (lambda ()
        (with-mutex m
          (let wait () (unless done (condition-wait c m) (wait)))
          value))))

  (define (wait-until ms ready?)
    (let ([deadline (+ (now-ms) ms)])
      (let loop ()
        (cond
          [(ready?) #t]
          [(> (now-ms) deadline) #f]
          [else (sleep-ms 5) (loop)]))))

  ;; Handlers run on the server's threads; tests read what they record from
  ;; the main thread.
  (define (make-log) (cons (make-mutex) '()))
  (define (log! log x) (with-mutex (car log) (set-cdr! log (cons x (cdr log)))))
  (define (log-items log) (with-mutex (car log) (reverse (cdr log)))))
