(library (h2o text)
  (export string-split string-trim-blanks string-index token? unquote-value)
  (import (chezscheme))

  (define (string-split s c)
    (let loop ([i 0] [start 0] [parts '()])
      (cond
        [(fx= i (string-length s)) (reverse (cons (substring s start i) parts))]
        [(char=? (string-ref s i) c) (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) parts))]
        [else (loop (fx+ i 1) start parts)])))

  (define (string-index s c)
    (let loop ([i 0])
      (cond
        [(fx= i (string-length s)) #f]
        [(char=? (string-ref s i) c) i]
        [else (loop (fx+ i 1))])))

  ;; HTTP's optional whitespace is spaces and tabs only (RFC 9110 5.6.3).
  (define (string-trim-blanks s)
    (define (blank? c) (memv c '(#\space #\tab)))
    (let loop ([start 0] [end (string-length s)])
      (cond
        [(and (fx< start end) (blank? (string-ref s start))) (loop (fx+ start 1) end)]
        [(and (fx< start end) (blank? (string-ref s (fx- end 1)))) (loop start (fx- end 1))]
        [else (substring s start end)])))

  (define (token-char? c)
    (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
        (memv c '(#\! #\# #\$ #\% #\& #\' #\* #\+ #\- #\. #\^ #\_ #\` #\| #\~))))

  (define (token? s)
    (and (string? s) (fx> (string-length s) 0) (for-all token-char? (string->list s))))

  (define (unquote-value s)
    (let ([n (string-length s)])
      (if (and (fx>= n 2) (char=? (string-ref s 0) #\") (char=? (string-ref s (fx- n 1)) #\"))
          (substring s 1 (fx- n 1))
          s))))
