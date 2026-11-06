(library (h2o forms)
  (export
    percent-encode percent-decode query->alist alist->query request-form request-uploads
    upload? upload-name upload-filename upload-content-type upload-body)
  (import (chezscheme) (h2o request) (h2o text))

  (define-record-type upload
    (nongenerative) (sealed #t)
    (fields name filename content-type body))

  (define (hex-value b)
    (cond
      [(fx<= 48 b 57) (fx- b 48)]
      [(fx<= 65 b 70) (fx- b 55)]
      [(fx<= 97 b 102) (fx- b 87)]
      [else #f]))

  (define (unreserved? b)
    (or (fx<= 65 b 90) (fx<= 97 b 122) (fx<= 48 b 57) (memv b '(45 46 95 126))))

  (define (percent-encode s)
    (unless (string? s) (assertion-violationf 'percent-encode "~s is not a string" s))
    (let-values ([(out get) (open-string-output-port)])
      (bytevector-for-each
        (lambda (b)
          (if (unreserved? b)
              (write-char (integer->char b) out)
              (format out "%~2,'0X" b)))
        (string->utf8 s))
      (get)))

  (define (bytevector-for-each proc bv)
    (let loop ([i 0])
      (when (fx< i (bytevector-length bv))
        (proc (bytevector-u8-ref bv i))
        (loop (fx+ i 1)))))

  ;; As the URL standard does, a % not followed by two hex digits stays as
  ;; it is, and bytes that are not UTF-8 become U+FFFD, so decoding never
  ;; fails on what a client sends.
  (define (decode s plus-is-space?)
    (let ([bytes (string->utf8 s)])
      (let-values ([(out get) (open-bytevector-output-port)])
        (let loop ([i 0])
          (when (fx< i (bytevector-length bytes))
            (let ([b (bytevector-u8-ref bytes i)])
              (cond
                [(and (fx= b 37) (fx< (fx+ i 2) (bytevector-length bytes))
                      (hex-value (bytevector-u8-ref bytes (fx+ i 1)))
                      (hex-value (bytevector-u8-ref bytes (fx+ i 2))))
                 (put-u8 out (fx+ (fx* 16 (hex-value (bytevector-u8-ref bytes (fx+ i 1))))
                                  (hex-value (bytevector-u8-ref bytes (fx+ i 2)))))
                 (loop (fx+ i 3))]
                [(and plus-is-space? (fx= b 43)) (put-u8 out 32) (loop (fx+ i 1))]
                [else (put-u8 out b) (loop (fx+ i 1))]))))
        (utf8->string (get)))))

  (define (percent-decode s)
    (unless (string? s) (assertion-violationf 'percent-decode "~s is not a string" s))
    (decode s #f))

  (define (query->alist query)
    (unless (string? query) (assertion-violationf 'query->alist "~s is not a string" query))
    (fold-right
      (lambda (field fields)
        (cond
          [(string=? field "") fields]
          [(string-index field #\=)
           => (lambda (i)
                (cons (cons (decode (substring field 0 i) #t)
                            (decode (substring field (fx+ i 1) (string-length field)) #t))
                      fields))]
          [else (cons (cons (decode field #t) "") fields)]))
      '()
      (string-split query #\&)))

  (define (alist->query alist)
    (unless (and (list? alist)
                 (for-all (lambda (p) (and (pair? p) (string? (car p)) (string? (cdr p)))) alist))
      (assertion-violationf 'alist->query "~s is not a list of string pairs" alist))
    (let-values ([(out get) (open-string-output-port)])
      (let loop ([alist alist] [first? #t])
        (unless (null? alist)
          (unless first? (write-char #\& out))
          (put-string out (percent-encode (caar alist)))
          (write-char #\= out)
          (put-string out (percent-encode (cdar alist)))
          (loop (cdr alist) #f)))
      (get)))

  ;; Browsers escape no character inside a quoted value with a backslash
  ;; (see browser-unescape), but other clients (Go's mime/multipart) escape
  ;; " and \ as \" and \\. Those two escapes are read first; if that leaves
  ;; a quote unclosed, the backslash is an ordinary character, as from a
  ;; browser. A quoted value may also hold a semicolon.
  (define (split-parameters value escapes?)
    (let ([n (string-length value)])
      (let loop ([i 0] [start 0] [quoted? #f] [parts '()])
        (cond
          [(fx= i n) (and (not quoted?) (reverse (cons (substring value start n) parts)))]
          [(and escapes? quoted? (char=? (string-ref value i) #\\) (fx< (fx+ i 1) n)
                (memv (string-ref value (fx+ i 1)) '(#\" #\\)))
           (loop (fx+ i 2) start quoted? parts)]
          [(char=? (string-ref value i) #\")
           (loop (fx+ i 1) start (not quoted?) parts)]
          [(and (not quoted?) (char=? (string-ref value i) #\;))
           (loop (fx+ i 1) (fx+ i 1) quoted? (cons (substring value start i) parts))]
          [else (loop (fx+ i 1) start quoted? parts)]))))

  (define (unescape s)
    (let-values ([(out get) (open-string-output-port)])
      (let loop ([i 0])
        (cond
          [(fx= i (string-length s)) (get)]
          [(and (char=? (string-ref s i) #\\) (fx< (fx+ i 1) (string-length s))
                (memv (string-ref s (fx+ i 1)) '(#\" #\\)))
           (write-char (string-ref s (fx+ i 1)) out)
           (loop (fx+ i 2))]
          [else (write-char (string-ref s i) out) (loop (fx+ i 1))]))))

  (define (parse-header-value value)
    (define (parameter-of escapes?)
      (lambda (part)
        (let ([i (string-index part #\=)])
          (and i
               (let ([text (unquote-value
                             (string-trim-blanks (substring part (fx+ i 1) (string-length part))))])
                 (cons (string-downcase (string-trim-blanks (substring part 0 i)))
                       (if escapes? (unescape text) text)))))))
    (define (parsed parts escapes?)
      (values (string-downcase (string-trim-blanks (car parts)))
              (filter values (map (parameter-of escapes?) (cdr parts)))))
    (cond
      [(split-parameters value #t) => (lambda (parts) (parsed parts #t))]
      [(split-parameters value #f) => (lambda (parts) (parsed parts #f))]
      [else (values #f '())]))

  ;; Browsers write ", CR and LF in a part's name and filename as %22, %0D
  ;; and %0A, and leave every other character as it is, % included (WHATWG
  ;; HTML, multipart/form-data encoding).
  (define (browser-unescape s)
    (let-values ([(out get) (open-string-output-port)])
      (let loop ([i 0])
        (cond
          [(fx= i (string-length s)) (get)]
          [(and (char=? (string-ref s i) #\%) (fx<= (fx+ i 3) (string-length s))
                (assoc (string-upcase (substring s (fx+ i 1) (fx+ i 3)))
                  '(("22" . #\") ("0D" . #\return) ("0A" . #\newline))))
           => (lambda (escape) (write-char (cdr escape) out) (loop (fx+ i 3)))]
          [else (write-char (string-ref s i) out) (loop (fx+ i 1))]))))

  (define (parameter parameters name)
    (let ([p (assoc name parameters)]) (and p (cdr p))))

  (define (bytevector-search bv pattern start)
    (let ([n (bytevector-length bv)] [m (bytevector-length pattern)])
      (let loop ([i start])
        (cond
          [(fx> (fx+ i m) n) #f]
          [(let match ([j 0])
             (or (fx= j m)
                 (and (fx= (bytevector-u8-ref bv (fx+ i j)) (bytevector-u8-ref pattern j))
                      (match (fx+ j 1)))))
           i]
          [else (loop (fx+ i 1))]))))

  (define (subbytevector bv start end)
    (let ([out (make-bytevector (fx- end start))])
      (bytevector-copy! bv start out 0 (fx- end start))
      out))

  (define crlf (string->utf8 "\r\n"))
  (define blank-line (string->utf8 "\r\n\r\n"))

  (define (parse-part-headers text)
    (let loop ([lines (string-split text #\newline)] [headers '()])
      (if (null? lines)
          headers
          (let* ([line (let ([l (car lines)])
                         (if (and (fx> (string-length l) 0)
                                  (char=? (string-ref l (fx- (string-length l) 1)) #\return))
                             (substring l 0 (fx- (string-length l) 1))
                             l))]
                 [i (string-index line #\:)])
            (and i
                 (loop (cdr lines)
                   (cons (cons (string-downcase (string-trim-blanks (substring line 0 i)))
                               (string-trim-blanks (substring line (fx+ i 1) (string-length line))))
                         headers)))))))

  (define (bytevector-prefix? bv pattern at)
    (and (fx<= (fx+ at (bytevector-length pattern)) (bytevector-length bv))
         (fx= at (or (bytevector-search bv pattern at) -1))))

  (define dashes (string->utf8 "--"))

  ;; RFC 2046 5.1.1 puts CRLF before every delimiter but lets the first one
  ;; start the body, so the body is searched as if it began with CRLF.
  (define (split-multipart body boundary)
    (let* ([delimiter (string->utf8 (string-append "\r\n--" boundary))]
           [body (let ([b (make-bytevector (fx+ 2 (bytevector-length body)))])
                   (bytevector-copy! crlf 0 b 0 2)
                   (bytevector-copy! body 0 b 2 (bytevector-length body))
                   b)]
           [first (bytevector-search body delimiter 0)])
      (define (part head-start head-end content-start next)
        (let* ([headers (parse-part-headers (utf8->string (subbytevector body head-start head-end)))]
               [disposition (and headers (parameter headers "content-disposition"))])
          (and disposition
               (let-values ([(kind parameters) (parse-header-value disposition)])
                 (let ([name (parameter parameters "name")]
                       [filename (parameter parameters "filename")])
                   (and (equal? kind "form-data")
                        name
                        (make-upload (browser-unescape name) (and filename (browser-unescape filename))
                          (or (parameter headers "content-type") "text/plain")
                          (subbytevector body content-start next))))))))
      (and first
           (let loop ([at (fx+ first (bytevector-length delimiter))] [parts '()])
             (if (bytevector-prefix? body dashes at)
                 (reverse parts)
                 (let* ([line-end (bytevector-search body crlf at)]
                        [head-start (and line-end (fx+ line-end 2))]
                        [head-end (and head-start
                                       (if (bytevector-prefix? body crlf head-start)
                                           head-start
                                           (bytevector-search body blank-line head-start)))]
                        [content-start (and head-end
                                            (fx+ head-end (if (fx= head-end head-start) 2 4)))]
                        [next (and content-start (bytevector-search body delimiter content-start))]
                        [this (and next (part head-start head-end content-start next))])
                   (and this (loop (fx+ next (bytevector-length delimiter)) (cons this parts)))))))))

  (define (content-type request)
    (let ([value (request-header request "content-type")])
      (if value
          (parse-header-value value)
          (values "" '()))))

  (define (request-form request)
    (unless (request? request) (assertion-violationf 'request-form "~s is not a request" request))
    (let-values ([(type parameters) (content-type request)])
      (cond
        [(equal? type "application/x-www-form-urlencoded")
         (query->alist (utf8->string (request-buffered-body request 'request-form)))]
        [(equal? type "multipart/form-data")
         (let ([parts (multipart-parts 'request-form request parameters)])
           (and parts
                (map (lambda (part) (cons (upload-name part) (utf8->string (upload-body part))))
                     (filter (lambda (part) (not (upload-filename part))) parts))))]
        [else #f])))

  (define (request-uploads request)
    (unless (request? request) (assertion-violationf 'request-uploads "~s is not a request" request))
    (let-values ([(type parameters) (content-type request)])
      (cond
        [(equal? type "application/x-www-form-urlencoded")
         (request-buffered-body request 'request-uploads)
         '()]
        [(equal? type "multipart/form-data")
         (let ([parts (multipart-parts 'request-uploads request parameters)])
           (and parts (filter upload-filename parts)))]
        [else #f])))

  (define (multipart-parts who request parameters)
    (let ([body (request-buffered-body request who)] [boundary (parameter parameters "boundary")])
      (and boundary (fx<= 1 (string-length boundary) 70)
           (split-multipart body boundary)))))
