(import (chezscheme) (check) (support) (h2o forms) (h2o request) (h2o dates) (h2o cookies))

(define (fake-request headers body)
  (make-request "POST" "/form" #f "localhost" "http" 'http/1.1 headers "127.0.0.1" 40000
    body #f #f))

(define (library-imports library)
  (let ([form (call-with-input-file (format "src/h2o/~a.sls" (cadr library)) read)])
    (cdr (assq 'import (cddr form)))))

(define (all-imports library)
  (let loop ([pending (list library)] [seen '()])
    (cond
      [(null? pending) seen]
      [(member (car pending) seen) (loop (cdr pending) seen)]
      [(eq? (caar pending) 'h2o)
       (loop (append (library-imports (car pending)) (cdr pending)) (cons (car pending) seen))]
      [else (loop (cdr pending) (cons (car pending) seen))])))

(test "the helpers import no foreign code"
  (check (member '(h2o ffi) (all-imports '(h2o server))))
  (for-each
    (lambda (library) (check (not (member '(h2o ffi) (all-imports library)))))
    '((h2o forms) (h2o cookies) (h2o dates))))

(test "percent-encode keeps only unreserved characters"
  (check-equal? "a-b.c_d~e" (percent-encode "a-b.c_d~e"))
  (check-equal? "a%20b%26c%3Dd%2B" (percent-encode "a b&c=d+"))
  (check-equal? "%C3%A9" (percent-encode "é")))

(test "percent-decode decodes UTF-8 and leaves a broken escape as it is"
  (check-equal? "a b+é" (percent-decode "a%20b+%C3%A9"))
  (check-equal? "100%" (percent-decode "100%"))
  (check-equal? "%zz%4" (percent-decode "%zz%4"))
  (check-equal? "\xFFFD;" (percent-decode "%FF")))

(test "query->alist parses a query the way browsers write forms"
  (check-equal? '(("a" . "1") ("b" . "x y") ("c" . "") ("a" . "2") ("é" . "&"))
    (query->alist "a=1&b=x+y&c&&a=2&%C3%A9=%26"))
  (check-equal? '() (query->alist ""))
  (check-equal? '(("" . "v") ("k" . "a=b")) (query->alist "=v&k=a=b")))

(test "alist->query and query->alist round-trip"
  (let ([alist '(("name" . "Jay B") ("q" . "a&b=c+d") ("é" . ""))])
    (check-equal? alist (query->alist (alist->query alist))))
  (check-raises assertion-violation? (alist->query '(("a" . 1)))))

(define (form-request body)
  (fake-request '(("content-type" . "application/x-www-form-urlencoded")) (string->utf8 body)))

(test "request-form reads a URL-encoded body"
  (check-equal? '(("name" . "Jay") ("note" . "a b")) (request-form (form-request "name=Jay&note=a+b")))
  (check-equal? '() (request-uploads (form-request "name=Jay")))
  (let ([json (fake-request '(("content-type" . "application/json")) #vu8(123 125))])
    (check-equal? #f (request-form json))
    (check-equal? #f (request-uploads json)))
  (check-equal? #f (request-form (fake-request '() #vu8()))))

(test "request-form and request-uploads name themselves when the body is streamed"
  (let ([streamed (make-request "POST" "/form" #f "localhost" "http" 'http/1.1
                    '(("content-type" . "application/x-www-form-urlencoded")) "127.0.0.1" 40000
                    #f (open-bytevector-input-port #vu8()) #f)])
    (check-raises (lambda (c) (eq? 'request-form (condition-who c))) (request-form streamed))
    (check-raises (lambda (c) (eq? 'request-uploads (condition-who c))) (request-uploads streamed))))

(define boundary "----boundary1234")

(define (multipart . parts)
  (string->utf8
    (string-append
      (apply string-append
        (map (lambda (part) (string-append "--" boundary "\r\n" part "\r\n")) parts))
      "--" boundary "--\r\n")))

(define (multipart-request body)
  (fake-request
    (list (cons "content-type" (string-append "multipart/form-data; boundary=\"" boundary "\"")))
    body))

(test "request-form and request-uploads split a multipart body"
  (let ([r (multipart-request
             (multipart
               "Content-Disposition: form-data; name=\"title\"\r\n\r\nHello; world"
               "Content-Disposition: form-data; name=\"file\"; filename=\"a;b.txt\"\r\nContent-Type: text/plain\r\n\r\nline 1\r\nline 2"
               "Content-Disposition: form-data; name=\"empty\"\r\n\r\n"))])
    (check-equal? '(("title" . "Hello; world") ("empty" . "")) (request-form r))
    (let ([uploads (request-uploads r)])
      (check-equal? 1 (length uploads))
      (check-equal? "file" (upload-name (car uploads)))
      (check-equal? "a;b.txt" (upload-filename (car uploads)))
      (check-equal? "text/plain" (upload-content-type (car uploads)))
      (check-equal? (string->utf8 "line 1\r\nline 2") (upload-body (car uploads))))))

(test "a malformed multipart body gives #f"
  (define (malformed body) (request-form (multipart-request (string->utf8 body))))
  (check-equal? #f (malformed ""))
  (check-equal? #f (malformed "no boundary here"))
  (check-equal? #f (malformed (string-append "--" boundary "\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\nunterminated")))
  (check-equal? #f (malformed (string-append "--" boundary "\r\nno colon line\r\n\r\nx\r\n--" boundary "--")))
  (check-equal? #f (malformed (string-append "--" boundary "\r\nContent-Disposition: attachment\r\n\r\nx\r\n--" boundary "--")))
  (check-equal? #f (request-uploads (fake-request '(("content-type" . "multipart/form-data")) #vu8())))
  (check-equal? #f (request-form (fake-request '(("content-type" . "multipart/form-data; boundary=\"x\\")) #vu8())))
  (check-equal? #f (request-form (multipart-request (multipart "Content-Disposition: form-data; name=\"a\\\r\n\r\nx"))))
  (check-equal? '() (malformed (string-append "--" boundary "--"))))

(test "part names and filenames are read as browsers write them"
  (let ([uploads (request-uploads
                   (multipart-request
                     (multipart
                       "Content-Disposition: form-data; name=\"f%22\"; filename=\"a\\b%22c%0A.txt\"\r\n\r\nx")))])
    (check-equal? "f\"" (upload-name (car uploads)))
    (check-equal? "a\\b\"c\n.txt" (upload-filename (car uploads))))
  (let ([uploads (request-uploads
                   (multipart-request
                     (multipart
                       "Content-Disposition: form-data; name=\"dir\\\"; filename=\"x.txt\"\r\n\r\nx")))])
    (check-equal? "dir\\" (upload-name (car uploads)))
    (check-equal? "x.txt" (upload-filename (car uploads)))))

(test "part names and filenames escaped with backslashes are read"
  (let ([uploads (request-uploads
                   (multipart-request
                     (multipart
                       "Content-Disposition: form-data; name=\"q\\\"t\"; filename=\"a\\\"b.txt\"\r\n\r\nx"
                       "Content-Disposition: form-data; name=\"f\"; filename=\"a\\\\b.txt\"\r\n\r\ny")))])
    (check-equal? '("q\"t" "f") (map upload-name uploads))
    (check-equal? '("a\"b.txt" "a\\b.txt") (map upload-filename uploads))))

(test "request-cookies reads every cookie header"
  (let ([r (fake-request '(("cookie" . "a=1; b=\"two\"") ("x-other" . "c=3") ("Cookie" . "c=3;; =bad; flag; d=x=y"))
             #vu8())])
    (check-equal? '(("a" . "1") ("b" . "two") ("c" . "3") ("d" . "x=y")) (request-cookies r))))

(test "set-cookie-header writes a cookie and its attributes"
  (check-equal? '("set-cookie" . "id=abc; Max-Age=3600; Path=/; Secure; HttpOnly; SameSite=Lax")
    (set-cookie-header "id" "abc" (cookie-max-age 3600) (cookie-path "/") (cookie-secure)
      (cookie-http-only) (cookie-same-site (same-site lax))))
  (check-equal? '("set-cookie" . "id=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Domain=example.org")
    (set-cookie-header "id" "" (cookie-expires (make-time 'time-utc 0 0)) (cookie-domain "example.org"))))

(test "set-cookie-header refuses what browsers would misread"
  (check-raises assertion-violation? (set-cookie-header "bad name" "v"))
  (check-raises assertion-violation? (set-cookie-header "id" "a;b"))
  (check-raises assertion-violation? (set-cookie-header "id" "a b"))
  (check-raises assertion-violation? (set-cookie-header "id" "v" (cookie-secure) (cookie-secure)))
  (check-raises assertion-violation? (set-cookie-header "id" "v" (cookie-same-site (same-site none))))
  (check-raises assertion-violation? (set-cookie-header "id" "v" "Path=/"))
  (check-raises assertion-violation? (cookie-path "/a;b"))
  (check-raises assertion-violation? (cookie-same-site 'sideways)))

(test "time->http-date writes an IMF-fixdate"
  (check-equal? "Sun, 06 Nov 1994 08:49:37 GMT" (time->http-date (make-time 'time-utc 0 784111777)))
  (check-equal? "Thu, 01 Jan 1970 00:00:00 GMT" (time->http-date (make-time 'time-utc 999 0)))
  (check-equal? "Tue, 29 Feb 2000 12:00:00 GMT" (time->http-date (make-time 'time-utc 0 951825600)))
  (check-raises assertion-violation? (time->http-date (make-time 'time-monotonic 0 0))))

(define (seconds text)
  (let ([t (http-date->time text)])
    (and t (eq? 'time-utc (time-type t)) (= 0 (time-nanosecond t)) (time-second t))))

(test "http-date->time reads the three formats RFC 9110 requires"
  (check-equal? 784111777 (seconds "Sun, 06 Nov 1994 08:49:37 GMT"))
  (check-equal? 784111777 (seconds "Sunday, 06-Nov-94 08:49:37 GMT"))
  (check-equal? 784111777 (seconds "Sun Nov  6 08:49:37 1994"))
  (check-equal? 951825600 (seconds "Tue, 29 Feb 2000 12:00:00 GMT"))
  (check-equal? 3155760000 (seconds "Wednesday, 01-Jan-70 00:00:00 GMT"))
  (let ([now (current-time 'time-utc)])
    (check-equal? (time-second now) (seconds (time->http-date now)))))

(test "http-date->time gives #f for anything else"
  (for-each
    (lambda (text) (check-equal? #f (http-date->time text)))
    '("" "Sun, 06 Nov 1994 08:49:37" "Sun, 06 Nov 1994 08:49:37 UTC" "Sun, 6 Nov 1994 08:49:37 GMT"
      "Sun, 31 Nov 1994 08:49:37 GMT" "Sun, 29 Feb 1900 08:49:37 GMT" "Sun, 06 Nov 1994 24:00:00 GMT"
      "Sun, 06 Nov 1994 08:60:00 GMT" "Xyz, 06 Nov 1994 08:49:37 GMT" "Sun, 06 Foo 1994 08:49:37 GMT"
      "Sun, 06 Nov 1994 08:49:37 GMT " "Sunday, 06-Nov-1994 08:49:37 GMT" "Sun Nov 6 08:49:37 1994"
      "1994-11-06T08:49:37Z")))

(test-exit)
