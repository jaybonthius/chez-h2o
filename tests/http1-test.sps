(import (chezscheme) (check) (support) (h2o))

(define port 18411)
(define base (format "http://127.0.0.1:~a" port))
(define errors (make-log))
(define uploads (make-log))

(define (echo request)
  (response 200 '(("content-type" . "text/plain") ("x-reply" . "yes"))
    (format "~a ~a ~s ~s ~s ~a" (request-method request) (request-path request)
      (request-query request) (request-header request "x-test") (request-version request)
      (utf8->string (request-body request)))))

(define (upload request)
  (let ([port (request-body-port request)])
    (let loop ([reads 0] [bytes 0])
      (let ([part (get-bytevector-some port)])
        (if (eof-object? part)
            (begin
              (log! uploads reads)
              (response 200 '() (format "~a bytes" bytes)))
            (loop (+ reads 1) (+ bytes (bytevector-length part))))))))

(define received-upload (string-append scratch "/received"))

(define (form request)
  (let ([uploads (request-uploads request)])
    (for-each (lambda (u) (write-file! received-upload (upload-body u))) uploads)
    (response 200 '()
      (format "~s ~s" (request-form request)
        (map (lambda (u) (list (upload-name u) (upload-filename u) (upload-content-type u)))
             uploads)))))

(make-directories! (string-append scratch "/www/sub"))
(write-file! (string-append scratch "/www/a.txt") (string->utf8 "plain text\n"))
(write-file! (string-append scratch "/www/index.html") (string->utf8 "<p>index</p>\n"))
(define big-file (string-append scratch "/big"))
(write-file! big-file (make-bytevector 1000000 65))

(define server
  (start-server (list (http-listener "127.0.0.1" port))
    (list
      (make-host (list (format "127.0.0.1:~a" port) (format "localhost:~a" port))
        (list
          (route "/echo" (handler echo))
          (route "/prefix" (handler (lambda (r) (response 200 '() (request-path r)))))
          (route "/teapot" (handler (lambda (r) (response 418 '() "short and stout"))))
          (route "/empty" (handler (lambda (r) (response 204 '() #vu8()))))
          (route "/raise" (handler (lambda (r) (error 'raise-handler "deliberate"))))
          (route "/silent" (handler (lambda (r) (void))))
          (route "/bad-header" (handler (lambda (r) (response 200 '(("x-bad" . "a\r\nb")) "never"))))
          (route "/upload" (streaming-handler upload))
          (route "/ignore-body" (streaming-handler (lambda (r) (response 202 '() "ignored"))))
          (route "/static" (files (string-append scratch "/www")))
          (route "/form" (handler form))
          (route "/cookies"
            (handler
              (lambda (r)
                (response 200 (list (set-cookie-header "seen" "yes" (cookie-path "/")))
                  (format "~s" (request-cookies r)))))))))
    (server-options
      [on-error (lambda (c) (log! errors (condition-message c)))]
      [max-request-body-size 2000000])))

(test "a GET reaches its handler with method, path, query and headers"
  (let ([r (curl "-H" "X-Test: 42" (string-append base "/echo?a=1&b=%20"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "yes" (reply-header r "x-reply"))
    (check-equal? "text/plain" (reply-header r "content-type"))
    (check-equal? "GET /echo \"a=1&b=%20\" \"42\" http/1.1 " (reply-body r))))

(test "a request without a query has none"
  (check-equal? "GET /echo #f #f http/1.1 " (reply-body (curl (string-append base "/echo")))))

(test "a POST body arrives whole"
  (check-equal? "POST /echo #f #f http/1.1 hello"
    (reply-body (curl "--data-binary" "hello" (string-append base "/echo")))))

(test "a chunked POST body arrives whole"
  (check-equal? "POST /echo #f #f http/1.1 chunked body"
    (reply-body (curl "-H" "Transfer-Encoding: chunked" "--data-binary" "chunked body"
                  (string-append base "/echo")))))

(test "HEAD gets the headers without the body"
  (let ([r (curl "-I" (string-append base "/echo"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "26" (reply-header r "content-length"))
    (check (not (string-contains? (reply-body r) "GET /echo")))))

(test "a path registers a prefix on segment boundaries"
  (check-equal? "/prefix/sub/x" (reply-body (curl (string-append base "/prefix/sub/x"))))
  (check-equal? "/prefix" (reply-body (curl (string-append base "/prefix"))))
  (check-equal? 404 (reply-status (curl (string-append base "/prefixx"))))
  (check-equal? 404 (reply-status (curl (string-append base "/nowhere")))))

(test "the path is normalized before routing"
  (check-equal? "/prefix/x" (reply-body (curl "--path-as-is" (string-append base "/a/../prefix/./x")))))

(test "any status code can be sent"
  (check-equal? 418 (reply-status (curl (string-append base "/teapot"))))
  (let ([r (curl (string-append base "/empty"))])
    (check-equal? 204 (reply-status r))
    (check-equal? #f (reply-header r "content-length"))))

(test "a handler that raises gets a 500 and the loop keeps serving"
  (let ([before (length (log-items errors))])
    (check-equal? 500 (reply-status (curl (string-append base "/raise"))))
    (check-equal? 200 (reply-status (curl (string-append base "/echo"))))
    (check-equal? '("deliberate") (list-tail (log-items errors) before))))

(test "a handler that returns something other than a response gets a 500"
  (let ([before (length (log-items errors))])
    (check-equal? 500 (reply-status (curl (string-append base "/silent"))))
    (check-equal? '("handler returned neither a response nor a stream-response")
      (list-tail (log-items errors) before))))

(test "a header value with a line break is refused"
  (check-equal? 500 (reply-status (curl (string-append base "/bad-header")))))

(test "a streamed request body is read from a port in parts"
  (check-equal? "1000000 bytes"
    (reply-body (curl "--limit-rate" "2M" "--data-binary" (string-append "@" big-file)
                  (string-append base "/upload"))))
  (check (> (car (last-pair (log-items uploads))) 1)))

(test "a small streamed body is read in one part"
  (check-equal? "5 bytes" (reply-body (curl "--data-binary" "hello" (string-append base "/upload"))))
  (check-equal? 1 (car (last-pair (log-items uploads)))))

(test "a streaming handler may answer without reading the body"
  (check-equal? 202
    (reply-status (curl "--limit-rate" "2M" "--data-binary" (string-append "@" big-file)
                    (string-append base "/ignore-body"))))
  (check-equal? 200 (reply-status (curl (string-append base "/echo")))))

(test "a body over max-request-body-size gets 413"
  (let ([huge (string-append scratch "/huge")])
    (write-file! huge (make-bytevector 2100000 66))
    (check-equal? 413 (reply-status (curl "--data-binary" (string-append "@" huge)
                                     (string-append base "/echo"))))))

(test "static files come from H2O's file handler"
  (let ([r (curl (string-append base "/static/a.txt"))])
    (check-equal? 200 (reply-status r))
    (check-equal? "plain text\n" (reply-body r))
    (check-equal? "text/plain" (reply-header r "content-type"))
    (check (reply-header r "etag")))
  (check-equal? "<p>index</p>\n" (reply-body (curl (string-append base "/static/"))))
  (check-equal? 404 (reply-status (curl (string-append base "/static/missing"))))
  (check-equal? 301 (reply-status (curl (string-append base "/static/sub")))))

(test "a binary file uploaded through a multipart form arrives intact"
  (let ([file (string-append scratch "/binary")]
        [bytes (let ([bv (make-bytevector 100000)])
                 (for-each (lambda (i) (bytevector-u8-set! bv i (modulo (* i 7) 256))) (iota 100000))
                 bv)])
    (write-file! file bytes)
    (check-equal?
      "((\"title\" . \"a b\")) ((\"file\" \"binary\" \"application/octet-stream\"))"
      (reply-body (curl "-F" "title=a b" "-F" (string-append "file=@" file ";type=application/octet-stream")
                    (string-append base "/form"))))
    (check-equal? bytes (read-file received-upload)))
  (check-equal? "((\"a\" . \"1 2\")) ()"
    (reply-body (curl "--data-binary" "a=1+2" (string-append base "/form")))))

(test "cookies come from every cookie header and are set with set-cookie"
  (let ([r (curl "-H" "Cookie: a=1; b=2" "-H" "Cookie: c=3" (string-append base "/cookies"))])
    (check-equal? "((\"a\" . \"1\") (\"b\" . \"2\") (\"c\" . \"3\"))" (reply-body r))
    (check-equal? "seen=yes; Path=/" (reply-header r "set-cookie"))))

(test "a foreign Host reaches the first host when matching is not strict"
  (check-equal? 200 (reply-status (curl "-H" "Host: evil.example" (string-append base "/echo")))))

(test "bad arguments are refused before the server starts"
  (check-raises assertion-violation? (http-listener "127.0.0.1" 0))
  (check-raises assertion-violation? (make-host '("a:x") '()))
  (check-raises assertion-violation?
    (make-host '("a") (list (route "/x" (handler echo)) (route "/x" (handler echo)))))
  (check-raises assertion-violation? (route "x" (handler echo)))
  (check-raises assertion-violation? (route "/x"))
  (check-raises assertion-violation? (route "/x" (handler echo) (files "www")))
  (check-raises
    (lambda (c) (and (assertion-violation? c) (string-contains? (condition-message c) "whole body")))
    (route "/x" (files "www") (streaming-handler echo)))
  (route "/x" (files "www") (handler echo))
  (check-raises assertion-violation? (route "/x" (compress) (handler echo) (compress)))
  (check-raises assertion-violation? (handler 'echo))
  (check-raises assertion-violation? (max-age -1))
  (check-raises assertion-violation? (compress (encodings)))
  (check-raises assertion-violation? (compress (encodings zstd gzip) (qualities [br 5])))
  (check-raises assertion-violation? (compress (encodings br) 'fast))
  (check-raises assertion-violation? (qualities [br 12]))
  (check-raises assertion-violation? (qualities [gzip 0]))
  (check-raises assertion-violation? (qualities [zstd 3.0]))
  (compress (encodings br) (qualities [br 0]))
  (check-raises assertion-violation? (server-options [workers 0]))
  (check-raises assertion-violation?
    (start-server (list (http-listener "127.0.0.1" (+ port 1))) (list (make-host '("a") '())) 'none))
  (check-raises assertion-violation?
    (start-server (list (http-listener "127.0.0.1" (+ port 1))) (list (make-host '("a:1") '()))
      (server-options [strict-hosts #t]))))

(test "responses are checked when they are made"
  (check-raises assertion-violation? (response 99 '() ""))
  (check-raises assertion-violation? (response 204 '() "body"))
  (check-raises assertion-violation? (response 200 '(("content-length" . "4")) "body"))
  (for-each
    (lambda (name) (check-raises assertion-violation? (response 200 (list (cons name "x")) "body")))
    '("Transfer-Encoding" "connection" "keep-alive" "proxy-connection" "upgrade"))
  (check-raises assertion-violation? (response 200 '(("bad name" . "x")) "body"))
  (check-raises assertion-violation? (response 200 '() 'body))
  (check-equal? (string->utf8 "body") (response-body (response 200 '() "body"))))

(server-stop! server)

(test-exit)
