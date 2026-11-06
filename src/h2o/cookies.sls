(library (h2o cookies)
  (export
    request-cookies set-cookie-header
    cookie-max-age cookie-expires cookie-path cookie-domain cookie-secure cookie-http-only
    cookie-same-site same-site)
  (import (chezscheme) (h2o request) (h2o dates) (h2o text))

  (define-enumeration same-site (strict lax none) same-site-set)

  ;; kind names the attribute, so that each appears once and SameSite=None
  ;; can be checked for Secure.
  (define-record-type cookie-attribute
    (nongenerative) (sealed #t)
    (fields kind text))

  (define (cookie-octet? c)
    (let ([n (char->integer c)])
      (and (fx<= #x21 n #x7e) (not (memv c '(#\" #\, #\; #\\))))))

  (define (attribute-value? s)
    (and (string? s)
         (for-all (lambda (c) (and (char<=? #\space c #\~) (not (char=? c #\;)))) (string->list s))))

  (define (cookie-max-age seconds)
    (unless (and (integer? seconds) (exact? seconds))
      (assertion-violationf 'cookie-max-age "~s is not a number of seconds" seconds))
    (make-cookie-attribute 'max-age (format "Max-Age=~a" seconds)))

  (define (cookie-expires time)
    (unless (and (time? time) (eq? (time-type time) 'time-utc))
      (assertion-violationf 'cookie-expires "~s is not a time-utc time" time))
    (make-cookie-attribute 'expires (string-append "Expires=" (time->http-date time))))

  (define (cookie-path path)
    (unless (and (attribute-value? path) (fx> (string-length path) 0))
      (assertion-violationf 'cookie-path "~s is not a cookie path" path))
    (make-cookie-attribute 'path (string-append "Path=" path)))

  (define (cookie-domain domain)
    (unless (and (attribute-value? domain) (fx> (string-length domain) 0))
      (assertion-violationf 'cookie-domain "~s is not a cookie domain" domain))
    (make-cookie-attribute 'domain (string-append "Domain=" domain)))

  (define (cookie-secure) (make-cookie-attribute 'secure "Secure"))

  (define (cookie-http-only) (make-cookie-attribute 'http-only "HttpOnly"))

  (define (cookie-same-site value)
    (unless (enum-set-member? value (same-site-set strict lax none))
      (assertion-violationf 'cookie-same-site "~s is not made by same-site" value))
    (make-cookie-attribute 'same-site
      (string-append "SameSite="
        (case value [(strict) "Strict"] [(lax) "Lax"] [(none) "None"]))))

  (define (set-cookie-header name value . attributes)
    (unless (token? name) (assertion-violationf 'set-cookie-header "~s is not a cookie name" name))
    (unless (and (string? value) (for-all cookie-octet? (string->list value)))
      (assertion-violationf 'set-cookie-header "~s is not a cookie value" value))
    (unless (for-all cookie-attribute? attributes)
      (assertion-violationf 'set-cookie-header "~s are not all cookie attributes" attributes))
    (let ([kinds (map cookie-attribute-kind attributes)])
      (let loop ([kinds kinds])
        (when (pair? kinds)
          (when (memq (car kinds) (cdr kinds))
            (assertion-violationf 'set-cookie-header "the ~s attribute is given twice" (car kinds)))
          (loop (cdr kinds))))
      ;; Browsers drop a SameSite=None cookie that is not also Secure.
      (when (and (exists (lambda (a) (string=? (cookie-attribute-text a) "SameSite=None")) attributes)
                 (not (memq 'secure kinds)))
        (assertion-violationf 'set-cookie-header "SameSite=None needs cookie-secure")))
    (cons "set-cookie"
      (apply string-append name "=" value
        (map (lambda (a) (string-append "; " (cookie-attribute-text a))) attributes))))

  ;; HTTP/2 may split the cookie header into one field per cookie (RFC 9113
  ;; 8.2.3), so every cookie header is read.
  (define (request-cookies request)
    (unless (request? request) (assertion-violationf 'request-cookies "~s is not a request" request))
    (fold-right
      (lambda (header cookies)
        (if (string-ci=? (car header) "cookie")
            (fold-right
              (lambda (pair cookies)
                (let ([i (string-index pair #\=)])
                  (if (and i (fx> i 0))
                      (cons (cons (string-trim-blanks (substring pair 0 i))
                                  (unquote-value
                                    (string-trim-blanks
                                      (substring pair (fx+ i 1) (string-length pair)))))
                            cookies)
                      cookies)))
              cookies
              (map string-trim-blanks (string-split (cdr header) #\;)))
            cookies))
      '()
      (request-headers request))))
