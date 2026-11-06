(library (h2o ffi)
  (export
    iovec-size iovec-base iovec-len
    globalconf-size globalconf-hosts globalconf-max-request-entity-size
    globalconf-handshake-timeout globalconf-http1-req-timeout
    globalconf-http1-req-io-timeout globalconf-http1-upgrade-to-http2
    globalconf-http2-idle-timeout globalconf-http2-graceful-shutdown-timeout
    globalconf-http2-max-streams globalconf-http2-max-concurrent-requests
    globalconf-http2-max-concurrent-streaming-requests globalconf-send-informational-mode
    hostconf-strict-match context-size
    accept-ctx-size accept-ctx-ctx accept-ctx-hosts accept-ctx-ssl-ctx
    handler-size handler-on-req handler-supports-request-streaming
    handler-supports-request-streaming-mask
    generator-size generator-proceed generator-stop
    req-conn req-scheme req-authority req-method req-path req-query-at req-path-normalized
    req-version req-headers req-entity req-res-status req-res-reason
    req-res-content-length req-res-headers req-res-mime-attr mime-attributes-size
    req-write-req-cb req-write-req-ctx
    req-proceed-req req-pool
    url-scheme-name
    compress-args-size compress-args-min-size compress-args-gzip-quality
    compress-args-brotli-quality compress-args-zstd-quality
    expires-args-size expires-args-mode expires-args-max-age expires-mode-max-age
    file-flag-send-compressed
    vector-entries vector-size header-size header-name header-value
    socket-on-close-cb socket-on-close-data
    receiver-size message-size linklist-next linklist-prev
    send-state-in-progress send-state-final send-state-error send-informational-mode-all
    socket-flag-dont-read somaxconn shut-rdwr
    size-max

    c-memcpy-from c-memcpy-to c-memset c-close c-shutdown c-strerror
    listen-socket

    h2o-config-init h2o-config-dispose h2o-register-host
    h2o-config-register-path h2o-create-handler h2o-file-register
    h2o-compress-register h2o-expires-register
    h2o-context-init h2o-context-dispose h2o-context-request-shutdown
    h2o-context-close-idle-connections
    h2o-evloop-create h2o-evloop-destroy h2o-evloop-run
    h2o-evloop-socket-create h2o-evloop-socket-accept
    h2o-socket-read-start h2o-socket-read-stop h2o-socket-close
    h2o-socket-get-fd h2o-accept h2o-start-response h2o-send h2o-send-inline
    h2o-send-informational h2o-mimemap-get-default-attributes
    h2o-add-header-by-str h2o-mem-alloc-pool h2o-mem-alloc-shared
    h2o-multithread-create-queue h2o-multithread-destroy-queue
    h2o-multithread-register-receiver h2o-multithread-unregister-receiver
    h2o-multithread-send-message call-proceed-req peer-address

    make-tls-context ssl-ctx-free

    iovec->bytevector iovec->string pool-copy)
  (import (chezscheme))

  (include "build/h2o/layout.ss")

  (define h2o-library (load-shared-object h2o-shared-object))

  (define-ftype iovec (struct [base uptr] [len size_t]))

  (define (make-iovec bv)
    (let ([p (make-ftype-pointer iovec (foreign-alloc iovec-size))]
          [base (foreign-alloc (max 1 (bytevector-length bv)))])
      (c-memcpy-to base bv (bytevector-length bv))
      (ftype-set! iovec (base) p base)
      (ftype-set! iovec (len) p (bytevector-length bv))
      p))

  (define (free-iovec p)
    (foreign-free (ftype-ref iovec (base) p))
    (foreign-free (ftype-pointer-address p)))

  (define c-memcpy-from (foreign-procedure "memcpy" (u8* uptr size_t) uptr))
  (define c-memcpy-to (foreign-procedure "memcpy" (uptr u8* size_t) uptr))
  (define c-memset (foreign-procedure "memset" (uptr int size_t) uptr))
  (define c-socket (foreign-procedure __errno "socket" (int int int) int))
  (define c-setsockopt (foreign-procedure "setsockopt" (int int int u8* int) int))
  (define c-bind (foreign-procedure __errno "bind" (int u8* int) int))
  (define c-listen (foreign-procedure __errno "listen" (int int) int))
  (define c-close (foreign-procedure "close" (int) int))
  (define c-shutdown (foreign-procedure "shutdown" (int int) int))
  (define c-inet-pton (foreign-procedure "inet_pton" (int string u8*) int))
  (define c-strerror (foreign-procedure "strerror" (int) string))
  (define c-inet-ntop (foreign-procedure "inet_ntop" (int u8* u8* unsigned-32) uptr))

  (define h2o-config-init (foreign-procedure "h2o_config_init" (uptr) void))
  (define h2o-config-dispose (foreign-procedure "h2o_config_dispose" (uptr) void))
  (define h2o-config-register-host
    (foreign-procedure "h2o_config_register_host" (uptr (& iovec) unsigned-16) uptr))

  (define (h2o-register-host config name port)
    (let ([name (make-iovec (string->utf8 name))])
      (let ([hostconf (h2o-config-register-host config name port)])
        (free-iovec name)
        hostconf)))
  (define h2o-config-register-path
    (foreign-procedure "h2o_config_register_path" (uptr string int) uptr))
  (define h2o-create-handler (foreign-procedure "h2o_create_handler" (uptr size_t) uptr))
  (define h2o-file-register
    (foreign-procedure "h2o_file_register" (uptr string uptr uptr int) uptr))
  (define h2o-compress-register (foreign-procedure "h2o_compress_register" (uptr uptr) void))
  (define h2o-expires-register (foreign-procedure "h2o_expires_register" (uptr uptr) void))
  (define h2o-context-init (foreign-procedure "h2o_context_init" (uptr uptr uptr) void))
  (define h2o-context-dispose (foreign-procedure "h2o_context_dispose" (uptr) void))
  (define h2o-context-request-shutdown
    (foreign-procedure "h2o_context_request_shutdown" (uptr) void))
  (define h2o-context-close-idle-connections
    (foreign-procedure "h2o_context_close_idle_connections" (uptr size_t unsigned-64) void))
  (define h2o-evloop-create (foreign-procedure "h2o_evloop_create" () uptr))
  (define h2o-evloop-destroy (foreign-procedure "h2o_evloop_destroy" (uptr) void))
  (define h2o-evloop-run (foreign-procedure __collect_safe "h2o_evloop_run" (uptr int) int))
  (define h2o-evloop-socket-create
    (foreign-procedure "h2o_evloop_socket_create" (uptr int int) uptr))
  (define h2o-evloop-socket-accept
    (foreign-procedure "h2o_evloop_socket_accept" (uptr) uptr))
  (define h2o-socket-read-start (foreign-procedure "h2o_socket_read_start" (uptr uptr) void))
  (define h2o-socket-read-stop (foreign-procedure "h2o_socket_read_stop" (uptr) void))
  (define h2o-socket-close (foreign-procedure "h2o_socket_close" (uptr) void))
  (define h2o-socket-get-fd (foreign-procedure "h2o_socket_get_fd" (uptr) int))
  (define h2o-accept (foreign-procedure "h2o_accept" (uptr uptr) void))
  (define h2o-start-response (foreign-procedure "h2o_start_response" (uptr uptr) void))
  (define h2o-send (foreign-procedure "h2o_send" (uptr uptr size_t int) void))
  ;; h2o_send_inline copies the body before it can call back into Scheme,
  ;; so a movable bytevector is safe here.
  (define h2o-send-inline (foreign-procedure "h2o_send_inline" (uptr u8* size_t) void))
  (define h2o-send-informational (foreign-procedure "h2o_send_informational" (uptr) void))
  (define h2o-mimemap-get-default-attributes
    (foreign-procedure "h2o_mimemap_get_default_attributes" (string uptr) void))
  (define h2o-add-header-by-str
    (foreign-procedure "h2o_add_header_by_str"
      (uptr uptr uptr size_t int uptr uptr size_t) ssize_t))
  (define h2o-mem-alloc-pool
    (let ([alloc (foreign-procedure "h2o_mem__do_alloc_pool_aligned" (uptr size_t size_t) uptr)])
      (lambda (pool size) (alloc pool 8 size))))
  (define h2o-mem-alloc-shared
    (foreign-procedure "h2o_mem_alloc_shared" (uptr size_t uptr) uptr))
  (define h2o-multithread-create-queue
    (foreign-procedure "h2o_multithread_create_queue" (uptr) uptr))
  (define h2o-multithread-destroy-queue
    (foreign-procedure "h2o_multithread_destroy_queue" (uptr) void))
  (define h2o-multithread-register-receiver
    (foreign-procedure "h2o_multithread_register_receiver" (uptr uptr uptr) void))
  (define h2o-multithread-unregister-receiver
    (foreign-procedure "h2o_multithread_unregister_receiver" (uptr uptr) void))
  (define h2o-multithread-send-message
    (foreign-procedure __collect_safe "h2o_multithread_send_message" (uptr uptr) void))
  (define h2o-ssl-register-alpn-protocols
    (foreign-procedure "h2o_ssl_register_alpn_protocols" (uptr uptr) void))

  (define-ftype proceed-req (function (uptr uptr) void))
  (define (call-proceed-req req)
    (let ([f (foreign-ref 'uptr req req-proceed-req)])
      (unless (= f 0)
        ((ftype-ref proceed-req () (make-ftype-pointer proceed-req f)) req 0))))

  (define (address->string family address-bytes)
    (let ([text (make-bytevector inet6-addrstrlen 0)])
      (if (= 0 (c-inet-ntop family address-bytes text inet6-addrstrlen))
          #f
          (c-string->string text))))

  (define (sockaddr-bytes sa start count)
    (let ([bytes (make-bytevector count)])
      (bytevector-copy! sa start bytes 0 count)
      bytes))

  (define (peer-address req)
    (let* ([conn (foreign-ref 'uptr req req-conn)]
           [get-peername (foreign-ref 'uptr (foreign-ref 'uptr conn conn-callbacks)
                           conn-callbacks-get-peername)]
           [sa (make-bytevector sockaddr-storage-size 0)])
      (if (= 0 ((foreign-procedure get-peername (uptr u8*) unsigned-32) conn sa))
          (values #f #f)
          (let ([family (bytevector-u16-native-ref sa sockaddr-family)])
            (cond
              [(= family af-inet)
               (values (address->string af-inet (sockaddr-bytes sa sockaddr-in-addr 4))
                       (bytevector-u16-ref sa sockaddr-in-port (endianness big)))]
              [(= family af-inet6)
               (values (address->string af-inet6 (sockaddr-bytes sa sockaddr-in6-addr 16))
                       (bytevector-u16-ref sa sockaddr-in6-port (endianness big)))]
              [else (values #f #f)])))))

  (define tls-server-method (foreign-procedure "TLS_server_method" () uptr))
  (define ssl-ctx-new (foreign-procedure "SSL_CTX_new" (uptr) uptr))
  (define ssl-ctx-free (foreign-procedure "SSL_CTX_free" (uptr) void))
  (define ssl-ctx-use-certificate-chain-file
    (foreign-procedure "SSL_CTX_use_certificate_chain_file" (uptr string) int))
  (define ssl-ctx-use-private-key-file
    (foreign-procedure "SSL_CTX_use_PrivateKey_file" (uptr string int) int))
  (define ssl-ctx-check-private-key
    (foreign-procedure "SSL_CTX_check_private_key" (uptr) int))
  (define err-get-error (foreign-procedure "ERR_get_error" () unsigned-long))
  (define err-error-string-n
    (foreign-procedure "ERR_error_string_n" (unsigned-long u8* size_t) void))

  (define (c-string->string bv)
    (let loop ([n 0])
      (if (= (bytevector-u8-ref bv n) 0)
          (utf8->string (let ([s (make-bytevector n)]) (bytevector-copy! bv 0 s 0 n) s))
          (loop (+ n 1)))))

  (define (iovec->bytevector address)
    (let ([base (foreign-ref 'uptr address iovec-base)]
          [len (foreign-ref 'size_t address iovec-len)])
      (let ([bv (make-bytevector len)])
        (unless (= len 0) (c-memcpy-from bv base len))
        bv)))

  (define (iovec->string address)
    (utf8->string (iovec->bytevector address)))

  (define (pool-copy pool bv)
    (let* ([n (bytevector-length bv)] [p (h2o-mem-alloc-pool pool (max n 1))])
      (unless (= n 0) (c-memcpy-to p bv n))
      p))

  (define (openssl-error)
    (let ([code (err-get-error)] [buffer (make-bytevector 256 0)])
      (if (= code 0)
          "no OpenSSL error"
          (begin (err-error-string-n code buffer 256) (c-string->string buffer)))))

  (define (make-tls-context who certificate key)
    (let ([ctx (ssl-ctx-new (tls-server-method))])
      (when (= ctx 0) (errorf who "cannot create a TLS context: ~a" (openssl-error)))
      (unless (and (= 1 (ssl-ctx-use-certificate-chain-file ctx certificate))
                   (= 1 (ssl-ctx-use-private-key-file ctx key ssl-filetype-pem))
                   (= 1 (ssl-ctx-check-private-key ctx)))
        (let ([message (openssl-error)])
          (ssl-ctx-free ctx)
          (errorf who "cannot load TLS certificate ~a and key ~a: ~a" certificate key message)))
      (h2o-ssl-register-alpn-protocols ctx (foreign-entry "h2o_http2_alpn_protocols"))
      ctx))

  (define (socket-address who address port)
    (let ([address-bytes (make-bytevector 16 0)])
      (cond
        [(= 1 (c-inet-pton af-inet address address-bytes))
         (let ([sa (make-bytevector sockaddr-in-size 0)])
           (bytevector-u16-native-set! sa sockaddr-in-family af-inet)
           (bytevector-u16-set! sa sockaddr-in-port port (endianness big))
           (bytevector-copy! address-bytes 0 sa sockaddr-in-addr 4)
           (values af-inet sa))]
        [(= 1 (c-inet-pton af-inet6 address address-bytes))
         (let ([sa (make-bytevector sockaddr-in6-size 0)])
           (bytevector-u16-native-set! sa sockaddr-in6-family af-inet6)
           (bytevector-u16-set! sa sockaddr-in6-port port (endianness big))
           (bytevector-copy! address-bytes 0 sa sockaddr-in6-addr 16)
           (values af-inet6 sa))]
        [else (assertion-violationf who "~s is not a numeric IPv4 or IPv6 address" address)])))

  (define (set-socket-option! fd level option)
    (let ([one (make-bytevector 4 0)])
      (bytevector-s32-native-set! one 0 1)
      (c-setsockopt fd level option one 4)))

  (define (listen-socket who address port)
    (let-values ([(family sa) (socket-address who address port)])
      (let-values ([(fd errno) (c-socket family (logor sock-stream sock-cloexec) 0)])
        (when (< fd 0)
          (errorf who "cannot create a socket for ~a: ~a" address (c-strerror errno)))
        (set-socket-option! fd sol-socket so-reuseaddr)
        (when (= family af-inet6) (set-socket-option! fd ipproto-ipv6 ipv6-v6only))
        (let*-values ([(bound errno) (c-bind fd sa (bytevector-length sa))]
                      [(listening errno) (if (= bound 0) (c-listen fd somaxconn) (values -1 errno))])
          (unless (= listening 0)
            (c-close fd)
            (errorf who "cannot listen on ~a port ~a: ~a" address port (c-strerror errno))))
        fd))))
