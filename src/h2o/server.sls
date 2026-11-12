(library (h2o server)
  (export
    start-server server? server-stop! server-options
    http-listener https-listener make-host
    route handler streaming-handler loop-handler files compress encodings qualities max-age
    send-early-hints!
    response response? response-status response-headers response-body stream-response
    make-stream stream? stream-send! stream-close! stream-open?)
  (import (chezscheme) (h2o ffi) (h2o request) (h2o text))

  (include "server/options.ss")
  (include "server/routes.ss")
  (include "server/stream.ss")
  (include "server/response.ss")
  (include "server/body.ss")

  ;; The mutex guards state, failure, work, wake-sent?, threads,
  ;; thread-ids, joining? and joined?. core is set before the server runs
  ;; and only read after.
  (define-record-type server
    (nongenerative) (sealed #t)
    (fields (immutable options server-options-value) mutex condition jobs
            (mutable state) (mutable failure) (mutable work) (mutable wake-sent?) (mutable core)
            (mutable threads) (mutable thread-ids) (mutable joining?) (mutable joined?))
    (protocol
      (lambda (new)
        (lambda (options)
          (new options (make-mutex) (make-condition) (make-jobs)
            'starting #f '() #f #f '() '() #f #f)))))

  ;; Only the loop thread touches a core, except that other threads read
  ;; thread-id, receiver and message, which are set before the server runs.
  (define-record-type core
    (nongenerative) (sealed #t)
    (fields thread-id
            (mutable receiver) (mutable message) (mutable config) (mutable context)
            (mutable evloop) (mutable queue) (mutable generator)
            (mutable accept-contexts) (mutable tls-contexts) (mutable callables)
            (mutable handlers) (mutable entries)
            listeners sockets streams exchanges (mutable deadline) (mutable forced?))
    (protocol
      (lambda (new)
        (lambda ()
          (new (get-thread-id) #f #f #f #f #f #f #f '() '() '() '#() '()
            (make-eqv-hashtable) (make-eqv-hashtable) (make-eqv-hashtable) (make-eqv-hashtable)
            0 #f)))))

  ;; active counts the jobs queued or running, so that a stop can wait for
  ;; them.
  (define-record-type jobs
    (nongenerative) (sealed #t)
    (fields mutex condition (mutable front) (mutable back) (mutable active) (mutable stopping?))
    (protocol
      (lambda (new) (lambda () (new (make-mutex) (make-condition) '() '() 0 #f)))))

  (define-record-type exchange
    (nongenerative) (sealed #t)
    (fields server req (mutable live?) (mutable answered?) (mutable feed)))

  ;; A report must not raise: it runs inside callbacks from C and on
  ;; worker threads.
  (define (report server condition)
    (define (print c) (guard (e [#t (void)]) (report-to-standard-error c)))
    (guard (c [#t (print condition) (print c)])
      ((options-on-error (server-options-value server)) condition)))

  (define (problem who message . irritants)
    (condition (make-error) (make-who-condition who) (make-message-condition message)
               (make-irritants-condition irritants)))

  (define (now-ms)
    (let ([t (current-time 'time-monotonic)])
      (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

  (define (on-loop-thread? server)
    (eqv? (get-thread-id) (core-thread-id (server-core server))))

  ;; Wakes the loop thread with one preallocated message: a message is sent
  ;; only when the work list becomes non-empty, and the loop thread unlinks
  ;; it before taking the list, so H2O never sees it linked twice.
  (define (post! server thunk)
    (with-mutex (server-mutex server)
      (cond
        [(memq (server-state server) '(running draining))
         (server-work-set! server (cons thunk (server-work server)))
         (unless (server-wake-sent? server)
           (server-wake-sent?-set! server #t)
           (let ([core (server-core server)])
             (h2o-multithread-send-message (core-receiver core) (core-message core))))
         #t]
        [else #f])))

  (define (run-posted! server anchor)
    (let ([message (core-message (server-core server))])
      (foreign-set! 'uptr anchor linklist-next anchor)
      (foreign-set! 'uptr anchor linklist-prev anchor)
      (foreign-set! 'uptr message linklist-next 0)
      (foreign-set! 'uptr message linklist-prev 0))
    (let ([work (with-mutex (server-mutex server)
                  (let ([work (server-work server)])
                    (server-work-set! server '())
                    (server-wake-sent?-set! server #f)
                    work))])
      (for-each
        (lambda (thunk) (guard (c [#t (report server c)]) (thunk)))
        (reverse work))))

  (define (enqueue! server job)
    (let ([jobs (server-jobs server)])
      (with-mutex (jobs-mutex jobs)
        (jobs-back-set! jobs (cons job (jobs-back jobs)))
        (jobs-active-set! jobs (fx+ (jobs-active jobs) 1))
        (condition-signal (jobs-condition jobs)))))

  (define (active-jobs server)
    (let ([jobs (server-jobs server)])
      (with-mutex (jobs-mutex jobs) (jobs-active jobs))))

  (define (next-job jobs)
    (with-mutex (jobs-mutex jobs)
      (let wait ()
        (cond
          [(jobs-stopping? jobs) #f]
          [(pair? (jobs-front jobs))
           (let ([job (car (jobs-front jobs))])
             (jobs-front-set! jobs (cdr (jobs-front jobs)))
             job)]
          [(pair? (jobs-back jobs))
           (jobs-front-set! jobs (reverse (jobs-back jobs)))
           (jobs-back-set! jobs '())
           (wait)]
          [else (condition-wait (jobs-condition jobs) (jobs-mutex jobs)) (wait)]))))

  (define (work! server)
    (with-mutex (server-mutex server)
      (server-thread-ids-set! server (cons (get-thread-id) (server-thread-ids server))))
    (let ([jobs (server-jobs server)])
      (let loop ()
        (let ([job (next-job jobs)])
          (when job
            (guard (c [#t (report server c)]) (job))
            (with-mutex (jobs-mutex jobs) (jobs-active-set! jobs (fx- (jobs-active jobs) 1)))
            (loop))))))

  (define (stop-workers! server)
    (let ([jobs (server-jobs server)])
      (with-mutex (jobs-mutex jobs)
        (jobs-stopping?-set! jobs #t)
        (jobs-front-set! jobs '())
        (jobs-back-set! jobs '())
        (condition-broadcast (jobs-condition jobs)))))

  (define start-server
    (case-lambda
      [(listeners hosts) (start-server listeners hosts (server-options))]
      [(listeners hosts options)
       (unless (and (list? listeners) (pair? listeners) (for-all listener? listeners))
         (assertion-violationf 'start-server "~s is not a list of listeners" listeners))
       (unless (and (list? hosts) (pair? hosts) (for-all host? hosts))
         (assertion-violationf 'start-server "~s is not a list of hosts" hosts))
       (unless (options? options)
         (assertion-violationf 'start-server "~s is not made by server-options" options))
       (let ([names (apply append (map host-names hosts))])
         ;; With one host H2O skips host matching, and a strict host is
         ;; then never chosen.
         (when (and (null? (cdr names)) (options-strict-hosts options))
           (assertion-violationf 'start-server
             "strict host matching needs at least two host names, not ~s" names)))
       (let* ([server (make-server options)]
              [loop-thread (fork-thread (lambda () (run-server server listeners hosts)))])
         (with-mutex (server-mutex server)
           (let wait ()
             (when (eq? (server-state server) 'starting)
               (condition-wait (server-condition server) (server-mutex server))
               (wait))))
         (when (eq? (server-state server) 'failed)
           (thread-join loop-thread)
           (raise (server-failure server)))
         (let ([workers (map (lambda (i) (fork-thread (lambda () (work! server))))
                             (iota (options-workers options)))])
           (with-mutex (server-mutex server)
             (server-threads-set! server (cons loop-thread workers))))
         server)]))

  ;; A thread of the server waiting for its own threads would wait forever.
  ;; A second caller waits for the first caller's joins: returning earlier
  ;; would let it go on while a worker still runs a handler.
  (define (server-stop! server)
    (unless (server? server) (assertion-violationf 'server-stop! "~s is not a server" server))
    (when (or (on-loop-thread? server)
              (with-mutex (server-mutex server)
                (memv (get-thread-id) (server-thread-ids server))))
      (assertion-violationf 'server-stop! "~s cannot be stopped from one of its own threads" server))
    (post! server (lambda () (begin-drain! server)))
    (let ([threads (with-mutex (server-mutex server)
                     (let wait ()
                       (unless (memq (server-state server) '(stopped failed))
                         (condition-wait (server-condition server) (server-mutex server))
                         (wait)))
                     (cond
                       [(server-joining? server) #f]
                       [else (server-joining?-set! server #t) (server-threads server)]))])
      (cond
        [threads
         (stop-workers! server)
         (for-each thread-join threads)
         (with-mutex (server-mutex server)
           (server-joined?-set! server #t)
           (condition-broadcast (server-condition server)))]
        [else
         (with-mutex (server-mutex server)
           (let wait ()
             (unless (server-joined? server)
               (condition-wait (server-condition server) (server-mutex server))
               (wait))))])))

  (define (finish! server state failure)
    (with-mutex (server-mutex server)
      (server-state-set! server state)
      (server-failure-set! server failure)
      (condition-broadcast (server-condition server))))

  (define (run-server server listeners hosts)
    (server-core-set! server (make-core))
    (when (guard (c [#t (guard (c [#t (report server c)]) (dispose! server))
                        (finish! server 'failed c)
                        #f])
            (setup! server listeners hosts)
            #t)
      (finish! server 'running #f)
      (guard (c [#t (report server c) (abort-exchanges! server) (finish! server 'failed c)])
        (serve! server))))

  (define (serve! server)
    (let ([core (server-core server)])
      (let loop ()
        (h2o-evloop-run (core-evloop core) (if (eq? (server-state server) 'running) 1000 10))
        ;; H2O's HTTP/1 shutdown request does nothing, and a connection kept
        ;; alive becomes idle after each response, so idle ones are closed on
        ;; every pass while draining.
        (unless (eq? (server-state server) 'running)
          (h2o-context-close-idle-connections (core-context core) size-max 0))
        (let ([sockets (hashtable-size (core-sockets core))])
          (cond
            [(eq? (server-state server) 'running) (loop)]
            [(and (fx= 0 sockets) (or (core-forced? core) (fx= 0 (active-jobs server))))
             (stop-loop! server)]
            [(and (not (core-forced? core)) (>= (now-ms) (core-deadline core)))
             (force-close! server)
             (loop)]
            ;; Every socket left has had shutdown(2); one that still does not
            ;; close is abandoned with its H2O state rather than freed in use.
            [(and (core-forced? core) (>= (now-ms) (+ (core-deadline core) 1000)))
             (report server (problem 'server-stop! "connections did not close; leaving them open"
                              sockets))
             (abort-exchanges! server)
             (finish! server 'stopped #f)]
            [else (loop)])))))

  ;; H2O refuses to unregister a receiver with a message still queued, and
  ;; a thread may have posted one just before the state changed.
  (define (stop-loop! server)
    (with-mutex (server-mutex server) (server-state-set! server 'disposing))
    (let drain ()
      (when (with-mutex (server-mutex server) (server-wake-sent? server))
        (h2o-evloop-run (core-evloop (server-core server)) 10)
        (drain)))
    (abort-exchanges! server)
    (dispose! server)
    (finish! server 'stopped #f))

  ;; A worker blocked reading a request body would otherwise wait for a
  ;; part that never comes, and the stop would never join it.
  (define (abort-exchanges! server)
    (let ([core (server-core server)])
      (vector-for-each end-exchange! (hashtable-values (core-exchanges core)))
      (hashtable-clear! (core-exchanges core))))

  (define (end-exchange! exchange)
    (exchange-live?-set! exchange #f)
    (let ([feed (exchange-feed exchange)]) (when feed (feed-abort! feed))))

  (define (begin-drain! server)
    (let ([core (server-core server)])
      (when (eq? (server-state server) 'running)
        (with-mutex (server-mutex server) (server-state-set! server 'draining))
        (close-listeners! server)
        (vector-for-each stream-close! (hashtable-values (core-streams core)))
        (h2o-context-request-shutdown (core-context core))
        (core-deadline-set! core
          (+ (now-ms) (options-shutdown-timeout (server-options-value server)))))))

  ;; shutdown(2) makes H2O's own next read or write fail, so H2O closes and
  ;; frees each connection itself. H2O neither reads nor writes a
  ;; connection whose handler is still running, so each such request is
  ;; answered to give it a write that fails.
  (define (force-close! server)
    (let ([core (server-core server)])
      (core-forced?-set! core #t)
      (vector-for-each
        (lambda (socket) (c-shutdown (h2o-socket-get-fd socket) shut-rdwr))
        (hashtable-keys (core-sockets core)))
      (vector-for-each
        (lambda (exchange) (answer! server exchange service-unavailable))
        (hashtable-values (core-exchanges core)))))

  (define (close-listeners! server)
    (let ([core (server-core server)])
      (vector-for-each
        (lambda (socket) (h2o-socket-read-stop socket) (h2o-socket-close socket))
        (hashtable-keys (core-listeners core)))
      (hashtable-clear! (core-listeners core))))

  (define (callable! server code)
    (let ([core (server-core server)])
      (lock-object code)
      (core-callables-set! core (cons code (core-callables core)))
      (foreign-callable-entry-point code)))

  ;; Each callback catches every condition: one that escaped would unwind
  ;; through H2O's C frames.
  (define-syntax callback
    (syntax-rules ()
      [(_ server failed-value (arg ...) body ...)
       (lambda (arg ...)
         (guard (c [#t (report server c) failed-value])
           body ...))]))

  (define (make-callables! server)
    (let ([core (server-core server)])
      (core-entries-set! core
        (list
          (cons 'request
            (callable! server
              (foreign-callable __collect_safe
                (callback server 0 (h2o-handler req)
                  (handle-request! server
                    (vector-ref (core-handlers core) (foreign-ref 'uptr h2o-handler handler-size))
                    req)
                  0)
                (uptr uptr) int)))
          (cons 'accept
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (listener error)
                  (when (= error 0) (accept! server listener)))
                (uptr uptr) void)))
          (cons 'socket-close
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (socket)
                  (hashtable-delete! (core-sockets core) socket))
                (uptr) void)))
          (cons 'proceed
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (generator req)
                  (let ([stream (hashtable-ref (core-streams core) req #f)])
                    (when stream
                      (let-values ([(action parts) (stream-proceeded! stream)])
                        (perform! server req stream action parts)))))
                (uptr uptr) void)))
          (cons 'stop
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (generator req)
                  (let ([stream (hashtable-ref (core-streams core) req #f)])
                    (when stream
                      (hashtable-delete! (core-streams core) req)
                      (stream-ended! stream)
                      (notify-close server stream))))
                (uptr uptr) void)))
          (cons 'body
            (callable! server
              (foreign-callable __collect_safe
                (callback server 0 (req end-stream)
                  (let ([exchange (hashtable-ref (core-exchanges core) req #f)])
                    (when (and exchange (exchange-feed exchange))
                      (feed-deliver! (exchange-feed exchange) (iovec->bytevector (+ req req-entity))
                        (not (= end-stream 0)))))
                  0)
                (uptr int) int)))
          (cons 'request-dispose
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (hook)
                  (let* ([req (foreign-ref 'uptr hook 0)]
                         [exchange (hashtable-ref (core-exchanges core) req #f)])
                    (when exchange
                      (hashtable-delete! (core-exchanges core) req)
                      (end-exchange! exchange))))
                (uptr) void)))
          (cons 'messages
            (callable! server
              (foreign-callable __collect_safe
                (callback server (void) (receiver anchor)
                  (run-posted! server anchor))
                (uptr uptr) void)))))))

  (define (entry server name) (cdr (assq name (core-entries (server-core server)))))

  (define (setup! server listeners hosts)
    (let ([core (server-core server)])
      (make-callables! server)
      (let ([config (foreign-alloc globalconf-size)] [options (server-options-value server)])
        (h2o-config-init config)
        (core-config-set! core config)
        (apply-options! config options)
        (foreign-set! 'int config globalconf-http1-upgrade-to-http2 0)
        ;; H2O sends 1xx responses only over HTTP/2 and later by default;
        ;; RFC 9110 15.2 requires HTTP/1.1 clients to accept them too.
        (foreign-set! 'int config globalconf-send-informational-mode send-informational-mode-all)
        (register-hosts! server config hosts (options-strict-hosts options))
        (let ([loop (h2o-evloop-create)])
          (core-evloop-set! core loop)
          (let ([context (foreign-alloc context-size)])
            (h2o-context-init context loop config)
            (core-context-set! core context))
          (let ([queue (h2o-multithread-create-queue loop)])
            (core-queue-set! core queue)
            (let ([receiver (foreign-alloc receiver-size)] [message (foreign-alloc message-size)])
              (c-memset message 0 message-size)
              (core-message-set! core message)
              (h2o-multithread-register-receiver queue receiver (entry server 'messages))
              (core-receiver-set! core receiver)))
          (let ([generator (foreign-alloc generator-size)])
            (foreign-set! 'uptr generator generator-proceed (entry server 'proceed))
            (foreign-set! 'uptr generator generator-stop (entry server 'stop))
            (core-generator-set! core generator))
          (for-each (lambda (listener) (open-listener! server config loop listener)) listeners)))))

  (define (apply-options! config options)
    (define (set-option! type offset value)
      (when value (foreign-set! type config offset value)))
    (set-option! 'size_t globalconf-max-request-entity-size (options-max-request-body-size options))
    (set-option! 'unsigned-64 globalconf-handshake-timeout (options-handshake-timeout options))
    (set-option! 'unsigned-64 globalconf-http1-req-timeout (options-http1-request-timeout options))
    (set-option! 'unsigned-64 globalconf-http1-req-io-timeout
      (options-http1-request-io-timeout options))
    (set-option! 'unsigned-64 globalconf-http2-idle-timeout (options-http2-idle-timeout options))
    (set-option! 'unsigned-64 globalconf-http2-graceful-shutdown-timeout
      (options-http2-graceful-shutdown-timeout options))
    (set-option! 'unsigned-32 globalconf-http2-max-streams (options-http2-max-streams options))
    (set-option! 'size_t globalconf-http2-max-concurrent-requests
      (options-http2-max-concurrent-requests options))
    (set-option! 'size_t globalconf-http2-max-concurrent-streaming-requests
      (options-http2-max-concurrent-streaming-requests options)))

  (define (register-hosts! server config hosts strict?)
    (let ([handlers (make-eq-hashtable)] [order '()])
      (for-each
        (lambda (host)
          (for-each
            (lambda (r)
              (for-each
                (lambda (item)
                  (when (and (handler-item? item) (not (hashtable-contains? handlers item)))
                    (hashtable-set! handlers item (length order))
                    (set! order (cons item order))))
                (route-handlers r)))
            (host-routes host)))
        hosts)
      (core-handlers-set! (server-core server) (list->vector (reverse order)))
      (for-each
        (lambda (host)
          (for-each
            (lambda (name)
              (let-values ([(hostname port) (split-host-name 'start-server name)])
                (let ([hostconf (h2o-register-host config hostname port)])
                  (when (= hostconf 0)
                    (assertion-violationf 'start-server "host ~s is given twice" name))
                  (when strict? (foreign-set! 'unsigned-8 hostconf hostconf-strict-match 1))
                  (for-each
                    (lambda (r) (register-route! server hostconf r handlers))
                    (host-routes host)))))
            (host-names host)))
        hosts)))

  (define index-file "index.html")

  (define (register-files! pathconf directory)
    (let ([name (string->utf8 index-file)]
          [names (foreign-alloc (fx* 2 (foreign-sizeof 'uptr)))])
      (let ([c-name (foreign-alloc (fx+ (bytevector-length name) 1))])
        (c-memcpy-to c-name name (bytevector-length name))
        (foreign-set! 'unsigned-8 c-name (bytevector-length name) 0)
        (foreign-set! 'uptr names 0 c-name)
        (foreign-set! 'uptr names (foreign-sizeof 'uptr) 0)
        ;; H2O copies the directory and index names.
        (h2o-file-register pathconf directory names 0 file-flag-send-compressed)
        (foreign-free c-name)
        (foreign-free names))))

  (define (register-compress! pathconf item)
    (let ([args (foreign-alloc compress-args-size)])
      (c-memset args 0 compress-args-size)
      (foreign-set! 'size_t args compress-args-min-size 100)
      (foreign-set! 'int args compress-args-gzip-quality (or (compress-item-gzip item) -1))
      (foreign-set! 'int args compress-args-brotli-quality (or (compress-item-br item) -1))
      (foreign-set! 'int args compress-args-zstd-quality (or (compress-item-zstd item) -1))
      (h2o-compress-register pathconf args)
      (foreign-free args)))

  (define (register-max-age! pathconf seconds)
    (let ([args (foreign-alloc expires-args-size)])
      (c-memset args 0 expires-args-size)
      (foreign-set! 'int args expires-args-mode expires-mode-max-age)
      (foreign-set! 'unsigned-64 args expires-args-max-age seconds)
      (h2o-expires-register pathconf args)
      (foreign-free args)))

  (define (register-route! server hostconf r handlers)
    (let ([pathconf (h2o-config-register-path hostconf (route-path r) 0)])
      (for-each
        (lambda (item)
          (if (files-item? item)
              (register-files! pathconf (files-item-directory item))
              (let ([handler (h2o-create-handler pathconf (+ handler-size 8))])
                (foreign-set! 'uptr handler handler-on-req (entry server 'request))
                (foreign-set! 'uptr handler handler-size (hashtable-ref handlers item #f))
                (when (eq? (handler-item-kind item) 'streaming)
                  (foreign-set! 'unsigned-8 handler handler-supports-request-streaming
                    (logor handler-supports-request-streaming-mask
                      (foreign-ref 'unsigned-8 handler handler-supports-request-streaming)))))))
        (route-handlers r))
      (when (route-compress r) (register-compress! pathconf (route-compress r)))
      (when (route-max-age r) (register-max-age! pathconf (route-max-age r)))))

  (define (open-listener! server config loop listener)
    (let ([core (server-core server)])
      (let ([accept (foreign-alloc accept-ctx-size)])
        (c-memset accept 0 accept-ctx-size)
        (core-accept-contexts-set! core (cons accept (core-accept-contexts core)))
        (foreign-set! 'uptr accept accept-ctx-ctx (core-context core))
        (foreign-set! 'uptr accept accept-ctx-hosts (foreign-ref 'uptr config globalconf-hosts))
        (when (listener-certificate listener)
          (let ([tls (make-tls-context 'start-server (listener-certificate listener)
                       (listener-key listener))])
            (core-tls-contexts-set! core (cons tls (core-tls-contexts core)))
            (foreign-set! 'uptr accept accept-ctx-ssl-ctx tls)))
        (let ([socket (h2o-evloop-socket-create loop
                        (listen-socket 'start-server (listener-address listener)
                          (listener-port listener))
                        socket-flag-dont-read)])
          (hashtable-set! (core-listeners core) socket accept)
          (h2o-socket-read-start socket (entry server 'accept))))))

  (define (accept! server listener)
    (let ([core (server-core server)])
      (let ([accept (hashtable-ref (core-listeners core) listener #f)]
            [socket (h2o-evloop-socket-accept listener)])
        (when (and accept (not (= socket 0)))
          (foreign-set! 'uptr socket socket-on-close-cb (entry server 'socket-close))
          (foreign-set! 'uptr socket socket-on-close-data socket)
          (hashtable-set! (core-sockets core) socket #t)
          (h2o-accept accept socket)))))

  ;; Handles partial setup too, so a failed start frees what it made.
  (define (dispose! server)
    (let ([core (server-core server)])
      (close-listeners! server)
      (when (core-receiver core)
        (h2o-multithread-unregister-receiver (core-queue core) (core-receiver core)))
      (when (core-queue core) (h2o-multithread-destroy-queue (core-queue core)))
      (when (core-context core) (h2o-context-dispose (core-context core)))
      (when (core-evloop core) (h2o-evloop-destroy (core-evloop core)))
      (when (core-config core) (h2o-config-dispose (core-config core)))
      (for-each ssl-ctx-free (core-tls-contexts core))
      (for-each
        (lambda (p) (when p (foreign-free p)))
        (append (core-accept-contexts core)
          (list (core-receiver core) (core-message core) (core-generator core)
                (core-context core) (core-config core))))
      (for-each unlock-object (core-callables core))))

  (define (version-symbol version)
    (let ([known (assv version '((#x100 . http/1.0) (#x101 . http/1.1) (#x200 . http/2)
                                 (#x300 . http/3)))])
      (assert known)
      (cdr known)))

  (define (bytevector-tail bv start)
    (let ([tail (make-bytevector (fx- (bytevector-length bv) start))])
      (bytevector-copy! bv start tail 0 (bytevector-length tail))
      tail))

  (define (request-header-list req)
    (let ([entries (foreign-ref 'uptr req (+ req-headers vector-entries))])
      (let loop ([i (fx- (foreign-ref 'size_t req (+ req-headers vector-size)) 1)] [headers '()])
        (if (fx< i 0)
            headers
            (let ([h (+ entries (* i header-size))])
              (loop (fx- i 1)
                (cons (cons (iovec->string (foreign-ref 'uptr h header-name))
                            (iovec->string (+ h header-value)))
                      headers)))))))

  (define (snapshot-request req exchange body port)
    (let ([query-at (foreign-ref 'size_t req req-query-at)])
      (let-values ([(address port-number) (peer-address req)])
        (make-request
          (iovec->string (+ req req-method))
          (iovec->string (+ req req-path-normalized))
          (and (not (= query-at size-max))
               (utf8->string (bytevector-tail (iovec->bytevector (+ req req-path)) (+ query-at 1))))
          (iovec->string (+ req req-authority))
          (iovec->string (+ (foreign-ref 'uptr req req-scheme) url-scheme-name))
          (version-symbol (foreign-ref 'int req req-version))
          (request-header-list req)
          address port-number body port exchange))))

  ;; H2O frees the request when its client goes; the hook marks the
  ;; exchange so that a worker's late answer is dropped.
  (define (register-exchange! server exchange)
    (let ([req (exchange-req exchange)])
      (hashtable-set! (core-exchanges (server-core server)) req exchange)
      (foreign-set! 'uptr
        (h2o-mem-alloc-shared (+ req req-pool) 8 (entry server 'request-dispose)) 0 req)))

  ;; H2O counts a request as handled once on_req returns, so one that no
  ;; worker owns and no response has started is answered here, or its
  ;; connection would hang. Its exchange is ended too: no dispose hook will
  ;; mark it, and a proceed already posted for its body would reach a freed
  ;; request.
  (define (handle-request! server item req)
    (let ([exchange (make-exchange server req #t #f #f)])
      (guard (c [#t (report server c)
                    (unless (hashtable-contains? (core-exchanges (server-core server)) req)
                      (end-exchange! exchange)
                      (when (fx= 0 (foreign-ref 'int req req-res-status))
                        (send-response! req internal-error)))])
        (let ([proc (handler-item-procedure item)])
          (case (handler-item-kind item)
            [(loop)
             (answer! server exchange
               (run-handler server proc
                 (snapshot-request req exchange (iovec->bytevector (+ req req-entity)) #f)))]
            [(worker)
             (let ([request (snapshot-request req exchange (iovec->bytevector (+ req req-entity)) #f)])
               (register-exchange! server exchange)
               (enqueue! server
                 (lambda ()
                   (post-answer! server exchange (run-handler server proc request)))))]
            [(streaming)
             (let ([request (snapshot-request req exchange #f (open-body! server exchange req))])
               (register-exchange! server exchange)
               (enqueue! server
                 (lambda ()
                   (let ([result (run-handler server proc request)])
                     (let ([feed (exchange-feed exchange)]) (when feed (feed-finish! feed)))
                     (post-answer! server exchange result)))))])))))

  ;; A worker can outlive the loop after a forced stop; its stream is then
  ;; ended here, or its on-close would never run.
  (define (post-answer! server exchange result)
    (unless (post! server (lambda () (answer! server exchange result)))
      (drop-answer! server result)))

  ;; Without proceed_req, H2O has the whole body already. With it, the
  ;; first part is in the request and H2O passes each later part to the
  ;; body callback once proceed_req has been called for the one before.
  (define (open-body! server exchange req)
    (let ([first (iovec->bytevector (+ req req-entity))])
      (if (= 0 (foreign-ref 'uptr req req-proceed-req))
          (open-bytevector-input-port first)
          (let ([feed (make-feed (lambda () (post! server (lambda () (proceed-body! exchange)))))])
            (exchange-feed-set! exchange feed)
            (foreign-set! 'uptr req req-write-req-cb (entry server 'body))
            (foreign-set! 'uptr req req-write-req-ctx req)
            (if (fx= 0 (bytevector-length first))
                (post! server (lambda () (proceed-body! exchange)))
                (feed-deliver! feed first #f))
            (feed-port feed)))))

  (define (proceed-body! exchange)
    (when (exchange-live? exchange)
      (call-proceed-req (exchange-req exchange))))

  (define (run-handler server proc request)
    (guard (c [#t (report server c) #f])
      (let ([result (proc request)]
            [limit (options-max-stream-buffer (server-options-value server))])
        (cond
          [(response? result) result]
          [(and (stream-response? result) (stream-limit! (stream-response-stream result) limit))
           result]
          [(stream-response? result)
           (report server
             (problem 'h2o "the stream of a stream-response holds more than max-stream-buffer"
               (request-path request) limit))
           (drop-answer! server result)
           #f]
          [else
           (report server
             (problem 'h2o "handler returned neither a response nor a stream-response"
               (request-path request) result))
           #f]))))

  (define internal-error
    (response 500 '(("content-type" . "text/plain")) "Internal Server Error\n"))

  (define service-unavailable
    (response 503 '(("content-type" . "text/plain")) "Service Unavailable\n"))

  ;; H2O answers a request itself when its body is malformed or too large,
  ;; or cannot be buffered (400, 413, 502), and frees it only once that
  ;; answer is written; in between, res.status is set, and a second answer
  ;; would fail an assertion in H2O.
  (define (answerable? exchange)
    (and (exchange-live? exchange)
         (not (exchange-answered? exchange))
         (fx= 0 (foreign-ref 'int (exchange-req exchange) req-res-status))))

  (define (answer! server exchange result)
    (cond
      [(answerable? exchange)
       (exchange-answered?-set! exchange #t)
       (cond
         [(stream-response? result) (start-stream! server exchange result)]
         [else (send-response! (exchange-req exchange) (or result internal-error))])]
      [else (drop-answer! server result)]))

  (define (drop-answer! server result)
    (when (stream-response? result)
      (let ([stream (stream-response-stream result)])
        (stream-ended! stream)
        (notify-close server stream))))

  (define (reason-phrase status)
    (case status
      [(103) "Early Hints"]
      [(200) "OK"] [(201) "Created"] [(202) "Accepted"] [(204) "No Content"]
      [(206) "Partial Content"]
      [(301) "Moved Permanently"] [(302) "Found"] [(303) "See Other"]
      [(304) "Not Modified"] [(307) "Temporary Redirect"] [(308) "Permanent Redirect"]
      [(400) "Bad Request"] [(401) "Unauthorized"] [(403) "Forbidden"]
      [(404) "Not Found"] [(405) "Method Not Allowed"] [(408) "Request Timeout"]
      [(409) "Conflict"] [(410) "Gone"] [(411) "Length Required"]
      [(413) "Content Too Large"] [(415) "Unsupported Media Type"]
      [(421) "Misdirected Request"] [(422) "Unprocessable Content"]
      [(429) "Too Many Requests"]
      [(500) "Internal Server Error"] [(501) "Not Implemented"] [(502) "Bad Gateway"]
      [(503) "Service Unavailable"] [(504) "Gateway Timeout"]
      [else "Unknown"]))

  (define (c-bytes s)
    (let* ([bv (string->utf8 s)] [c (make-bytevector (fx+ (bytevector-length bv) 1) 0)])
      (bytevector-copy! bv 0 c 0 (bytevector-length bv))
      c))

  (define (add-headers! req headers)
    (let ([pool (+ req req-pool)])
      (for-each
        (lambda (h)
          (let ([name (string->utf8 (string-downcase (car h)))] [value (string->utf8 (cdr h))])
            (h2o-add-header-by-str pool (+ req req-res-headers)
              (pool-copy pool name) (bytevector-length name) 1 0
              (pool-copy pool value) (bytevector-length value))))
        headers)))

  ;; H2O's compress filter judges a content type only by the types its
  ;; file-extension map knows, which lack text/event-stream; giving the
  ;; response its attributes applies H2O's own rule (text/*, JSON, XML)
  ;; to every type.
  (define (set-mime-attributes! req headers)
    (let ([content-type (assp (lambda (name) (string-ci=? name "content-type")) headers)])
      (when content-type
        (let ([attributes (h2o-mem-alloc-pool (+ req req-pool) mime-attributes-size)])
          (h2o-mimemap-get-default-attributes (cdr content-type) attributes)
          (foreign-set! 'uptr req req-res-mime-attr attributes)))))

  ;; H2O keeps pointers to the reason and header strings until the response
  ;; is sent, so they are copied into the request's pool.
  (define (set-response-head! req status headers)
    (foreign-set! 'int req req-res-status status)
    (foreign-set! 'uptr req req-res-reason
      (pool-copy (+ req req-pool) (c-bytes (reason-phrase status))))
    (add-headers! req headers))

  (define (send-response! req r)
    (let ([status (response-status r)] [bv (response-body r)])
      (set-response-head! req status (response-headers r))
      (set-mime-attributes! req (response-headers r))
      (unless (bodiless-status? status)
        (foreign-set! 'size_t req req-res-content-length (bytevector-length bv)))
      (h2o-send-inline req bv (bytevector-length bv))))

  (define (start-stream! server exchange r)
    (let ([core (server-core server)])
      (let ([req (exchange-req exchange)] [stream (stream-response-stream r)])
        (set-response-head! req (stream-response-status r) (stream-response-headers r))
        (set-mime-attributes! req (stream-response-headers r))
        (hashtable-set! (core-streams core) req stream)
        (h2o-start-response req (core-generator core))
        (let-values ([(action parts)
                      (stream-start! stream
                        (lambda ()
                          (post! server
                            (lambda ()
                              (let-values ([(action parts) (stream-pump! stream)])
                                (perform! server req stream action parts))))))])
          (perform! server req stream action parts))
        (unless (eq? (server-state server) 'running) (stream-close! stream)))))

  (define (perform! server req stream action parts)
    (case action
      [(send) (send-parts! req parts)]
      [(final abort)
       (hashtable-delete! (core-streams (server-core server)) req)
       (h2o-send req 0 0 (if (eq? action 'final) send-state-final send-state-error))
       (notify-close server stream)]
      [else (void)]))

  (define (send-parts! req parts)
    (if (null? parts)
        (h2o-send req 0 0 send-state-in-progress)
        (let* ([n (length parts)] [iov (foreign-alloc (fx* n iovec-size))])
          (let loop ([parts parts] [p iov])
            (unless (null? parts)
              (foreign-set! 'uptr p iovec-base (object->reference-address (car parts)))
              (foreign-set! 'size_t p iovec-len (bytevector-length (car parts)))
              (loop (cdr parts) (+ p iovec-size))))
          (h2o-send req iov n send-state-in-progress)
          (foreign-free iov))))

  (define (notify-close server stream)
    (let ([on-close (stream-on-close stream)])
      (when on-close
        (guard (c [#t (report server c)]) (on-close)))))

  (define (send-early-hints! request headers)
    (unless (request? request)
      (assertion-violationf 'send-early-hints! "~s is not a request" request))
    (check-headers 'send-early-hints! headers)
    (let* ([exchange (request-exchange request)]
           [send! (lambda ()
                    (when (answerable? exchange)
                      (let ([req (exchange-req exchange)])
                        (set-response-head! req 103 headers)
                        (h2o-send-informational req))))]
           [server (exchange-server exchange)])
      (if (on-loop-thread? server) (send!) (post! server send!)))))
