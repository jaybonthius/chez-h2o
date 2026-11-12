# chez-h2o

An HTTP server library for threaded Chez Scheme 10.4.1, built on the H2O HTTP server library. H2O does the protocol work in C (HTTP/1.1, HTTP/2 over TLS, static files, compression) on one event-loop thread; handlers are Scheme procedures that run on worker threads or on the loop thread and return response values, and server-sent event streams can be fed from any thread. The library also reads URL-encoded and multipart forms, cookies and HTTP dates.

```scheme
(import (chezscheme) (h2o))

(define clients-mutex (make-mutex))
(define clients '())

(define (home request)
  (response 200 '(("content-type" . "text/html; charset=utf-8"))
    "<form method=post action=/click><input name=note><button>Send</button></form>"))

(define (click request)
  (let ([event (format "data: ~a\n\n" (cdr (assoc "note" (request-form request))))])
    (for-each (lambda (stream) (stream-send! stream event))
      (with-mutex clients-mutex clients)))
  (response 303 '(("location" . "/")) ""))

(define (events request)
  (letrec ([stream (make-stream
                     (lambda ()
                       (with-mutex clients-mutex (set! clients (remq stream clients)))))])
    (with-mutex clients-mutex (set! clients (cons stream clients)))
    (stream-response 200 '(("content-type" . "text/event-stream")) stream)))

(define server
  (start-server
    (list (http-listener "127.0.0.1" 8080))
    (list (make-host '("127.0.0.1:8080" "localhost:8080")
            (list (route "/" (files "public") (handler home))
                  (route "/click" (handler click))
                  (route "/events" (loop-handler events) (compress)))))
    (server-options [workers 8] [strict-hosts #t])))
```

`files` serves `public/` with its precompressed `.br`, `.zst` and `.gz` variants and passes a missing file on to `home`. Each handler returns a `response` or a `stream-response`; a stream can be sent to from any thread, and belongs to one response. `(server-stop! server)`, from any thread that is not one of the server's, drains the server and joins its threads.

## Building

The build needs threaded Chez Scheme 10.4.1 and, from the system: a C compiler, cmake, pkg-config, and the development files of OpenSSL, zlib, brotli and zstd. H2O is a git submodule, so clone with `--recursive`.

```sh
git clone --recursive <this repository>
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

`CAP` prefixes every build, test and benchmark process, for example `CAP="systemd-run --user --scope -q -p MemoryMax=8G"`. The tests need `curl`, `openssl` and `python3`.

Programs import `(h2o)` with `--libdirs /path/to/chez-h2o/build/obj/src`. The compiled library loads H2O from this checkout's `build/h2o`, so moving the checkout needs a rebuild.

## API

| Form | What it does |
|---|---|
| `(start-server listeners hosts [options])`, `(server-stop! server)` | start a server; drain it, wait for running handlers up to `shutdown-timeout`, and join its threads |
| `(server-options [name value] ...)` | `workers`, `strict-hosts`, `on-error`, `shutdown-timeout`, `max-request-body-size`, `max-stream-buffer`, and H2O's timeouts and HTTP/2 limits; an unknown name fails to compile |
| `(http-listener address port)`, `(https-listener address port certificate key)` | where to listen |
| `(make-host names routes)`, `(route path item ...)` | hosts and path prefixes |
| `(handler proc)`, `(streaming-handler proc)`, `(loop-handler proc)` | a procedure on a worker with the whole body, on a worker reading the body from a port, or on the loop thread |
| `(files directory)`, `(compress [encodings [qualities]])`, `(max-age seconds)` | static files with precompressed variants; gzip, br and zstd; `Cache-Control` |
| `(encodings name ...)`, `(qualities [name quality] ...)` | which of `zstd`, `br` and `gzip` to use, in order of preference: a handler's response is compressed with the first one the client accepts, and `(compress)` means `(encodings zstd br gzip)`; a quality per encoding (zstd 1–22, default 3; br 0–11, default 1; gzip 1–9, default 1), for example `(compress (encodings zstd br gzip) (qualities [br 5]))` |
| `request-method`, `request-path`, `request-query`, `request-header`, `request-body`, `request-body-port`, `request-cookies`, `request-form`, `request-uploads`, … | what a handler receives |
| `(response status headers body)`, `(stream-response status headers stream)`, `(send-early-hints! request headers)` | what a handler returns, and 103 responses before it |
| `(make-stream [on-close])`, `stream-send!`, `stream-close!`, `stream-open?` | event streams, written from any thread |
| `set-cookie-header`, `time->http-date`, `http-date->time`, `query->alist`, `percent-encode`, … | helpers that need no server |

An `Accept-Encoding` element counts only as a bare name, as H2O reads it: `br;q=0` is not accepted, and neither is `br;q=0.5`; `*` is ignored. Files served by `files` keep H2O's own order, br, then zstd, then gzip, for precompressed variants and for compression as they are sent.

H2O accepts some framing that RFC 9112 forbids (two `Host` or `Content-Length` headers, bare LF line ends); run it behind a front end that normalises them where that matters.
