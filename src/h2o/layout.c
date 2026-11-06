#include <arpa/inet.h>
#include <netinet/in.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <openssl/ssl.h>
#include "h2o.h"
#include "h2o/multithread.h"
#include "h2o/socket/evloop.h"

#define SIZE(name, type) printf("(define %s %zu)\n", name, sizeof(type))
#define CONSTANT(name, value) printf("(define %s %lld)\n", name, (long long)(value))
/* The binding reads and writes each field with a fixed width; the assertion
   stops the build when H2O changes a field's width. */
#define FIELD(name, type, member, width)                                                                                          \
    do {                                                                                                                           \
        _Static_assert(sizeof(((type *)0)->member) == (width), name);                                                              \
        printf("(define %s %zu)\n", name, offsetof(type, member));                                                                 \
    } while (0)

/* offsetof cannot name a bit-field, so find its byte and mask by setting it. */
static void handler_streaming_bit(void)
{
    h2o_handler_t handler;
    memset(&handler, 0, sizeof(handler));
    handler.supports_request_streaming = 1;
    const unsigned char *bytes = (const unsigned char *)&handler;
    for (size_t i = 0; i != sizeof(handler); ++i) {
        if (bytes[i] != 0) {
            printf("(define handler-supports-request-streaming %zu)\n", i);
            printf("(define handler-supports-request-streaming-mask %u)\n", bytes[i]);
            return;
        }
    }
}

int main(void)
{
    printf("(define h2o-shared-object \"%s\")\n", H2O_SHARED_OBJECT);

    SIZE("iovec-size", h2o_iovec_t);
    FIELD("iovec-base", h2o_iovec_t, base, 8);
    FIELD("iovec-len", h2o_iovec_t, len, 8);

    SIZE("globalconf-size", h2o_globalconf_t);
    FIELD("globalconf-hosts", h2o_globalconf_t, hosts, 8);
    FIELD("globalconf-max-request-entity-size", h2o_globalconf_t, max_request_entity_size, 8);
    FIELD("globalconf-handshake-timeout", h2o_globalconf_t, handshake_timeout, 8);
    FIELD("globalconf-http1-req-timeout", h2o_globalconf_t, http1.req_timeout, 8);
    FIELD("globalconf-http1-req-io-timeout", h2o_globalconf_t, http1.req_io_timeout, 8);
    FIELD("globalconf-http1-upgrade-to-http2", h2o_globalconf_t, http1.upgrade_to_http2, 4);
    FIELD("globalconf-http2-idle-timeout", h2o_globalconf_t, http2.idle_timeout, 8);
    FIELD("globalconf-http2-graceful-shutdown-timeout", h2o_globalconf_t, http2.graceful_shutdown_timeout, 8);
    FIELD("globalconf-http2-max-streams", h2o_globalconf_t, http2.max_streams, 4);
    FIELD("globalconf-http2-max-concurrent-requests", h2o_globalconf_t, http2.max_concurrent_requests_per_connection, 8);
    FIELD("globalconf-http2-max-concurrent-streaming-requests", h2o_globalconf_t,
          http2.max_concurrent_streaming_requests_per_connection, 8);
    FIELD("globalconf-send-informational-mode", h2o_globalconf_t, send_informational_mode, 4);

    FIELD("hostconf-strict-match", h2o_hostconf_t, strict_match, 1);

    SIZE("context-size", h2o_context_t);

    SIZE("accept-ctx-size", h2o_accept_ctx_t);
    FIELD("accept-ctx-ctx", h2o_accept_ctx_t, ctx, 8);
    FIELD("accept-ctx-hosts", h2o_accept_ctx_t, hosts, 8);
    FIELD("accept-ctx-ssl-ctx", h2o_accept_ctx_t, ssl_ctx, 8);

    SIZE("handler-size", h2o_handler_t);
    FIELD("handler-on-req", h2o_handler_t, on_req, 8);
    handler_streaming_bit();

    SIZE("generator-size", h2o_generator_t);
    FIELD("generator-proceed", h2o_generator_t, proceed, 8);
    FIELD("generator-stop", h2o_generator_t, stop, 8);

    FIELD("req-conn", h2o_req_t, conn, 8);
    FIELD("req-scheme", h2o_req_t, scheme, 8);
    FIELD("req-authority", h2o_req_t, authority, 16);
    FIELD("req-method", h2o_req_t, method, 16);
    FIELD("req-path", h2o_req_t, path, 16);
    FIELD("req-query-at", h2o_req_t, query_at, 8);
    FIELD("req-path-normalized", h2o_req_t, path_normalized, 16);
    FIELD("req-version", h2o_req_t, version, 4);
    FIELD("req-headers", h2o_req_t, headers, 24);
    FIELD("req-entity", h2o_req_t, entity, 16);
    FIELD("req-res-status", h2o_req_t, res.status, 4);
    FIELD("req-res-reason", h2o_req_t, res.reason, 8);
    FIELD("req-res-content-length", h2o_req_t, res.content_length, 8);
    FIELD("req-res-headers", h2o_req_t, res.headers, 24);
    FIELD("req-res-mime-attr", h2o_req_t, res.mime_attr, 8);
    SIZE("mime-attributes-size", h2o_mime_attributes_t);
    FIELD("req-write-req-cb", h2o_req_t, write_req.cb, 8);
    FIELD("req-write-req-ctx", h2o_req_t, write_req.ctx, 8);
    FIELD("req-proceed-req", h2o_req_t, proceed_req, 8);
    printf("(define req-pool %zu)\n", offsetof(h2o_req_t, pool));

    FIELD("conn-callbacks", h2o_conn_t, callbacks, 8);
    FIELD("conn-callbacks-get-peername", h2o_conn_callbacks_t, get_peername, 8);
    FIELD("url-scheme-name", h2o_url_scheme_t, name, 16);

    SIZE("compress-args-size", h2o_compress_args_t);
    FIELD("compress-args-min-size", h2o_compress_args_t, min_size, 8);
    FIELD("compress-args-gzip-quality", h2o_compress_args_t, gzip.quality, 4);
    FIELD("compress-args-brotli-quality", h2o_compress_args_t, brotli.quality, 4);
    FIELD("compress-args-zstd-quality", h2o_compress_args_t, zstd.quality, 4);
    SIZE("expires-args-size", h2o_expires_args_t);
    FIELD("expires-args-mode", h2o_expires_args_t, mode, 4);
    FIELD("expires-args-max-age", h2o_expires_args_t, data.max_age, 8);

    FIELD("vector-entries", h2o_vector_t, entries, 8);
    FIELD("vector-size", h2o_vector_t, size, 8);

    SIZE("header-size", h2o_header_t);
    FIELD("header-name", h2o_header_t, name, 8);
    FIELD("header-value", h2o_header_t, value, 16);

    FIELD("socket-on-close-cb", h2o_socket_t, on_close.cb, 8);
    FIELD("socket-on-close-data", h2o_socket_t, on_close.data, 8);

    SIZE("receiver-size", h2o_multithread_receiver_t);
    SIZE("message-size", h2o_multithread_message_t);
    FIELD("linklist-next", h2o_linklist_t, next, 8);
    FIELD("linklist-prev", h2o_linklist_t, prev, 8);

    CONSTANT("send-state-in-progress", H2O_SEND_STATE_IN_PROGRESS);
    CONSTANT("send-state-final", H2O_SEND_STATE_FINAL);
    CONSTANT("send-state-error", H2O_SEND_STATE_ERROR);
    CONSTANT("send-informational-mode-all", H2O_SEND_INFORMATIONAL_MODE_ALL);
    CONSTANT("expires-mode-max-age", H2O_EXPIRES_MODE_MAX_AGE);
    CONSTANT("file-flag-send-compressed", H2O_FILE_FLAG_SEND_COMPRESSED);
    CONSTANT("socket-flag-dont-read", H2O_SOCKET_FLAG_DONT_READ);
    CONSTANT("somaxconn", H2O_SOMAXCONN);
    printf("(define size-max %zu)\n", (size_t)SIZE_MAX);

    CONSTANT("af-inet", AF_INET);
    CONSTANT("af-inet6", AF_INET6);
    CONSTANT("sock-stream", SOCK_STREAM);
    CONSTANT("sock-cloexec", SOCK_CLOEXEC);
    CONSTANT("sol-socket", SOL_SOCKET);
    CONSTANT("so-reuseaddr", SO_REUSEADDR);
    CONSTANT("ipproto-ipv6", IPPROTO_IPV6);
    CONSTANT("ipv6-v6only", IPV6_V6ONLY);
    CONSTANT("shut-rdwr", SHUT_RDWR);
    SIZE("sockaddr-storage-size", struct sockaddr_storage);
    FIELD("sockaddr-family", struct sockaddr, sa_family, 2);
    CONSTANT("inet6-addrstrlen", INET6_ADDRSTRLEN);
    SIZE("sockaddr-in-size", struct sockaddr_in);
    FIELD("sockaddr-in-family", struct sockaddr_in, sin_family, 2);
    FIELD("sockaddr-in-port", struct sockaddr_in, sin_port, 2);
    FIELD("sockaddr-in-addr", struct sockaddr_in, sin_addr, 4);
    SIZE("sockaddr-in6-size", struct sockaddr_in6);
    FIELD("sockaddr-in6-family", struct sockaddr_in6, sin6_family, 2);
    FIELD("sockaddr-in6-port", struct sockaddr_in6, sin6_port, 2);
    FIELD("sockaddr-in6-addr", struct sockaddr_in6, sin6_addr, 16);

    CONSTANT("ssl-filetype-pem", SSL_FILETYPE_PEM);
    return 0;
}
