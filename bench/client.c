#define _GNU_SOURCE
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

static long long now_ns(void) {
  struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}
static int port;
static int dial(void) {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in a = {0}; a.sin_family = AF_INET; a.sin_port = htons(port);
  a.sin_addr.s_addr = htonl(0x7f000001);
  if (connect(fd, (void *)&a, sizeof a) < 0) { perror("connect"); exit(1); }
  int one = 1; setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
  return fd;
}
static void send_str(int fd, const char *s) {
  size_t n = strlen(s);
  while (n) { ssize_t w = write(fd, s, n); if (w <= 0) { perror("write"); exit(1); } s += w; n -= w; }
}
static int read_head(int fd, char *buf, int cap, int *len) {
  for (;;) {
    char *e = memmem(buf, *len, "\r\n\r\n", 4);
    if (e) return (int)(e - buf) + 4;
    ssize_t r = read(fd, buf + *len, cap - *len);
    if (r <= 0) return -1;
    *len += r;
  }
}
static int cmp(const void *a, const void *b) {
  long long x = *(long long *)a, y = *(long long *)b; return x < y ? -1 : x > y;
}
static void report(const char *what, long long *v, int n) {
  qsort(v, n, sizeof *v, cmp);
  double sum = 0; for (int i = 0; i < n; i++) sum += v[i];
  printf("%s n=%d p50=%.1fus p99=%.1fus max=%.1fus mean=%.1fus\n", what, n,
         v[n / 2] / 1e3, v[(int)(n * 0.99)] / 1e3, v[n - 1] / 1e3, sum / n / 1e3);
}
static char host[64];

static void rtt(int n) {
  int fd = dial();
  char req[256], buf[4096];
  snprintf(req, sizeof req, "POST /commands HTTP/1.1\r\nHost: %s\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n\r\nkey ctrl-x 42", host);
  long long *v = malloc(sizeof *v * n);
  for (int i = 0; i < n; i++) {
    long long t = now_ns();
    send_str(fd, req);
    int len = 0, h = read_head(fd, buf, sizeof buf, &len);
    if (h < 0 || strncmp(buf, "HTTP/1.1 204", 12)) { fprintf(stderr, "bad response at %d: %.*s\n", i, len, buf); exit(1); }
    v[i] = now_ns() - t;
  }
  report("rtt", v, n);
  close(fd);
}

static void post(const char *body) {
  int fd = dial(); char req[512], buf[4096]; int len = 0;
  snprintf(req, sizeof req, "POST /commands HTTP/1.1\r\nHost: %s\r\nContent-Length: %zu\r\nConnection: close\r\n\r\n%s", host, strlen(body), body);
  send_str(fd, req); read_head(fd, buf, sizeof buf, &len); close(fd);
}

static void sse(int streams, int n, int size, int ms) {
  int fds[256]; char *bufs[256]; int lens[256]; int counts[256];
  int cap = 1 << 20;
  char req[256];
  snprintf(req, sizeof req, "GET /events HTTP/1.1\r\nHost: %s\r\nAccept: text/event-stream\r\n\r\n", host);
  for (int i = 0; i < streams; i++) {
    fds[i] = dial(); bufs[i] = malloc(cap); lens[i] = 0; counts[i] = 0;
    send_str(fds[i], req);
    int h = read_head(fds[i], bufs[i], cap, &lens[i]);
    if (h < 0) { fprintf(stderr, "no SSE head\n"); exit(1); }
    memmove(bufs[i], bufs[i] + h, lens[i] - h); lens[i] -= h;
  }
  usleep(100000);
  char cmd[64]; snprintf(cmd, sizeof cmd, "push %d %d %d", n, size, ms);
  post(cmd);
  long long *v = malloc(sizeof *v * n * streams); int got = 0;
  struct pollfd p[256];
  long long deadline = now_ns() + (long long)(n * ms + 5000) * 1000000LL;
  while (got < n * streams && now_ns() < deadline) {
    for (int i = 0; i < streams; i++) { p[i].fd = fds[i]; p[i].events = POLLIN; }
    if (poll(p, streams, 1000) <= 0) continue;
    for (int i = 0; i < streams; i++) {
      if (!(p[i].revents & POLLIN)) continue;
      ssize_t r = read(fds[i], bufs[i] + lens[i], cap - lens[i]);
      if (r <= 0) { fprintf(stderr, "stream %d closed\n", i); exit(1); }
      long long t = now_ns();
      lens[i] += r;
      for (;;) {
        char *m = memmem(bufs[i], lens[i], "data-ns=\"", 9);
        if (!m) break;
        char *end = memmem(m, lens[i] - (m - bufs[i]), "</div>", 6);
        if (!end) break;
        v[got++] = t - atoll(m + 9); counts[i]++;
        int used = (int)(end + 6 - bufs[i]);
        memmove(bufs[i], bufs[i] + used, lens[i] - used); lens[i] -= used;
      }
    }
  }
  char what[64]; snprintf(what, sizeof what, "sse streams=%d size=%d", streams, size);
  report(what, v, got);
  if (got < n * streams) printf("missing %d events (newest-wins drops or loss)\n", n * streams - got);
  if (getenv("HOLD")) { fflush(stdout); sleep(atoi(getenv("HOLD"))); }
  for (int i = 0; i < streams; i++) close(fds[i]);
}

static long long stat_field(int k) {
  int fd = dial(); char req[256], buf[8192]; int len = 0;
  snprintf(req, sizeof req, "GET /stats HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n", host);
  send_str(fd, req);
  for (;;) { ssize_t r = read(fd, buf + len, sizeof buf - 1 - len); if (r <= 0) break; len += r; }
  buf[len] = 0; close(fd);
  char *b = strstr(buf, "\r\n\r\n"); if (!b) return -1; b += 4;
  long long f[4] = {0}; sscanf(b, "%lld %lld %lld %lld", &f[0], &f[1], &f[2], &f[3]);
  return f[k];
}

static void disc(int rounds) {
  long long *v = malloc(sizeof *v * rounds);
  char req[256], buf[4096];
  snprintf(req, sizeof req, "GET /events HTTP/1.1\r\nHost: %s\r\n\r\n", host);
  for (int i = 0; i < rounds; i++) {
    int fd = dial(), len = 0; send_str(fd, req);
    read_head(fd, buf, sizeof buf, &len);
    usleep(50000);
    long long t = now_ns();
    close(fd);
    usleep(1500000);
    long long c = stat_field(3);
    v[i] = c > t ? c - t : -1;
  }
  report("disconnect-detect", v, rounds);
}

static void stuck(int n) {
  char req[256], buf[4096]; int len = 0;
  snprintf(req, sizeof req, "GET /events HTTP/1.1\r\nHost: %s\r\n\r\n", host);
  int fd = dial(); int small = 4096; setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &small, sizeof small);
  send_str(fd, req); read_head(fd, buf, sizeof buf, &len);
  char cmd[64]; snprintf(cmd, sizeof cmd, "push %d 32000 5", n);
  long long t0 = now_ns(); post(cmd);
  for (int i = 0; i < 600; i++) {
    usleep(100000);
    if (stat_field(2) == 0) { printf("stuck stream dropped after %.1f s\n", (now_ns() - t0) / 1e9); close(fd); return; }
  }
  printf("stuck stream still open after 60 s\n"); close(fd);
}

static void disc2(int rounds) {
  long long *v = malloc(sizeof *v * rounds); int open_left = 0;
  char req[256], buf[4096];
  snprintf(req, sizeof req, "GET /events HTTP/1.1\r\nHost: %s\r\n\r\n", host);
  for (int i = 0; i < rounds; i++) {
    long long base = stat_field(2);
    int fd = dial(), len = 0; send_str(fd, req); read_head(fd, buf, sizeof buf, &len);
    while (stat_field(2) <= base) usleep(1000);
    usleep(50000);
    long long t = now_ns(); close(fd); v[i] = -1;
    while (now_ns() - t < 15000000000LL) {
      if (stat_field(2) <= base) { v[i] = now_ns() - t; break; }
      usleep(5000);
    }
    if (v[i] < 0) { open_left++; v[i] = 15000000000LL; }
  }
  report("disconnect-detect", v, rounds);
  if (open_left) printf("%d of %d closed streams still counted after 15 s\n", open_left, rounds);
}

int main(int argc, char **argv) {
  if (argc < 3) return 2;
  port = atoi(argv[2]);
  snprintf(host, sizeof host, "127.0.0.1:%d", port);
  if (!strcmp(argv[1], "rtt")) rtt(atoi(argv[3]));
  else if (!strcmp(argv[1], "sse")) sse(atoi(argv[3]), atoi(argv[4]), atoi(argv[5]), atoi(argv[6]));
  else if (!strcmp(argv[1], "disc")) disc(atoi(argv[3]));
  else if (!strcmp(argv[1], "disc2")) disc2(atoi(argv[3]));
  else if (!strcmp(argv[1], "stuck")) stuck(atoi(argv[3]));
  else if (!strcmp(argv[1], "stat")) printf("%lld\n", stat_field(atoi(argv[3])));
  return 0;
}
