import socket
import sys
import time

port, mode, size = int(sys.argv[1]), sys.argv[2], int(sys.argv[3])
host = f"127.0.0.1:{port}"
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
if mode == "stuck":
    s.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
s.connect(("127.0.0.1", port))
if mode == "push":
    s.sendall(f"POST /lazy HTTP/1.1\r\nHost: {host}\r\nContent-Length: {size}\r\n\r\n".encode())
    chunk = b"x" * 65536
    sent, early = 0, None
    start = time.monotonic()
    s.settimeout(0.05)
    while sent < size:
        if early is None and time.monotonic() - start >= 0.5:
            early = sent
        try:
            sent += s.send(chunk[: min(len(chunk), size - sent)])
        except socket.timeout:
            pass
    if early is None:
        early = sent
    s.settimeout(10)
    answer = s.recv(65536).split(b"\r\n", 1)[0].decode("latin1")
    print(f"{early} {sent} {answer}")
elif mode == "abort":
    s.sendall(f"POST /abort HTTP/1.1\r\nHost: {host}\r\nContent-Length: 1000000\r\n\r\n".encode())
    s.sendall(b"x" * size)
    time.sleep(0.2)
    s.close()
    print("closed")
elif mode == "bad-chunk":
    s.close()
    answers = {}
    for i in range(size):
        c = socket.create_connection(("127.0.0.1", port))
        c.sendall(f"POST /first-read HTTP/1.1\r\nHost: {host}\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n".encode())
        time.sleep(0.0045 + (i % 40) / 40000)
        try:
            c.sendall(b"zz\r\n")
            c.settimeout(2.0)
            status = c.recv(65536).split(b"\r\n", 1)[0].decode("latin1") or "closed"
        except OSError:
            status = "reset"
        c.close()
        answers[status] = answers.get(status, 0) + 1
    print(" | ".join(f"{k}: {v}" for k, v in sorted(answers.items())))
elif mode == "stuck":
    s.sendall(f"GET /stuck HTTP/1.1\r\nHost: {host}\r\n\r\n".encode())
    time.sleep(size)
    s.close()
    print("closed")
