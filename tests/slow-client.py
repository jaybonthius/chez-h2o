import socket
import sys
import time

port, mode, limit = int(sys.argv[1]), sys.argv[2], float(sys.argv[3])
s = socket.create_connection(("127.0.0.1", port))
host = f"127.0.0.1:{port}"
if mode == "body":
    s.sendall(f"POST /slow HTTP/1.1\r\nHost: {host}\r\nContent-Length: 100\r\n\r\n".encode())
elif mode == "head":
    s.sendall(f"GET /hello HTTP/1.1\r\nHost: {host}\r\nX-Slow: ".encode())
else:
    s.sendall(f"GET /hello HTTP/1.1\r\nHost: {host}\r\n\r\n".encode())
s.settimeout(0.2)
start = time.monotonic()
end = "open"
while time.monotonic() - start < limit:
    try:
        if mode != "idle":
            s.sendall(b"a")
        if s.recv(65536) == b"":
            end = "closed"
            break
    except socket.timeout:
        pass
    except OSError:
        end = "closed"
        break
print(f"{end} after {time.monotonic() - start:.1f} s")
