import socket
import sys

port, path = int(sys.argv[1]), sys.argv[2]
data = open(path, "rb").read()
s = socket.create_connection(("127.0.0.1", port))
s.settimeout(2.0)
out = b""
end = "closed"
try:
    s.sendall(data)
    while True:
        b = s.recv(65536)
        if not b:
            break
        out += b
        s.settimeout(0.2)
except socket.timeout:
    end = "open"
except (ConnectionResetError, BrokenPipeError):
    end = "reset"
s.close()
if not out:
    print(f"no answer, {end}")
else:
    status = out.split(b"\r\n", 1)[0].decode("latin1")
    later = [line.split(b" ")[1].decode("latin1")
             for line in out.split(b"\r\n")[1:] if line.startswith(b"HTTP/1.")]
    print(status + "".join(f", then {code}" for code in later))
