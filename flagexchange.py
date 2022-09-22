#!/usr/bin/env python3
# encoding: utf-8

import socketserver
import queue

q = queue.Queue()
q.put(b"meme")
q.put(b"wow")
q.put(b"huehue")

class Handler(socketserver.StreamRequestHandler):
    def handle(self):
        print("new connection")
        self.request.sendall("Flag to duplicate: > ".encode('utf-8'))
        received = self.rfile.readline(120).strip()
        q.put(received)
        print(self.client_address, "sent", received)
        self.request.sendall("Thanks for the flag! Here's two flags:\n".encode('utf-8'))
        if q.qsize() == 0:
            self.request.sendall(received + "\n".encode('utf-8'))
            self.request.sendall(received + "\n".encode('utf-8'))
            return

        other = q.get()
        self.request.sendall(other + "\n".encode('utf-8'))
        self.request.sendall(other + "\n".encode('utf-8'))

with socketserver.TCPServer(("0.0.0.0", 8888), Handler) as server:
    print("server started on 0.0.0.0:8888")
    server.serve_forever()
