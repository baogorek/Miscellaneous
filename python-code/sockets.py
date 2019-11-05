# Open two python shells
# Shell 1 will be our server, Shell 2 will be our client

##### In Shell 1 #####
import socket

server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.bind(("127.0.0.1", 5000))
server_socket.listen(5) # Max 5 connect requests in queue

# Shell 1 will seem to hang after running this line
client_socket_on_server, address = server_socket.accept()

##### In Shell 2 #####
import socket

client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client_socket.connect(("127.0.0.1", 5000))

##### In Shell 1 #####
# Which is now back in your control
client_socket_on_server.send("hey".encode()) # in Python3, default strings are unicode

##### In Shell 2 #####
client_socket.recv(100) # three characters in 'hey', 100 char buffer
