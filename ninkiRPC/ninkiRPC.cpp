// A program implementing the worst possible RPC API
//
// The RPC API looks like this:
//
// BE (address) EB
//
// The address is the memory address of the function to call.
// The function must not take any arguments nor return anything.
//
// To initiate a remote procedure call:
//
// Send an UDP datagram to localhost:11221 with a payload that looks
// like the above structure (in binary that is)
//
// A nping command example is shown below. Note that nping accepts
// binary data in a hex-encoded format.
//
// ----
// Compile:
//   clang++ -o rpcmeme rpcmeme.cpp
// ----
// Run:
//   ./rpcmeme
// ----
// Send a RPC using nping:
//   nping --udp --data be0000000011223344eb localhost -p 11212
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>

union addrunion_t {
  uint64_t addr;
  void (*fn)();
};

void print_packet(uint8_t buffer[], int buffer_size) {
  // A received packet is:
  // Start marker: BE
  // 4-byte or 8-byte network order address
  // End marker: EB
  // 1: address (4 bytes, network byte order)
  if (buffer_size != 6 && buffer_size != 10) {
    std::cout << "Received invalid buffer, not 6 or 10 bytes" << std::endl;
    std::cout << "(debug: received " << buffer_size << " octets)" << std::endl;
    return;
  }

  if (buffer[0] != 0xBE) {
    std::cout << "Buffer start mark wrong" << std::endl;
    return;
  }
  if (buffer[buffer_size - 1] != 0xEB) {
    std::cout << "Buffer end mark wrong" << std::endl;
    return;
  }
  
  if (buffer_size == 6) {

    std::cout << "32-bit address" << std::endl;
    // declare addr as pointer to function returning void
    uint64_t addr = ((buffer[1] & 0xff) << 24) |
                    ((buffer[2] & 0xff) << 16) |
                    ((buffer[3] & 0xff) << 8) |
                    (buffer[4]  & 0xff);
    std::cout << "Address: " << addr << std::endl;
    addrunion_t x = { .addr = addr };
    x.fn();
  } else if (buffer_size == 10) {
    std::cout << "64-bit address" << std::endl;
    uint64_t addr =  ((buffer[1] & 0xff) << 56) |
                     ((buffer[2] & 0xff) << 48) |
                     ((buffer[3] & 0xff) << 40) |
                     ((buffer[4] & 0xff) << 32) |
                     ((buffer[5] & 0xff) << 24) |
                     ((buffer[6] & 0xff) << 16) |
                     ((buffer[7] & 0xff) << 8) |
                      (buffer[8] & 0xff);
    std::cout << "Address: " << addr << std::endl;
    addrunion_t x = { .addr = addr };
    x.fn();
  }
}

void print_memes() {
  std::cout << "Memes" << std::endl;
}


void start_server_and_listen() {
  struct sockaddr_in servaddr, cliaddr;
  uint8_t buffer[10]; // NOTE: expecting 10 bytes not more
  
  std::cout << "starting a socket" << std::endl;

  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);

  if (sockfd < 0) {
    throw std::runtime_error("Socket cannot be created!");
  }
  
  // Zero memory around server & client address
  memset(&servaddr, 0, sizeof(servaddr));
  memset(&cliaddr, 0, sizeof(cliaddr));

  // program the socket
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;
  servaddr.sin_port = htons(11212);

  std::cout << "binding the socket" << std::endl;

  // bind the socket
  int res = bind(sockfd,  (const struct sockaddr*) &servaddr, sizeof(servaddr));
  
  if (res < 0) {
    throw std::runtime_error("Binding failed");
  }

  std::cout << "listening for data on UDP 11212" << std::endl;
  // n: number of bytes received from network, or -1 if an error, 0 if shutdown
  ssize_t n; 
  // s
  socklen_t len; 
  
  while (true) {
    memset(buffer, 0, sizeof(buffer));
    n = recvfrom(
        sockfd,
        buffer, 
        10, // NOTE: expecting 10 bytes not more
        MSG_WAITALL,
        (struct sockaddr*) &cliaddr,
        &len);

    if (n < 1) {
      std::cout << "received " << n << " return value, which means stopping" << std::endl;
      break;
    }

    std::cout << "received " << n << " octets from network" << std::endl;

    print_packet(buffer, n);
  }
}

int main() {
  // Print out some cool functions
  
  std::cout << "print_memes is on address: " << (void *) &print_memes << std::endl;

  // Start listening
  start_server_and_listen();
}

