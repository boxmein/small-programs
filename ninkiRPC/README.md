ninkiRPC - A program implementing the worst possible RPC API

## Usage

1. Implement functions into ninkiRPC.cpp
2. Log the function addresses out in `int main`
3. Compile the code
4. Run the resulting binary
5. Start sending UDP packets to the server to call functions

## RPC API format

The RPC API is designed to be really insecure.

It's based on receiving UDP packets from clients and then making stuff happen.

The UDP packet's payload is a binary blob with the following shape:

    BE 00 00 00 00 00 00 00 00 EB

BE marks the start of the payload.

EB marks the end of the payload.

There can be 4 or 8 bytes inbetween the markers, designating a memory address

to call like a C function.


## Example RPC client call

This command uses `nping` provided by nmap to remotely call the 0000000011223344 procedure.

    nping --udp --data be0000000011223344eb localhost -p 11212

