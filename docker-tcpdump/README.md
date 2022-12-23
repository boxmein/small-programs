# docker-tcpdump

Easy dump traffic inside docker networks

## Support

* Linux - yes
* macOS - no
* Windows - no

## Examples 

```shell
# specify container ID
docker-tcpdump -c fb721b73f74c -o wow.pcap
wireshark wow.pcap
```

```shell
# specify Docker Network ID 
docker-tcpdump -n e39af695826f -o wow.pcap
wireshark wow.pcap
```

```shell
# specify any Linux network interface 
docker-tcpdump -i eth0 -o wow.pcap
docker-tcpdump -i docker0 -o wow.pcap
wireshark wow.pcap
```

```shell
# specify libpcap filter
docker-tcpdump -c fb721b73f74c -f 'dst port 22' -o wow.pcap
wireshark wow.pcap
```

## Usage

```shell
Usage: docker-tcpdump [OPTIONS] --output-path <OUTPUT_PATH>

Options:
  -o, --output-path <OUTPUT_PATH>      File to write results to
  -n, --networks <NETWORKS>            Which Docker networks to capture
  -c, --container-ids <CONTAINER_IDS>  Capture all traffic for the given containers
  -i, --interface <INTERFACE>          Which interface to capture
  -f, --filter <FILTER>                Set a packet filter on the capture. Examples: "tcp port 20" Reference: https://www.tcpdump.org/manpages/pcap-filter.7.html
  -d, --docker-host <DOCKER_HOST>      [default: unix:///var/run/docker.sock]
  -h, --help                           Print help information
  -V, --version                        Print version information
```
