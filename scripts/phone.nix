#!/usr/bin/env bash
set -e

[[ -n "$DEVICE" ]] || DEVICE="enp0s29f0u2"
echo "Using DEVICE '$DEVICE'" 1>&2

# "Easy" method; unreliable
#sudo ip address add 192.168.0.200/24 dev "$DEVICE"
#sudo ip link set dev "$DEVICE" up

# "Harder" method; seems to work, also forwards the Internet connection
sudo iptables -A POSTROUTING -t nat -j MASQUERADE -s 192.168.0.0/24
sudo sysctl -w net.ipv4.ip_forward=1
sudo ip addr add 192.168.0.200/24 dev "$DEVICE"

echo "Phone should now be available at 192.168.0.202" 1>&2
