#!/bin/bash

iptables -F
iptables -t nat -F
iptables -P INPUT ACCEPT
iptables -P OUTPUT ACCEPT
iptables -P FORWARD DROP

iptables -t nat -A POSTROUTING -o enp3s0 -j SNAT --to-source 192.168.0.100
iptables -A FORWARD -i enp3s0 -o enp3s0 -s 10.146.17.0/24 -j ACCEPT
iptables -A FORWARD -i enp3s0 -o enp3s0 -m state --state ESTABLISHED,RELATED -j ACCEPT

echo 1 > /proc/sys/net/ipv4/ip_forward
