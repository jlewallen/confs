#!/bin/bash

iptables -F

# iptables -P INPUT DROP
# iptables -P OUTPUT DROP
# iptables -P FORWARD DROP

iptables -A OUTPUT -o lo -j ACCEPT
iptables -A OUTPUT -o eth0 -m owner --uid-owner jnetwork -j ACCEPT
iptables -A OUTPUT -j DROP
