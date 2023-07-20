#!/bin/bash

non_interactive="DEBIAN_FRONTEND=noninteractive"

sudo $non_interactive apt-get install iptables -y

# Enable non interactive mode
echo iptables-persistent iptables-persistent/autosave_v4 boolean true | sudo debconf-set-selections
echo iptables-persistent iptables-persistent/autosave_v6 boolean true | sudo debconf-set-selections

sudo apt-get install iptables-persistent -y

# Allow DNS
sudo iptables -A OUTPUT -p udp --dport 53 -j ACCEPT
sudo iptables -A INPUT -p udp --sport 53 -j ACCEPT
# Allow HTTP
sudo iptables -A INPUT -p tcp --dport 80 -m state --state NEW,ESTABLISHED -j ACCEPT
sudo iptables -A OUTPUT -p tcp --sport 80 -m state --state ESTABLISHED -j ACCEPT
# Allow HTTPS
sudo iptables -A INPUT -p tcp --dport 443 -m state --state NEW,ESTABLISHED -j ACCEPT
sudo iptables -A OUTPUT -p tcp --sport 443 -m state --state ESTABLISHED -j ACCEPT
# Allow NTP
sudo iptables -A INPUT -p udp -m udp --sport 123 -j ACCEPT
sudo iptables -A OUTPUT -p udp -m udp --dport 123 -j ACCEPT
# Allow localhost
sudo iptables -A INPUT -i lo -j ACCEPT
sudo iptables -A OUTPUT -o lo -j ACCEPT

# Save iptables
sudo iptables-save | sudo tee /etc/iptables/rules.v4 > /dev/null
