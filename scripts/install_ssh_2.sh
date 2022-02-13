#!/bin/bash

non_interactive="DEBIAN_FRONTEND=noninteractive"

sudo $non_interactive apt-get install openssh-server -y

# Disable root login
sudo echo "PermitRootLogin no" | sudo tee -a /etc/ssh/sshd_config > /dev/null

# Allow keys authentication 
sudo echo "PubkeyAuthentication yes" | sudo tee -a /etc/ssh/sshd_config > /dev/null

# Drop incoming packages
sudo iptables -P INPUT DROP

# Allow port 22
sudo iptables -A INPUT -p tcp --dport 22 -j ACCEPT

# Save iptables
sudo iptables-save | sudo tee /etc/iptables/rules.v4 > /dev/null

