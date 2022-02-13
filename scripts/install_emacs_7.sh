#!/bin/bash

non_interactive="DEBIAN_FRONTEND=noninteractive"

# Install elpy requirements
sudo $non_interactive apt-get install virtualenv black flake8 pylint mypy -y

# Install emacs
sudo $non_interactive apt-get install emacs -y

sudo pip install jedi 

cp ./emacs/.emacs ~/
