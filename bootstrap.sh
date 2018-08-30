#!/bin/bash

set -x

apt-get update
apt-get install -y puppet git
su -c 'git clone https://github.com/frrad/dotfiles.git $HOME/dotfiles' $SUDO_USER
