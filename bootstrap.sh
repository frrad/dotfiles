#!/bin/bash

set -x

apt-get update
apt-get install -y puppet git
target=`su -c 'echo $HOME/dotfiles' $SUDO_USER`
if [ ! -d "$target" ]; then
  su -c "git clone https://github.com/frrad/dotfiles.git $target" $SUDO_USER
fi
puppet apply $target/puppet/main.pp
