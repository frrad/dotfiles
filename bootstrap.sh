#!/bin/bash

set -x

target=`su -c 'echo $HOME/dotfiles' $SUDO_USER`

if ! [ -x "$(command -v git)" ]; then
  apt-get update
  apt-get install -y git
fi

if ! [ -x "$(command -v puppet)" ]; then
  apt-get update
  apt-get install -y puppet r10k
fi

if [ ! -d "$target" ]; then
  su -c "git clone https://github.com/frrad/dotfiles.git $target" $SUDO_USER
fi

cd ${target}/puppet
r10k puppetfile install
export FACTER_sudo_user=$SUDO_USER
puppet apply --test --verbose $target/puppet/main.pp --modulepath=$target/puppet/modules
