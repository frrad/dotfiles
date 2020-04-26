#!/bin/bash

set -x

install_package () {
  # What OS are we running?
  if [[ "$OSTYPE" == "linux-gnu" ]]; then
	apt-get update
	apt-get install -y $1
  elif [[ `uname` == "Darwin" ]]; then
    sudo -u $SUDO_USER brew install $1
  else
	echo 'Unknown OS!'
	exit 1
  fi
}

install_if_not_exist () {
  if ! [ -x "$(command -v $1)" ]; then
    install_package $1
  fi
}

target=`su -c 'echo $HOME/dotfiles' $SUDO_USER`

install_if_not_exist git
install_if_not_exist puppet
install_if_not_exist r10k

if [ ! -d "$target" ]; then
  su -c "git clone https://github.com/frrad/dotfiles.git $target" $SUDO_USER
fi

cd ${target}/puppet
r10k puppetfile install
export FACTER_sudo_user=$SUDO_USER
puppet apply --test --verbose $target/puppet/main.pp --modulepath=$target/puppet/modules
