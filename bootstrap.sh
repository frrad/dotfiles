#!/bin/bash
set -x

apt_install_if_not_exist () {
  if ! [ -x "$(command -v $1)" ]; then
	apt-get update
	apt-get install -y $1
  fi
}

gem_install_if_not_exist () {
  if ! [ -x "$(command -v $1)" ]; then
	gem install $1
  fi
}


user_homedir=$(eval echo ~$SUDO_USER)
target="$user_homedir/dotfiles"

# What OS are we running?
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  apt_install_if_not_exist git
  apt_install_if_not_exist puppet
  apt_install_if_not_exist r10k
elif [[ `uname` == "Darwin" ]]; then
  gem_install_if_not_exist puppet
  gem_install_if_not_exist r10k
else
  echo 'Unknown OS!'
  exit 1
fi


if [ ! -d "$target" ]; then
  su -c "git clone https://github.com/frrad/dotfiles.git $target" $SUDO_USER
fi

cd ${target}/puppet
r10k puppetfile install
export FACTER_sudo_user=$SUDO_USER
puppet apply --test --verbose $target/puppet/main.pp --modulepath=$target/puppet/modules
