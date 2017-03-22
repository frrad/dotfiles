#!/bin/bash

install_if_absent () {
	if [ $2="" ]; then
	    PACKAGE_NAME=$1
	else
		PACKAGE_NAME=$2
	fi

	if ! which $1 &> /dev/null; then
		echo "Couldn't find $1. Installing $PACKAGE_NAME."
		sudo apt-get install $PACKAGE_NAME
	fi
}

if ! which sudo &> /dev/null; then
	echo "Installing sudo. Please supply root password."
	su -c "apt-get update && apt-get install sudo && usermod -aG sudo $USER"
fi

sudo apt-get update && sudo apt-get dist-upgrade

if [ ! -e "$HOME/.ssh/id_ecdsa" ]; then
    echo "Generating an SSH key."
    ssh-keygen -t ecdsa -f $HOME/.ssh/id_ecdsa
fi

if [ ! -e "$HOME/dotfiles" ]; then
    echo "Installing dotfiles."
	install_if_absent git
	install_if_absent stow

    eval `ssh-agent -s`
    ssh-add
    git clone git@github.com:frrad/dotfiles.git
fi

if ! which emacs &> /dev/null; then
	echo "Can't find emacs. Installing."
	sudo apt-get install emacs-nox
	echo "Installing dotfiles."
	stow emacs --dir=$HOME/dotfiles --target=$HOME
fi

sudo apt-get install openssh-server tree apt-file
