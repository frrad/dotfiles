# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi


if ! which python2 &> /dev/null; then
HOSTCOL=`python2.7 -c "
import commands, hashlib
colorTuples = zip( [0]*8 + [1]*8, range(30,39)*2 )
hostname = commands.getoutput( 'hostname' )
index = int(   hashlib.md5(hostname).hexdigest(), 16   ) % len(colorTuples)
hostColor = r'%d;%dm' % colorTuples[index]
print hostColor
"`
else
HOSTCOL=`python2 -c "
import commands, hashlib
colorTuples = zip( [0]*8 + [1]*8, range(30,39)*2 )
hostname = commands.getoutput( 'hostname' )
index = int(   hashlib.md5(hostname).hexdigest(), 16   ) % len(colorTuples)
hostColor = r'%d;%dm' % colorTuples[index]
print hostColor
"`
fi



HOSTCOL="\[\033[$HOSTCOL\]"
NOCOL='\[\033[00m\]'
GREEN='\[\033[01;32m\]'
BLUE='\[\033[01;34m\]'
RED='\[\033[01;31m\]'

PS1="${debian_chroot:+($debian_chroot)}$GREEN\u$NOCOL@$HOSTCOL\h$NOCOL:$BLUE\w"'`if [[ \$? != "0" ]]; then echo "'"$RED"'";  fi`'"\$$NOCOL "

unset HOSTCOL NOCOL GREEN BLUE RED



# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#Turn on Num Lock in TTYs
if [[ $( tty ) == /dev/tty? ]]; then
    setleds +num
fi


export GOPATH="/home/frederick/Projects/"
export PRINTER="HL2270DW"
export EDITOR="emacsclient -nw -a \"\" -c"
#No more .lesshst clutter
export LESSHISTFILE="-"

export PATH="$PATH:/home/frederick/Projects/bin"
export PATH="$PATH:/home/frederick/bin"
export PATH=$PATH:/home/frederick/.lib/pyroscope/bin/


#these mess with rtorrent
stty stop undef
stty start undef


if [ -e "$HOME/.workrc" ]; then
  source "$HOME/.workrc"
fi


if ! which autossh &> /dev/null; then
  eval `keychain --eval --agents ssh --noask --quiet`
fi
