HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory INC_APPEND_HISTORY autocd beep extendedglob nomatch notify SHARE_HISTORY
bindkey -e
bindkey '^R' history-incremental-pattern-search-backward

# Initialize the autocompletion
autoload -Uz compinit && compinit -i

autoload -U colors && colors
PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "

alias grep='grep --color=auto'
alias diff='colordiff'
alias ll='ls -lah'
alias less='less -R'
alias emacs="emacsclient -nw -a \"\" -c"
alias xclip='xclip -selection c'
alias recent='ls -t | head'

autoload -U select-word-style
select-word-style bash

export EDITOR="emacsclient -nw -a \"\" -c"
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:/usr/local/go/bin

if [ -e "$HOME/.workrc" ]; then
  source "$HOME/.workrc"
fi

# What OS are we running?
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    source "$HOME/.linux.zsh"
elif [[ `uname` == "Darwin" ]]; then
    source $HOME/.mac.zsh
else
    echo 'Unknown OS!'
fi
