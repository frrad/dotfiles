HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory INC_APPEND_HISTORY autocd beep extendedglob nomatch notify SHARE_HISTORY
bindkey -e


autoload -U colors && colors
PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "

alias grep='grep --color=auto'
alias diff='colordiff'
alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias less='less -R'
alias emacs="emacsclient -nw -a \"\" -c"
alias xclip='xclip -selection c'
alias recent='ls -t | head'
export EDITOR="emacsclient -nw -a \"\" -c"


autoload -U select-word-style
select-word-style bash

eval `keychain --eval --agents ssh --noask --quiet`


export GOPATH=$HOME/go
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/.local/bin
if [ -e "$HOME/.workrc" ]; then
  source "$HOME/.workrc"
fi

