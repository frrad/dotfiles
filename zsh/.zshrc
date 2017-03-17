HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory INC_APPEND_HISTORY autocd beep extendedglob nomatch notify SHARE_HISTORY
bindkey -e

if [ -e "$HOME/.workrc" ]; then
  source "$HOME/.workrc"
fi

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
export PATH=$PATH:$HOME/bin

autoload -U select-word-style
select-word-style bash

if [ -e "/usr/bin/zsh" ]; then
    eval `keychain --eval --agents ssh --noask --quiet`
fi
