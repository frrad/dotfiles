HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

setopt extended_history
setopt inc_append_history_time
setopt auto_cd
setopt beep
setopt extended_glob
setopt nomatch
setopt notify

bindkey -e
bindkey '^R' history-incremental-pattern-search-backward

autoload -U colors && colors

# A curated set of decent 256-color indexes (avoid super dark/light)
# (You can add/remove to taste)
typeset -a HOST_256_PALETTE=(33 36 37 38 41 42 44 45 70 71 74 75 106 110 140 141 174 175 176 177 178 179 180 181)

host_color_idx() {
  local h=${HOST:-${
    :-%m} }
  local -i sum=0 i code
  for (( i=1; i<=${#h}; i++ )); do
    printf -v code '%d' "'${h[i]}"
    (( sum += code ))
  done
  local -i idx=$(( (sum % ${#HOST_256_PALETTE[@]}) + 1 ))
  print -r -- ${HOST_256_PALETTE[idx]}
}

HOST_COLOR="$(host_color_idx)"
PROMPT="%F{red}%n%f@%F{${HOST_COLOR}}%m%f %F{yellow}%~%f %# "

alias grep='grep --color=auto'
alias diff='colordiff'
alias ll='ls -lah'
alias less='less -R'
alias emacs="emacsclient -nw -a \"\" -c"
alias xclip='xclip -selection c'
alias recent='ls -t | head'
alias socks='autossh -v -D 8080 -C -N'

autoload -U select-word-style
select-word-style bash

export EDITOR="emacsclient -nw -a \"\" -c"
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:/usr/local/go/bin

sie () {
  if [ -e "$1" ]; then
	source "$1"
  fi
}

sie "$HOME/.workrc"

sie "/usr/share/doc/fzf/examples/key-bindings.zsh"
sie "/usr/share/zsh/vendor-completions/_fzf"

# mac
sie "/usr/local/Cellar/fzf/0.22.0/shell/completion.zsh"
sie "/usr/local/Cellar/fzf/0.22.0/shell/key-bindings.zsh"

# What OS are we running?
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    source "$HOME/.linux.zsh"
elif [[ `uname` == "Darwin" ]]; then
    source $HOME/.mac.zsh
else
    echo 'Unknown OS!'
fi
