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

hash_hostname_to_color() {
    local hash=$(echo "%m" | cksum | awk '{print $1}')
    local blacklist=(188 189 190 191 192 193 194 195)
    local color_code=$((hash % 256 + 1))
    while [[ " ${blacklist[@]} " =~ " ${color_code} " ]]; do
        ((hash++))
        color_code=$((hash % 256 + 1))
    done
    echo $color_code
}

PS1="%F{red}%n%f@%F{$(hash_hostname_to_color)}%m%f %F{yellow}%~%f %% "

alias grep='grep --color=auto'
alias diff='colordiff'
alias ll='ls -lah'
alias less='less -R'
alias emacs="emacsclient -nw -a \"\" -c"
alias xclip='xclip -selection c'
alias recent='ls -t | head'
alias socks='ssh -v -D 8080 -C -N'

autoload -U select-word-style
select-word-style bash

export EDITOR="emacsclient -nw -a \"\" -c"
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/go/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/Library/Python/3.9/bin

sie () {
  if [ -e "$1" ]; then
	source "$1"
  fi
}

sie "$HOME/.workrc"

sie "/usr/share/doc/fzf/examples/key-bindings.zsh"
sie "/usr/share/zsh/vendor-completions/_fzf"

# mac
sie "/opt/homebrew/opt/fzf/shell/completion.zsh"
sie "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

# What OS are we running?
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    source "$HOME/.linux.zsh"
elif [[ `uname` == "Darwin" ]]; then
    source $HOME/.mac.zsh
else
    echo 'Unknown OS!'
fi
