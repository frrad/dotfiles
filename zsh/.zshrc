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
PROMPT="%F{196}%n%f@%F{${HOST_COLOR}}%m%f %F{yellow}%~%f %# "

alias grep='grep --color=auto'
alias ll='ls -lah'
alias less='less -R'
alias emacs="emacsclient -nw -a \"\" -c"
alias xclip='xclip -selection c'
alias recent='ls -t | head'
alias socks='autossh -v -D 8080 -C -N'

autoload -U select-word-style
select-word-style bash

export PERL_BADLANG=0
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

if command -v fzf &>/dev/null; then
  eval "$(fzf --zsh)"
fi

# What OS are we running?
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    source "$HOME/.linux.zsh"
elif [[ `uname` == "Darwin" ]]; then
    source $HOME/.mac.zsh
else
    echo 'Unknown OS!'
fi

cx() {
  local repo_root=$(git rev-parse --show-toplevel) || return 1
  local base=$(git symbolic-ref refs/remotes/origin/HEAD --short 2>/dev/null || echo "origin/main")
  local name="${*:-$(openssl rand -hex 4)}"
  local branch=$(echo "$name" | tr ' ' '-')
  local worktree_path="$repo_root/.claude/worktrees/$branch"
  git worktree add "$worktree_path" -b "$branch" "$base" && tmux new-session -s "$name" -c "$worktree_path" "codex --yolo"
}
