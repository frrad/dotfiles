alias ls='ls -G'

# Docker CLI completions (Docker Desktop)
fpath=($HOME/.docker/completions $fpath)
autoload -Uz compinit
(( ${+_comps[docker]} )) || compinit
