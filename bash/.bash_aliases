alias ll="ls -la"
alias emacs="emacsclient -nw -a \"\" -c"
alias djvu2pdf="ddjvu -format=pdf -quality=85 -verbose"
alias lll="ll|pager"
alias gitsub="git submodule foreach 'git fetch origin --tags; git checkout master; git pull' && git pull && git submodule update --init --recursive"
alias ssh-tunnel="autossh -N -D 8080"
alias watchtex="latexmk -f -pdf -pvc"

#Alias mosh to always use forwarded port
alias mosh="mosh -p 60002"