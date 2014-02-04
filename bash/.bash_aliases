alias ll="ls -la"
alias lll="ll|pager"

alias emacs="emacsclient -nw -a \"\" -c"

#ddjvu provided by djvulibre-bin on Debian
alias djvu2pdf="ddjvu -format=pdf -quality=85 -verbose"

alias gitsub="git submodule foreach 'git fetch origin --tags; git checkout master; git pull' && git pull && git submodule update --init --recursive"
alias gitsub-update="git submodule foreach git pull origin master"

alias watchtex="latexmk -f -pdf -pvc"

#Alias mosh to always use forwarded port
alias mosh="mosh -p 60002"

if ! which autossh &> /dev/null; then
   alias ssh-tunnel="ssh -N -D 8080"
else
   alias ssh-tunnel="autossh -N -D 8080"
fi

ssh-add -l &>/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'

function cutpdf() {
	gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=$2  -dLastPage=$3 -sOutputFile=$2to$3-$1 $1
}

#short for rsync resume... get it?
alias rsoom='rsync --partial --progress --rsh=ssh'
