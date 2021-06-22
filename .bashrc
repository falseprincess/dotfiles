
## defining more paths ##


## coloring manpages for comfy viewing ##
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

## some defaults ##
alias TERM=st
alias EDITOR=emacs
alias BROWSER=firefox-bin
alias FILEMANAGEMENT='pcmanfm .'

## generic but useful aliases ##
alias y='yes'
alias x='exit'
alias c='clear'
alias bc='bc -l'
alias rm='rm -rv'
alias cp='cp -rv'
alias mv='mv -vu'
alias la='ls -lisA'
alias rms='shred -uz'
alias mkdir='mkdir -pv'
alias hs='history | grep -i'
alias mount='mount |column -t'
alias myip='curl http://ifconfig.me/ip'
alias lh='ls -lisAd .[^.]*'
alias make='make -j16'
alias wget='wget -c'
## a quick way to get out of current directory ##
alias ..='cd ..'
alias ...='cd ../../../'
alias ....='cd ../../../../'
alias .....='cd ../../../../'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'
## pass options to free ##
alias meminfo='free -m -l -t'
## Stop after sending count ECHO_REQUEST packets ##
alias ping='ping -c 5'
## Do not wait interval 1 second, go fast ##
alias fastping='ping -c 100 -s.2'
## Parenting changing perms on / ##
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'
## get top process eating memory ##
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
## get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'
## progress bars are guilty pleasures ##
alias cpv='rsync -ah --info=progress2'
## mount now does what it should! ##
alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort"
## gentoo aliases ##
alias list-explicits="cat /var/lib/portage/world"
alias list-packages="eix --color -c --world | less -R"
alias update="sudo emerge --sync; sudo emerge --ask --verbose --update --deep --changed-use @world"

## unixporn aliases, lol ##
alias unimatrix='unimatrix -n -s 96 -l o -c blue'

# if user is not root, pass all commands via sudo #
if [ $UID -ne 0 ]; then
    alias reboot='sudo /sbin/reboot'
    alias poweroff='sudo /sbin/poweroff'
    alias halt='sudo /sbin/halt'
    alias shutdown='sudo /sbin/shutdown'
    
fi

## cool little archive extract script ##
extract () {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xjf $1        ;;
             *.tar.gz)    tar xzf $1     ;;
             *.bz2)       bunzip2 $1       ;;
             *.rar)       rar x $1     ;;
             *.gz)        gunzip $1     ;;
             *.tar)       tar xf $1        ;;
             *.tbz2)      tar xjf $1      ;;
             *.tgz)       tar xzf $1       ;;
             *.zip)       unzip $1     ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x $1    ;;
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

neofetch
