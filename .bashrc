

# - - - EXPORTS.
# my prompt.
export export PS1='\e[0;32m\w\e[m $ '
# exporting some paths.
export PATH=$PATH:/usr/local/bin/
export PATH=$PATH:~/AppImages/
export PATH=$PATH:~/Scripts/
export PATH=$PATH:/bin/
# manually exporting my language. 
export LANG=en_US.UTF-8
export IGNOREEOF=100
# coloring my man pages.
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
# grep colors
export GREP_OPTIONS='--color=auto'

# - - - Misc settings.
shopt -s autocd
shopt -s histappend
shopt -s checkwinsize
shopt -s no_empty_cmd_completion
unset use_color sh 

# - - - ALIASES.
alias c="clear"
alias ps="ps auxf"
alias df="df -Tha --total"
alias psg="ps aux | grep -v grep | grep -i -e VSZ -e"
# easy way to find whats taking up space.
alias diskspace="du -S | sort -n -r |more"
# exa
alias ls="exa --long --header --git"
# no more cd.
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
# - - - Some useless unixporn program aliases.
alias tty-clock="tty-clock -c -C 4 -s -t"
alias pipes="pipes.sh -t 1 -f 75"

# - - - VARIABLES.
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
ENABLE_CORRECTION="true"

# Autostart X.
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

# Nice script to extract files.
extract () {
   if [ -f $1 ] ; then
       case $1 in
           *.tar.bz2)   tar xvjf $1    ;;
           *.tar.gz)    tar xvzf $1    ;;
           *.bz2)       bunzip2 $1     ;;
           *.rar)       unrar x $1       ;;
           *.gz)        gunzip $1      ;;
           *.tar)       tar xvf $1     ;;
           *.tbz2)      tar xvjf $1    ;;
           *.tgz)       tar xvzf $1    ;;
           *.zip)       unzip $1       ;;
           *.Z)         uncompress $1  ;;
           *.7z)        7z x $1        ;;
           *)           echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
 }

# - - - Some pretty, but useless commands I am launching. 
# neofetch
ufetch
