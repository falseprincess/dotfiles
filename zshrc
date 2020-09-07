# Falseprincess's zshrc config

# Path to your oh-my-zsh installation.
export ZSH="/home/sudozelda/.oh-my-zsh"

ZSH_THEME="minimal"

plugins=(git)

source $ZSH/oh-my-zsh.sh

#### Aliases

# Aliases
alias ls="exa"

# Starting Xorg
alias x="startx"

## Colorize the grep command output for ease of use (good for log files)##
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

# Aliases for software managment
alias pacman="sudo pacman --color auto"
alias update="sudo pacman -Syyu"
alias prm="sudo pacman -Rs --color auto"
alias pins="sudo pacman -S --color auto"
alias psr="sudo pacman -Ss --color auto"
alias pdep="deps p"
alias pclean="sudo pacman -Rns \$(pacman -Qtdq)"

#get fastest mirrors in your neighborhood 
alias reflector-update-mirrors="sudo reflector --protocol https --latest 50 --number 20 --sort rate --save /etc/pacman.d/mirrorlist"

# yay as aur helper - updates everything
alias pksyua="yay -Syu --noconfirm"
alias yins="yay -S --color auto"
alias yrm="yay -R --color auto"
alias ysr="yay -Ss --color auto"
alias ydep="deps y"

#readable output
alias df='df -h'

# Launch pfetch at startup
# pfetch

# Starting x at login
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; fi

# Launch neofetch at startup
neofetch

