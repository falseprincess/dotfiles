# Falseprincess's zshrc config

# Path to your oh-my-zsh installation.
export ZSH="/home/sudozelda/.oh-my-zsh"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# Launch Custom Oh-my-zsh Shell Prompt
fpath+=$HOME/.zsh/typewritten
autoload -U promptinit; promptinit
prompt typewritten

# Aliases
alias ls="lsd -aF"
alias editherbs="micro ~/.config/herbstluftwm/autostart"
alias update="sudo pacman -Syu"
alias p="sudo pacman"
alias emacs="emacs emacs -q -l ~/.emacs.d/init.el"

# Launch pfetch at startup
pfetch
