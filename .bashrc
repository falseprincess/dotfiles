# Path to your oh-my-bash installation.
export OSH=/home/sudozelda/.oh-my-bash
# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_OSH_DAYS=13
# You may need to manually set your language environment
export LANG=en_US.UTF-8

alias ll="ls -lhA"
alias ps="ps auxf"
alias df="df -Tha --total"
alias psg="ps aux | grep -v grep | grep -i -e VSZ -e"
alias du="du -ach | sort -h"i

# Set name of the theme to load. Optionally, if you set this to "random"
OSH_THEME="mbriggs"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-bash/plugins/*)
plugins=(
  git
  bashmarks
)

source $OSH/oh-my-bash.sh

pfetch
