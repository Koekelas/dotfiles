[[ $- != *i* ]] && return

export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/bash/history"

alias ls='ls -lAh --group-directories-first --color=auto'

PS1='[\u@\h \W]\$ '
