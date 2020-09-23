[[ $- != *i* ]] && return

export HISTFILE="${XDG_DATA_HOME:-~/.local/share}/bash/history"

alias ls='ls -lah --group-directories-first --color=auto'

PS1='[\u@\h \W]\$ '
