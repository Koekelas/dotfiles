[[ $- != *i* ]] && return

export HISTFILE="$XDG_DATA_HOME/bash/history"

alias ls='ls -lah --group-directories-first --color=auto'

PS1='[\u@\h \W]\$ '
