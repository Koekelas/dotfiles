[[ $- != *i* ]] && return

export HISTFILE="$XDG_DATA_HOME/bash/history"

alias ls='ls -lah --color=auto'

PS1='[\u@\h \W]\$ '
