#!/bin/zsh


#rvm
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
rvm 1.9.2

# load profile
source ~/.profile
# python virtualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ -f /etc/bash_completion.d/virtualenvwrapper ]; then
		source /etc/bash_completion.d/virtualenvwrapper
fi
## Python pip -> virtualenv only
export PIP_REQUIRE_VIRTUALENV=true
export PIP_RESPECT_VIRTUALENV=true


#paths
export QTDIR=/usr/lib/qt4
export SAKURA=182.48.52.238
export PATH=$PATH:~/bin
# alias
[ -f ~/.zshrc_aliases ] && source ~/.zshrc_aliases

# pythonbrew
source $HOME/.pythonbrew/etc/bashrc


export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
source ~/.zsh.d/zshenv
