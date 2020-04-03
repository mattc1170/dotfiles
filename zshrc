# -*- mode: sh -*-
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	local THOST=`echo $HOST | awk -F . '{print $1}'`
	echo -n "\ek$THOST [$CMD]\e\\"
    fi
}

my_precmd () {
    print -Pn "\e]2;$USER@$HOST %~\a"
}

precmd_functions+=my_precmd

autoload -U compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
#setopt APPEND_HISTORY
## for sharing history between zsh processes
#setopt INC_APPEND_HISTORY
#setopt SHARE_HISTORY

## never ever beep ever
#setopt NO_BEEP

## automatically decide when to page a list of completions
#LISTMAX=0

## disable mail checking
#MAILCHECK=0

# autoload -U colors
#colors

export LS_OPTIONS="--color --group-directories-first"
export EDITOR="/usr/bin/emacsclient -t -a ''"
export VISUAL=$EDITOR

# Aliases
alias ls="/bin/ls $LS_OPTIONS"
alias ll="/bin/ls $LS_OPTIONS -l"
alias la="/bin/ls $LS_OPTIONS -la | less -r"
alias dv="dirs -v"
alias cld="dirs .; popd"
alias t="~/bin/todo.sh"

# Useful emacs aliases
alias e="emacsclient -c -n -a ''"
alias et="emacsclient -t -a ''"
alias ek="emacsclient -e '(shutdown-emacs-server)'"
alias ee="emacsclient -n"
alias ediff="~/bin/ediff.sh"
alias ssh-keygen="noglob ssh-keygen"

if [[ -f ~/.localshrc ]]; then
    source ~/.localshrc
fi

# Zsh options
setopt autopushd
setopt pushd_silent
setopt pushd_minus
setopt pushd_to_home
setopt auto_cd
setopt auto_menu

export DIRSTACKSIZE=5
export PATH=$PATH:~/.local/bin:~/bin
export HISTSIZE=1000

# Powerlevel10k prompt theme
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
