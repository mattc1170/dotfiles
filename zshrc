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

export LESS="-Xr"

# If we're running in emacs vterm, set simple emacsclient.
# Otherwise, call emacsclient text mode
if [[ ${INSIDE_EMACS:-no_emacs_here} != 'no_emacs_here' ]]; then
    export EDITOR=emacsclient
    export VISUAL=emacsclient
else
    export EDITOR="~/bin/et.sh"
    export VISUAL=$EDITOR
fi

# Aliases
alias ld="eza -lD"
alias ll="eza --group-directories-first -l"
alias lf="eza -lf --color=always | grep -v /"
alias la="eza -la --group-directories-first --color=always | less -r"
alias lh="eza -dl .* --group-directories-first"
alias lz="eza -alf --color=always --sort=size | grep -v /"
alias lt="eza -al --sort=modified"
alias ls="eza"

# Useful emacs aliases
alias e="emacsclient -c -n -a ''"
alias et="TERM=xterm-24bits emacsclient -t -a ''"
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
setopt interactive_comments

export DIRSTACKSIZE=5
export PATH=$PATH:~/.local/bin:~/bin:~/.cargo/bin
export HISTSIZE=1000

if [ $TERM != "linux" ] && [ $TERM != "dumb" ]; then
    # Powerlevel10k prompt theme
    source ~/powerlevel10k/powerlevel10k.zsh-theme

    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
fi

[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
