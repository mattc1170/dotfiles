#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

### -- Prompt code copied from crazy zsh prompt web page --
function precmd {

    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))


    ###
    # Truncate the path if it's too long.
    
    PR_FILLBAR=""
    PR_PWDLEN=""
    
    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}
    
    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	    ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
	PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi

}


setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	local THOST=`echo $HOST | awk -F . '{print $1}'`
	echo -n "\ek$THOST [$CMD]\e\\"
    fi
}


setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst


    ###
    # See if we can use colors.

    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	(( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"


    ###
    # Extended characters using old charset.  Use with older versions
    # of zsh and set character encoding to ISO-8859-1.

    # typeset -A altchar
    # set -A altchar ${(s..)terminfo[acsc]}
    # PR_SET_CHARSET="%{$terminfo[enacs]%}"
    # PR_SHIFT_IN="%{$terminfo[smacs]%}"
    # PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    # PR_HBAR=${altchar[q]:--}
    # PR_ULCORNER=${altchar[l]:--}
    # PR_LLCORNER=${altchar[m]:--}
    # PR_LRCORNER=${altchar[j]:--}
    # PR_URCORNER=${altchar[k]:--}


    ###
    # Extended characters using UTF-8.  Use with more recent versions
    # of zsh.

    PR_SET_CHARSET=""
    PR_SHIFT_IN=""
    PR_SHIFT_OUT=""
    PR_HBAR="─"
    PR_ULCORNER="┌"
    PR_LLCORNER="└"
    PR_URCORNER="┐"
    PR_LRCORNER="┘"

    
    ###
    # Decide if we need to set titlebar text.
    
    case $TERM in
	xterm*)
	    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
	    ;;
	screen)
	    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
	*)
	    PR_TITLEBAR=''
	    ;;
    esac
    
    
    ###
    # Decide whether to set a screen title
    if [[ "$TERM" == "screen" ]]; then
	PR_STITLE=$'%{\ek%m\e\\%}'
    else
	PR_STITLE=''
    fi
    
    
    ###
    # APM detection
    
##    if which ibam > /dev/null; then
##	PR_APM='$PR_RED${${PR_APM_RESULT[(f)1]}[(w)-2]}%%(${${PR_APM_RESULT[(f)3]}[(w)-1]})$PR_LIGHT_BLUE:'
##    elif which apm > /dev/null; then
##	PR_APM='$PR_RED${PR_APM_RESULT[(w)5,(w)6]/\% /%%}$PR_LIGHT_BLUE:'
##   else
	PR_APM=''
##    fi
    
    
    ###
    # Finally, the prompt.


PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_CYAN$PR_SHIFT_IN$PR_ULCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m:%l\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_HBAR${(e)PR_FILLBAR}$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_MAGENTA%$PR_PWDLEN<...<%~%<<\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_URCORNER$PR_SHIFT_OUT\

$PR_CYAN$PR_SHIFT_IN$PR_LLCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?$PR_BLUE:)\
${(e)PR_APM}$PR_YELLOW%D{%H:%M}\
$PR_LIGHT_BLUE:%(!.$PR_RED.$PR_WHITE)%#$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_NO_COLOUR '

    RPROMPT=' $PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_HBAR$PR_SHIFT_OUT\
($PR_YELLOW%D{%a,%b%d}$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

    PS2='$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

echo "Terminal is $TERM"

if [[ $TERM = "dumb" || $TERM = "vt100" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1="$ "
else
    setprompt
fi

### -- End of prompt code for crazy zsh prompt

#if [[ $COLORTERM = "gnome-terminal" ]]; then
#    export TERM=xterm-256color
#fi


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
export VISUAL="/usr/bin/emacsclient -c -a ''"

# Aliases
alias ls="/bin/ls $LS_OPTIONS"
alias ll="/bin/ls $LS_OPTIONS -l"
alias la="/bin/ls $LS_OPTIONS -la | less -r"
alias dv="dirs -v"
alias cld="dirs .; popd"
alias t="~/bin/todo.sh"

# Moved these to .localshrc because of XLIB_SKIP_ARGB_VISUALS issues on Elemenary Freya
#alias e="/usr/bin/emacsclient -c -n -a ''"
#alias et="/usr/bin/emacsclient -t -a ''"
#alias ek=="/usr/bin/emacsclient -e '(shutdown-emacs-server)'"
#alias ee="/usr/bin/emacsclient -n"

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
# cdpath=( /home/matt )


