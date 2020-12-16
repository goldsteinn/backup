# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


export PATH="/home/noah/scripts:$PATH"
export PATH="/home/noah/.local/bin:$PATH"


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
	;;
    *)
	;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'



#configure environment variables
TFHE_PREFIX=/usr/local #the prefix where you installed tfhe
export C_INCLUDE_PATH=$C_INCLUDE_PATH:$TFHE_PREFIX/include
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$TFHE_PREFIX/include
export LIBRARY_PATH=$LIBRARY_PATH:$TFHE_PREFIX/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$TFHE_PREFIX/lib
#export GOROOT="/usr/lib/go-1.11"

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi

export PS1="\[\e[92m\]\u\[\e[92m\]\[\e[92m\][\[\e[92m\]\[\e[92m\]\T\[\e[92m\]\[\e[92m\]]\[\e[92m\]:\[\e[34m\]\w\[\e[m\]\[\e[m\]\$ "
EC2_IP=3.23.37.168
EC2_USER=ubuntu
KVP_PATH=/home/noah/Downloads/361_aws_kp.pem
#EC2_IP=3.133.88.1
#EC2_USER=ec2-user
gc () { cd /home/noah/programs/classes/active/$*; }
mvlc () { cd /home/noah/documents/MSvlc; }
ssh330 () { ssh noah@ec2-13-59-142-83.us-east-2.compute.amazonaws.com; }
ssh422 () { ssh goldstein.n@shell.cec.wustl.edu; }
sshadm () { ssh goldstein.n@adams.cse.wustl.edu; }
cse213 () { cd /home/noah/prg_arch/programmingstuff/old_programming_stuff/213cmu; }
ssh361 () { ssh -i $KVP_PATH $EC2_USER@$EC2_IP; }
to361 () { scp -r -i $KVP_PATH  $1 $EC2_USER@$EC2_IP:$2; }
from361 () { scp -r -i $KVP_PATH $EC2_USER@$EC2_IP:/home/$EC2_USER/$1 $2; }
sshq () { ssh ngoldstein@10.10.2.22; }
lsa () { ls -a; }
lsf () { ls -ltra; }
lsd () { ls -lt; }
lss () { ls -lS; }
leavepriv () { fusermount -u ~/private; }
gopriv () { encfs ~/.private ~/private; cd ~/private; }
sshstp () { ssh goldsteinn@stoppard.seas.wustl.edu; }
slk () { screen -d -m slack; }
kslk () { foo=$(pidof slack); for w in $foo; do echo $w; kill -9 $w; done; }
zm () { screen -d -m zoom; }
kzm () { foo=$(pidof zoom); for w in $foo; do echo $w; kill -9 $w; done; }
krtag () { foo=$(pidof rdm); for w in $foo; do echo $w; kill -9 $w; done; }
rtag () { rdm & }
sshsth () { ssh seth@aware.aladdin.cs.cmu.edu; }
chrome () { screen -d -m google-chrome $*; }
x() {
    screen -d -m xdg-open "$1"
}
sw () { screen -wipe; }
sr () { screen -R $1; }
sls () { screen -ls; }
scr () { screen -d -m $*; }
mykill () { foo=$(pidof $1); for w in $foo; do echo $w; kill -9 $w; done; }
gb () { go build $1; }
hgrep ()
{
    history | grep "$*"
}
search()
{
    local s="$_"
    local query=

    case "$1" in
        '')   ;;
        that) query="search?q=${s//[[:space:]]/+}" ;;
        *)    s="$*"; query="search?q=${s//[[:space:]]/+}" ;;
    esac

    screen -d -m google-chrome "http://www.google.com/${query}"
}
sb()
{
    local new_bash="bashrc_$1"
    local old_bash="bashrc"
    echo $new_bash
    cp ~/.$new_bash ~/.$old_bash
    source ~/.bashrc
}

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if [ 1 == 0 ]; then
   __conda_setup="$('/home/noah/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"

   if [ $? -eq 0 ]; then
       eval "$__conda_setup"
   else
       if [ -f "/home/noah/anaconda3/etc/profile.d/conda.sh" ]; then
           . "/home/noah/anaconda3/etc/profile.d/conda.sh"
       else
           export PATH="/home/noah/anaconda3/bin:$PATH"
       fi
   fi
fi
   unset __conda_setup
   # <<< conda initialize <<<



SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add ~/.ssh/id_rsa;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

export WORKON_HOME=/home/noah/local_environments/py_envs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
