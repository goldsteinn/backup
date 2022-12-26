# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# ~/.bashrc: executed by bash(1) for non-login shells.
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

export PATH="/home/noah/scripts:$PATH"
export PATH="/home/noah/programs/pyscripts:$PATH"
export PATH="/home/noah/.local/bin:$PATH"
export PATH="/home/noah/programs/libraries/:$PATH"
export PATH="/home/noah/programs/libraries/arcanist/bin/:$PATH"
export PATH="/home/noah/programs/opensource/bap-dev/src/bap/install/bin:$PATH"
export LD_LIBRARY_PATH="/home/noah/.local/lib/"

HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=20000000

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
    history | grep "$*" | tail -n 50
}
fhgrep ()
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

rwifi() { sudo service network-manager restart; }
gshow() { git diff-tree --no-commit-id --name-only -r $1; }

killx() { foo=$(pidof $1); for w in $foo; do echo $w; kill -9 $w; done; }
foox() { todo="${@:2}"; foo=$(pidof $1); for w in $foo; do echo $w; $todo $w; done; }

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
    /usr/bin/ssh-add ~/.ssh/id_ed25519;
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

fpatch() { git format-patch -v $1 HEAD~$2 "${@:3}"; }
fpatchl() { patches=$(fpatch $1 $2 "${@:3}"); echo "${patches}"; echo "---------------"; echo $(echo ${patches} | tr '\n' ' '); }

fpatch-range() { git format-patch -v $1 HEAD~$2..HEAD~$3 "${@:4}"; }
fpatchl-range() { patches=$(fpatch $1 $2 $3 "${@:4}"); echo "${patches}"; echo "---------------"; echo $(echo ${patches} | tr '\n' ' '); }

gemail() { git send-email --to libc-alpha@sourceware.org --cc goldstein.w.n@gmail.com --cc hjl.tools@gmail.com --cc carlos@systemhalted.org $@; }

svml-email() { git send-email --to libc-alpha@sourceware.org --cc goldstein.w.n@gmail.com --cc hjl.tools@gmail.com --cc andrey.kolesov@intel.com --cc carlos@systemhalted.org $@; }

git-email() { git send-email --cc goldstein.w.n@gmail.com --to $@; }

gpatch-email() { patches=$(fpatch $1 $2); echo "Sending Out:"; echo "${patches}"; patch_list=$(echo ${patches} | tr '\n' ' '); gemail $patch_list; }


greply() { lhs="<"; rhs=">"; msgid=$1; msgid_len=${#msgid}; patches=${@:2}; first=${msgid:0:1}; last=${msgid:$((msgid_len-1)):1}; if [[ "$lhs" == "$first" ]]; then lhs=""; fi; if [[ "$rhs" == "$last" ]]; then rhs=""; fi; msgid=${lhs}${msgid}${rhs}; git send-email --to libc-alpha@sourceware.org --cc goldstein.w.n@gmail.com --cc carlos@systemhalted.org --in-reply-to=$msgid $patches; }

greply-libc() { lhs="<"; rhs=">"; msgid=$1; msgid_len=${#msgid}; patches=${@:2}; first=${msgid:0:1}; last=${msgid:$((msgid_len-1)):1}; if [[ "$lhs" == "$first" ]]; then lhs=""; fi; if [[ "$rhs" == "$last" ]]; then rhs=""; fi; msgid=${lhs}${msgid}${rhs}; git send-email --to libc-alpha@sourceware.org --cc goldstein.w.n@gmail.com --cc hjl.tools@gmail.com --cc carlos@systemhalted.org --in-reply-to=$msgid $patches; }

greply-svml() { lhs="<"; rhs=">"; msgid=$1; msgid_len=${#msgid}; patches=${@:2}; first=${msgid:0:1}; last=${msgid:$((msgid_len-1)):1}; if [[ "$lhs" == "$first" ]]; then lhs=""; fi; if [[ "$rhs" == "$last" ]]; then rhs=""; fi; msgid=${lhs}${msgid}${rhs}; git send-email --to libc-alpha@sourceware.org --cc goldstein.w.n@gmail.com --cc hjl.tools@gmail.com --cc andrey.kolesov@intel.com --cc carlos@systemhalted.org --in-reply-to=$msgid $patches; }
# git push --dry-run ssh://nwg@sourceware.org/git/glibc.git master:refs/heads/master
# git push ssh://nwg@sourceware.org/git/glibc.git master:refs/heads/master
#export WORKON_HOME=/home/noah/local_environments/py_envs
#export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3

export PYTHONPATH=$PYTHONPATH:"/home/noah/programs/pylibs"

new-c-project() { git clone --recursive git@github.com:goldsteinn/c-starter.git $1 && (cd $1; git remote rm origin); }

new-git-project() { git init && git remote add origin git@github.com:goldsteinn/$1.git; }

set-git-fork() { base=$(basename `git rev-parse --show-toplevel`); git remote add upstream git@github.com:goldsteinn/${base}.git; }
grep-pp() { find . -name "$1" -exec grep -i "$2" {} +; }

git-commit-all() { for i in $(git diff --name-only); do git add $i; git commit -m "$(basename $i)"; done; }

task-grep() { ps ax  | grep $@ | grep -v grep; }
# expire 02/26/23
export GITHUB_TOKEN="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"

export BROWSE_PATHS="/media/noah/ext-4tb/"
export BROWSE_HIST="/home/noah/.tmp/.browse-hist"

export MY_PERF_EXEC_PATH="/home/noah/programs/libraries/perf/linux-hwe-5.13-5.13.0/debian/build/tools-perarch/tools/perf"

if [ ! -z "${BASHRC_TO_RUN}" ]
then
    echo "Running: ${BASHRC_TO_RUN}"
    $BASHRC_TO_RUN
    unset BASHRC_TO_RUN
    exit

fi


libc-xcheck-fresh() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr; make -j12 --silent; make -j12 xcheck --silent); }

libc-check-fresh() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr; make -j15 --silent; make -j15 check --silent); }

libc-build-fresh() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr; make -j12 --silent;); }

libc-build-fresh-nm() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr --disable-multi-arch; make -j12 --silent;); }



libc-build-fresh-nm-isa() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr --disable-multi-arch CC="gcc -march=$1"; CXX="g++ -march=$1"; make -j12 --silent;); }

libc-build-fresh-m32() { rm -rf build; mkdir -p build/glibc; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure CC="gcc -m32" CXX="g++ -m32" --prefix=/usr --build=i686-pc-linux-gnu --host=i686-pc-linux-gnu; make -j12 --silent;); }

libc-build-fresh-m32-nm() { rm -rf build; mkdir -p build/glibc; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure CC="gcc -m32" CXX="g++ -m32" --prefix=/usr --build=i686-pc-linux-gnu --host=i686-pc-linux-gnu --disable-multi-arch; make -j12 --silent;); }

libc-build-fresh-isa() { rm -rf /home/noah/programs/opensource/glibc-dev/build; mkdir -p /home/noah/programs/opensource/glibc-dev/build/glibc/; (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; /home/noah/programs/opensource/glibc-dev/src/glibc/configure --prefix=/usr CC="gcc -march=$1"; CXX="g++ -march=$1"; make -j12 --silent;); }


libc-xcheck() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH;  make -j12 --silent; make -j12 xcheck --silent); }

libc-check() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH;  make -j15 --silent; make -j15  check --silent); }

libc-build() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH;  make -j12  --silent;); }

libc-string-check() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; make -j15 --silent; make -r -C /home/noah/programs/opensource/glibc-dev/src/glibc/string/ objdir=`pwd` check;) }

libc-wcsmbs-check() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; make -j15 --silent; make -r -C /home/noah/programs/opensource/glibc-dev/src/glibc/wcsmbs/ objdir=`pwd` check;) }


libc-dir-check() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; make -j15 --silent; make -r -C /home/noah/programs/opensource/glibc-dev/src/glibc/$1/ objdir=`pwd` check;) }
#. "$HOME/.cargo/env"

grab-patch() { output=""; if [[ -z "$2" ]]; then output="out.patch"; else output="$2"; fi; wget $1; mv index.html $output; }

libc-one-test() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; make -j15 --silent; make test t=$1); }

libc-fast-one-test() { (cd /home/noah/programs/opensource/glibc-dev/build/glibc/; unset LD_LIBRARY_PATH; make test t=$1); }

# opam configuration
test -r /home/noah/.opam/opam-init/init.sh && . /home/noah/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

cur-branch() { git branch --show-current; }
