# Path
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/tools/bin:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH

export PATH=$HOME/tools/apache-maven/bin:$PATH
export PATH=$HOME/tools/nanopb/generator:$PATH
export PATH=$HOME/tools/age:$PATH
export PATH=$HOME/tools/cmake/bin:$PATH
export PATH=$HOME/tools/node/bin:$PATH
export PATH=$HOME/tools/gitkraken:$PATH
export PATH=$HOME/tools/wtf:$PATH
export PATH=$HOME/tools/kitty/bin:$PATH
export PATH=$HOME/tools/flutter/bin:$PATH
export PATH=$HOME/tools/p4merge/bin:$PATH
export PATH=$HOME/tools/jlink:$PATH
export PATH=$HOME/tools/protoc/bin:$PATH
export PATH=$HOME/tools/node_modules/.bin:$PATH
export PATH=$HOME/tools/vscode/bin:$PATH
export PATH=$HOME/tools/syncthing:$PATH

export PATH=$HOME/go/bin:$PATH

export PATH=$HOME/fieldkit/fkc/build:$PATH
export PATH=$HOME/sync/bin:$PATH
export PATH=$HOME/tools/bin:$PATH

export PATH="$PATH":"$HOME/.pub-cache/bin"

export PATH=node_modules/.bin:$PATH

if [ -d /Applications/CMake.app/Contents/bin ]; then
    export PATH=/Applications/CMake.app/Contents/bin:$PATH
fi

export EDITOR=vim

# Setup go environment.
export GOPATH=~/go
export GIT_EDITOR=vim

if [ -d ~/tools/go ]; then
    export GOROOT=~/tools/go
    export PATH=$GOROOT/bin:$PATH
fi

# Android
if [ "$(uname 2> /dev/null)" = "Darwin" ]; then
    export ANDROID_HOME=$HOME/tools/android-sdk
else
    export ANDROID_HOME=$HOME/tools/android-sdk
fi

# Other miscellaneous things.
export MAKEFLAGS=--no-print-directory

export CMAKE_MODULE_PATH=$HOME/conservify/cmake

function ppgrep() { pgrep "$@" | xargs --no-run-if-empty ps fp; }

# Emacs, back in the day.
#
export EMACSCLIENT=emacsclient
if [ -f /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ]; then
	export EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
fi
if [ -f ~/tools/emacs/installed/bin/emacsclient ]; then
	export EMACSCLIENT=~/tools/emacs/installed/bin/emacsclient
fi
alias ecc="$EMACSCLIENT -n -c"
alias ec="i3-msg workspace number 1 ; $EMACSCLIENT -n"

# Spotify resolution.

if [ "$(hostname 2> /dev/null)" = "JACOB-LAPTOP" ]; then
    alias spotify="/usr/bin/spotify --force-device-scale-factor=1.5"
fi

# Alias tools if we find them.

if [ -x "$(command -v bat)" ]; then
    alias cat="bat"
fi

if [ -x "$(command -v batcat)" ]; then
    alias cat="batcat"
fi

# Aliases

alias c="clear"
alias p="python3"
alias mv="mv -n"
alias tx="tmuxinator"
alias cmm="cmake -H. -Bbuild"
alias cmb="cmake --build build --"
alias warn="notify-send -u critical"
alias flag="notify-send -u critical DONE"
alias sdone="spd-say done"
alias rword="shuf -n1 /usr/share/dict/american-english"
alias sword='spd-say `shuf -n1 /usr/share/dict/american-english`'
alias cgrep="grep --color=always"
alias stripcolors="sed -r 's/\x1B\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g'"
alias fix-home-workspaces="~/tools/bin/i3-setup-workspaces.sh --home"
alias fix-work-workspaces="~/tools/bin/i3-setup-workspaces.sh --work"
alias icat="kitty +kitten icat --align=left"
alias theme-reset="kitty @ --to unix:/tmp/kitty set-colors --reset"
alias theme="cd ~/.config/kitty/themes && fzf --preview 'head -n 40 {} && kitty @ set-colors -a -c {}'; cd -"
alias gitd="git checkout develop"
alias gitrd="git rebase -i develop"
alias gitrm="git rebase -i main"
alias gitrod="git rebase -i origin/develop"
alias gitrom="git rebase -i origin/main"
alias gitm="git checkout main"
alias gitd="git checkout develop"
alias ips="ip --brief addr"

# TODO Helpers

TODO="$HOME/sync/notes/TODO"

function td() {
	maybe_where=$1
	where=$TODO
	if [ "$maybe_where" != "" ]; then
		where="$TODO/$1*"
	fi
	case $PWD/ in
	$TODO/*) popd      ;;
	*) pushd ${~where} ;;  # zsh specific wildcard expansion
	esac
}

alias vtd="(cd $TODO && find . -not -path './.git*' -type f -printf '%T@ %p\n' | sort -n | cut -d' ' -f2-)"

# UNIX time helper.
function ux() {
	if [ -z "$1" ]; then
		TZ="UTC" date "+%s"
	else
		TZ="UTC" date -d @$1
	fi
}

# Find helpers.
function ffd() {
	find . -iname "*$1*" -type d
}

function fff() {
	find . -iname "*$1*" -type f
}

# Private environment, not in git.
if [ -f ~/.zshenv.private.sh ]; then
    source ~/.zshenv.private.sh
fi

# Rust
. "$HOME/.cargo/env"

# Random bits.

# Change default terminal emulator.
# sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/tools/bin/alacritty 50

# For some reason these don't work very well when set inside here, so check the
# oh-my-zsh files.
#
# ~/.oh-my-zsh/lib/directories.zsh
# ~/.oh-my-zsh/lib/history.zsh
#
unsetopt auto_pushd
setopt no_share_history
unsetopt share_history

# eof
