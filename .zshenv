# Path

export PATH=$PATH:$HOME/tools/nanopb/generator
export PATH=$PATH:$HOME/tools/cmake/bin
export PATH=$PATH:$HOME/tools/node/bin
export PATH=$PATH:$HOME/tools/gitkraken
export PATH=$PATH:$HOME/tools/flutter/bin
export PATH=$PATH:$HOME/tools/p4merge/bin
export PATH=$PATH:$HOME/tools/jlink
export PATH=$PATH:$HOME/tools/protoc/bin
export PATH=$PATH:$HOME/tools/vscode/bin
export PATH=$PATH:$HOME/tools/helix

export PATH=$PATH:$HOME/go/bin

export PATH=$PATH:$HOME/sync/bin
export PATH=$PATH:$HOME/tools/bin

export PATH=$PATH:$HOME/.pub-cache/bin

# WARNING DO NOT COPY Long been considering changing how I do this.
export PATH=$PATH:node_modules/.bin

# OSX Check 
if [ -d /Applications/CMake.app/Contents/bin ]; then
	export PATH=$PATH:/Applications/CMake.app/Contents/bin
fi

# This is me.
export EDITOR=vim
export GIT_EDITOR=vim

# Conditionally setup go environment.
if [ -d ~/tools/go ]; then
	export GOPATH=~/go
	export GOROOT=~/tools/go
	export PATH=$PATH:$GOROOT/bin
fi

# Android
if [ "$(uname 2> /dev/null)" = "Darwin" ]; then
	export ANDROID_HOME=$HOME/tools/android-sdk
else
	export ANDROID_HOME=$HOME/tools/android-sdk
fi

if [ -d ~/tools/android-studio/jre ]; then
	export JAVA_HOME=~/tools/android-studio/jre
else
	export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64/
fi

if [ -d $ANDROID_HOME ]; then
	export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin
	export PATH=$PATH:$ANDROID_HOME/tools
	export PATH=$PATH:$ANDROID_HOME/tools/bin
	export PATH=$PATH:$ANDROID_HOME/platform-tools

	if [ -d $ANDROID_HOME/ndk/25* ]; then
		export ANDROID_NDK_HOME=$(echo $ANDROID_HOME/ndk/25*)
	fi
fi

# Other miscellaneous things.
export MAKEFLAGS=--no-print-directory

# Helpers for simple todo notes.
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

alias wezterm="$HOME/tools/app-images/WezTerm*"
alias vtd="(cd $TODO && find . -not -path './.git*' -type f -printf '%T@ %p\n' | sort -n | cut -d' ' -f2-)"
alias reset='tput reset' 

# UNIX time helper.
function ux() {
	if [ -z "$1" ]; then
		TZ="UTC" date "+%s"
	else
		TZ="UTC" date -d @$1
	fi
}

# Private environment, not in git.
if [ -f ~/.zshenv.private.sh ]; then
    source ~/.zshenv.private.sh
fi

# Scache
export SCCACHE_CACHE_SIZE=10G
export SCCACHE_DIR=~/.sccache
if [ -f ~/.cargo/bin/sccache ]; then
    export RUSTC_WRAPPER=~/.cargo/bin/sccache
    export CMAKE_CXX_COMPILER_LAUNCHER=~/.cargo/bin/sccache
fi

# Rust
. "$HOME/.cargo/env"

# Spotify resolution.
if [ "$(hostname 2> /dev/null)" = "JACOB-LAPTOP" ]; then
    alias spotify="/usr/bin/spotify --force-device-scale-factor=1.5"
fi

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
