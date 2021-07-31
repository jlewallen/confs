# Setup go environment.
export GOPATH=~/go
export GIT_EDITOR=vim

if [ "$(uname 2> /dev/null)" = "Darwin" ]; then
    export ANDROID_HOME=$HOME/Library/Android/sdk
else
    export ANDROID_HOME=$HOME/android-sdk
fi

export EDITOR=vim

if [ -d ~/tools/go ]; then
    export GOROOT=~/tools/go
    export PATH=$GOROOT/bin:$PATH
fi

export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/tools/bin:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH

export PATH=$HOME/tools/apache-maven/bin:$PATH
export PATH=$HOME/tools/nanopb/generator:$PATH
export PATH=$HOME/tools/cmake/bin:$PATH
export PATH=$HOME/tools/node/bin:$PATH
export PATH=$HOME/tools/gitkraken:$PATH
export PATH=$HOME/tools/wtf:$PATH
export PATH=$HOME/tools/kitty/bin:$PATH
export PATH=$HOME/tools/p4merge/bin:$PATH
export PATH=$HOME/tools/jlink:$PATH
export PATH=$HOME/tools/protoc/bin:$PATH
export PATH=$HOME/tools/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/bin:$PATH
export PATH=$HOME/tools/node_modules/.bin:$PATH

export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/go/bin:$PATH

export PATH=$HOME/fieldkit/fkc/build:$PATH
export PATH=$HOME/sync/bin:$PATH
export PATH=$HOME/tools/bin:$PATH

export PATH=node_modules/.bin:$PATH

export MAKEFLAGS=--no-print-directory

export CMAKE_MODULE_PATH=$HOME/conservify/cmake

function ppgrep() { pgrep "$@" | xargs --no-run-if-empty ps fp; }

export EMACSCLIENT=emacsclient
if [ -f /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ]; then
	export EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
fi
if [ -f ~/tools/emacs/installed/bin/emacsclient ]; then
	export EMACSCLIENT=~/tools/emacs/installed/bin/emacsclient
fi
alias ecc="$EMACSCLIENT -n -c"
alias ec="i3-msg workspace number 1 ; $EMACSCLIENT -n"

if [ "$(hostname 2> /dev/null)" = "JACOB-LAPTOP" ]; then
    alias spotify="/usr/bin/spotify --force-device-scale-factor=1.5"
fi

if [ -x "$(command -v bat)" ]; then
    alias cat="bat"
fi

if [ -x "$(command -v batcat)" ]; then
    alias cat="batcat"
fi

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

function ux() {
	if [ -z "$1" ]; then
		TZ="UTC" date "+%s"
	else
		TZ="UTC" date -d @$1
	fi
}

function ffd() {
	find . -iname "*$1*" -type d
}

function fff() {
	find . -iname "*$1*" -type f
}

if [ -f ~/.zshenv.private.sh ]; then
    source ~/.zshenv.private.sh
fi
