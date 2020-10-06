# Setup go environment.
export GOPATH=~/go
export GIT_EDITOR=vim

if [ -d ~/tools/go ]; then
    export GOROOT=~/tools/go
    export PATH=$GOROOT/bin:$PATH
fi

if [ "$(uname 2> /dev/null)" = "Darwin" ]; then
    export JAVA_HOME=`/usr/libexec/java_home`
    export ANDROID_HOME=$HOME/Library/Android/sdk
else
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
    export ANDROID_HOME=$HOME/Android/Sdk
fi

export PATH=$HOME/.cargo/bin:$PATH

export EDITOR=vim

export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH

export PATH=$HOME/tools/bin:$PATH
export PATH=$HOME/tools/syncthing:$PATH
export PATH=$HOME/tools/nanopb/generator:$PATH
export PATH=$HOME/tools/cmake/bin:$PATH
export PATH=$HOME/tools/node/bin:$PATH
export PATH=$HOME/tools/gitkraken:$PATH
export PATH=$HOME/tools/node_modules/.bin:$PATH
export PATH=$HOME/tools/wtf:$PATH
export PATH=$HOME/tools/p4merge/bin:$PATH
export PATH=$HOME/tools/jlink:$PATH
export PATH=$HOME/tools/protoc/bin:$PATH
export PATH=$HOME/conservify/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/tools/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/bin:$PATH
export PATH=node_modules/.bin:$PATH

export MAKEFLAGS=--no-print-directory

export CMAKE_MODULE_PATH=$HOME/conservify/cmake

export PATH=$HOME/fieldkit/fkc/build:$PATH

export EMACSCLIENT=emacsclient
if [ -f /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ]; then
	export EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
fi
if [ -f ~/tools/emacs/installed/bin/emacsclient ]; then
	export EMACSCLIENT=~/tools/emacs/installed/bin/emacsclient
fi
alias ecc="$EMACSCLIENT -n -c"

alias ec="i3-msg workspace number 1 ; $EMACSCLIENT -n"

alias fix-home-workspaces="~/tools/bin/i3-setup-workspaces.sh --home"
alias fix-work-workspaces="~/tools/bin/i3-setup-workspaces.sh --work"

if [ "$(hostname 2> /dev/null)" = "JACOB-LAPTOP" ]; then
    alias spotify="/usr/bin/spotify --force-device-scale-factor=1.5"
fi

alias tx="tmuxinator"
alias cmm="cmake -H. -Bbuild"
alias cmb="cmake --build build --"

if [ -f ~/.zshenv.private.sh ]; then
    source ~/.zshenv.private.sh
fi

if [ -x "$(command -v bat)" ]; then
    alias cat="bat"
fi

if [ -x "$(command -v batcat)" ]; then
    alias cat="batcat"
fi

alias warn="notify-send -u critical"
alias flag="notify-send -u critical DONE"
alias sdone="spd-say done"
alias rword="shuf -n1 /usr/share/dict/american-english"
alias sword='spd-say `shuf -n1 /usr/share/dict/american-english`'
alias cgrep="grep --color=always"
