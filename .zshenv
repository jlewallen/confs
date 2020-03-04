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

if [ -x "$(which rustc)" ]; then
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

export EDITOR=vim

export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH

export PATH=$HOME/tools/bin:$PATH
export PATH=$HOME/tools/nanopb/generator:$PATH
export PATH=$HOME/tools/cmake/bin:$PATH
export PATH=$HOME/tools/node/bin:$PATH
export PATH=$HOME/tools/node_modules/.bin:$PATH
export PATH=$HOME/tools/wtf:$PATH
export PATH=$HOME/tools/p4merge/bin:$PATH
export PATH=$HOME/tools/jlink:$PATH
export PATH=$HOME/tools/protoc/bin:$PATH
export PATH=$HOME/conservify/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=node_modules/.bin:$PATH

export MAKEFLAGS=--no-print-directory

export CMAKE_MODULE_PATH=$HOME/conservify/cmake

export PATH=$HOME/fieldkit/fkc/build:$PATH

case $OSTYPE in
    darwin*)
        export EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
        alias emacsclient=$EMACSCLIENT
        ;;
    *)
        export EMACSCLIENT=emacsclient
        ;;
esac

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

if [ -x $(which bat) ]; then
    alias cat="bat"
fi

alias warn="notify-send -u critical"
alias flag="notify-send -u critical DONE"
alias sdone="spd-say done"
alias rword="shuf -n1 /usr/share/dict/american-english"
alias sword='spd-say `shuf -n1 /usr/share/dict/american-english`'
alias cgrep="grep --color=always"
alias tml1="tmux select-layout 'decc,477x97,0,0{80x97,0,0[80x48,0,0,5,80x48,0,49,78],396x97,81,0[396x48,81,0,3,396x48,81,49{198x48,81,49,57,197x48,280,49,80}]}'"
# 0: cloud- (6 panes) [477x97] [layout 6654,477x97,0,0{102x97,0,0[102x24,0,0,5,102x23,0,25,114,102x48,0,49,78],374x97,103,0[374x48,103,0,3,374x48,103,49{176x48,103,49,57,197x48,280,49,80}]}] @0
# 1: fknapp* (5 panes) [477x97] [layout 04d3,477x97,0,0{102x97,0,0[102x48,0,0,107,102x48,0,49,108],374x97,103,0[374x48,103,0,109,374x48,103,49{176x48,103,49,110,197x48,280,49,111}]}] @59 (active)
