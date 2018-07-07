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

export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH

export PATH=$HOME/tools/bin:$PATH
export PATH=$HOME/tools/protobuf-installed/bin/:$PATH
export PATH=$HOME/tools/nanopb/generator:$PATH
export PATH=$HOME/tools/cmake/bin:$PATH
export PATH=$HOME/tools/node/bin:$PATH
export PATH=$HOME/fieldkit/bin:$PATH
export PATH=$HOME/conservify/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=node_modules/.bin:$PATH

if [ -z "$SSH_CONNECTION" ]; then
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
    alias ec="$EMACSCLIENT -n"
else
fi

alias fix-home-workspaces="~/tools/bin/i3-setup-workspaces.sh --home"
alias fix-work-workspaces="~/tools/bin/i3-setup-workspaces.sh --work"

alias cmb="(mkdir -p build && cd build && cmake ..)"
alias cmm="(mkdir -p build && cd build && cmake .. && make)"
alias cmt="(mkdir -p build && cd build && cmake .. && make all test)"
alias mib="(cd build && make)"
alias tib="(cd build && make all test)"
