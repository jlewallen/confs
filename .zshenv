# Setup go environment.
export GOPATH=~/go
export GIT_EDITOR=vim

export PATH=~/tools/bin:~/tools/node/bin:~/tools/cmake/bin:$PATH

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

export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools

export PATH=$PATH:~/tools/protobuf-installed/bin/
export PATH=$PATH:~/tools/nanopb/generator
export PATH=$PATH:~/tools/bin
export PATH=$PATH:~/go/bin
export PATH=$PATH:node_modules/.bin

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
    alias ec="$EMACSCLIENT -c -n"
else
fi

alias fix-home-workspaces="i3-msg 'workspace 1, move workspace to output HDMI-0'; i3-msg 'workspace 2, move workspace to output DVI-D-0'"
alias fix-lab-workspaces="i3-msg 'workspace 1, move workspace to output HDMI-0'; i3-msg 'workspace 2, move workspace to output DVI-D-0'"

alias cmb="(mkdir -p build && cd build && cmake ..)"
alias cmm="(mkdir -p build && cd build && cmake .. && make)"
alias cmt="(mkdir -p build && cd build && cmake .. && make all test)"
