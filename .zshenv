# Setup go environment.
export GOPATH=~/go
export GIT_EDITOR=vim

export PATH=~/tools/bin:~/tools/node/bin:~/tools/cmake/bin:$GOROOT/bin:$PATH

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
