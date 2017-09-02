if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi

# Setup go environment.
export GOROOT=~/tools/go1.8.3
export GOPATH=~/go
export GIT_EDITOR=vim

export PATH=~/tools/bin:~/tools/node/bin:$GOROOT/bin:~/tools/cmake-3.9.0-Linux-x86_64/bin:$PATH

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/

export PATH=$PATH:~/tools/protobuf-installed/bin/
export PATH=$PATH:~/tools/nanopb/generator
export PATH=$PATH:~/tools/bin
export PATH=$PATH:~/go/bin
export PATH=$PATH:node_modules/.bin

# EOF
