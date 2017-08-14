if [ -f $HOME/.bashrc ]; then
    source $HOME/.bashrc
fi

# Setup go environment.
export GOROOT=~/tools/go1.8.3
export GOPATH=~/go
export GIT_EDITOR=vim

export PATH=~/tools/bin:~/tools/node/bin:$GOROOT/bin:~/tools/cmake-3.9.0-Linux-x86_64/bin:$PATH

# EOF
