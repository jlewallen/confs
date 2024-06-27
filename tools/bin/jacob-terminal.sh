#!/bin/bash


if [ -f ~/tools/app-images/WezTerm-nightly-Ubuntu20.04.AppImage ]; then
	~/tools/app-images/WezTerm-nightly-Ubuntu20.04.AppImage &
else
	~/.cargo/bin/alacritty &
fi
