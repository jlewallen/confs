#!/bin/bash

set -xe

cargo install sccache

export RUSTC_WRAPPER=~/.cargo/bin/sccache

for name in du-dust exa git-delta just trunk cargo-watch cargo-expand cargo-edit \
		ldproxy espup espflash \
		cargo-generate cargo-modules miniserve ; do
	cargo install $name
done

hash -r
