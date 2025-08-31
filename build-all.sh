#!/usr/bin/env bash
set -xeo pipefail

# ugly hack for cross to work on nixos
if [ -e /nix/store ]; then
    export NIX_STORE=/nix/store
fi

target_dir=$(cargo metadata --format-version=1 | jq -r .target_directory)
root=$(cargo metadata --format-version=1 | jq -r .resolve.root)
project_name=$(cargo metadata --format-version=1 \
               | jq -r ".packages[] | select(.id==\"${root}\") | .name")

function build_linux() {
    CARGO_TARGET_DIR=./.cross-target/ cross build --target=x86_64-unknown-linux-gnu --release
    rm -rf dist_linux
    mkdir -p dist_linux
    cp "./.centos-target/x86_64-unknown-linux-gnu/release/${project_name}" dist_linux/
}
build_linux

function build_windows() {
    cross build --target=x86_64-pc-windows-gnu --release
    rm -rf dist_windows
    mkdir -p dist_windows
    cp "${target_dir}/x86_64-pc-windows-gnu/release/${project_name}.exe" dist_windows/
}
build_windows

nix develop --command ./build_wasm.sh
