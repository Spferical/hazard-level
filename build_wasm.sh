#!/usr/bin/env bash
set -eo pipefail
target_dir=$(cargo metadata --format-version=1 | jq -r .target_directory)
cargo build --release --target wasm32-unknown-unknown && \
    wasm-bindgen "${target_dir}"/wasm32-unknown-unknown/release/run.wasm \
    --out-dir wasm --no-modules --no-typescript
wasm-opt -Os wasm/run_bg.wasm -o wasm/run_bg.wasm
