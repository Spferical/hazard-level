#!/usr/bin/env bash
cargo build --release --target wasm32-unknown-unknown && \
    wasm-bindgen target/wasm32-unknown-unknown/release/run.wasm \
    --out-dir wasm --no-modules --no-typescript
