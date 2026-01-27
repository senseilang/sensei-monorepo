#!/bin/bash
cargo test --workspace
cd ./sir-solidity-diff-tests/ && forge test --ffi
cd ..
