Verifiable Retirements
----------------------

This is an experiment to use rollups and verifiable computation to enable a privacy-preserving,
auditable carbon retirement system.

## Install

Note, if you are on Apple M1 you will likely need to override `clang` with the homebrew one.

```
brew install llvm
```

Then add to your `.zshrc` file (or similar).

```
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export CC=/opt/homebrew/opt/llvm/bin/clang
export AR=/opt/homebrew/opt/llvm/bin/llvm-ar
```

## Build

Hopefully you can simply run `make build` and everything will just work!

## Test

To test we will need `octez-smart-rollup-wasm-debugger`. Luckily we're probably all OCaml hackers here, so you can grab that using opam.

```
# Gitlab or someone broke some packages
opam pin tezt git+https://gitlab.com/nomadic-labs/tezt.git#3.0.0
opam pin aches git+https://gitlab.com/nomadic-labs/ringo.git#v1.0.0
# Big pinning, takes a good moment
opam pin -yn git+https://gitlab.com/tezos/tezos --with-version=dev
opam instal -y octez-smart-rollup-wasm-debugger
```

This might install rust using brew which will be a pain, so either don't let opam do that or uninstall it afterwards.

## Overview

The basic idea is to combine [lurk][] with a Tezos [rollup][]. The rollup provides the scalable, tamper-proof
datastructure that enforces a consensus on our actions e.g. commiting to some private data. Whilst lurk gives
us verifiable computation (vericomp).



[lurk]: https://lurk-lang.org
[rollup]: https://tezos.gitlab.io/alpha/smart_rollups.html