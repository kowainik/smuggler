# smuggler

[![Hackage](https://img.shields.io/hackage/v/smuggler.svg)](https://hackage.haskell.org/package/smuggler)
[![Build status](https://secure.travis-ci.org/kowainik/smuggler.svg)](https://travis-ci.org/kowainik/smuggler)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/smuggler/blob/master/LICENSE)

Haskell Compiler Plugin which removes unused imports automatically.

## Build instructions

Requirements:

* `ghc >= ghc-8.6.1-alpha2`

### Build project

```shell
$ cabal new-update
$ cabal new-build --allow-newer
```

### Run tests

```shell
$ cabal new-test --allow-newer
```
