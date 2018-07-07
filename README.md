# smuggler

[![Hackage](https://img.shields.io/hackage/v/smuggler.svg)](https://hackage.haskell.org/package/smuggler)
[![Build status](https://secure.travis-ci.org/kowainik/smuggler.svg)](https://travis-ci.org/kowainik/smuggler)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/kowainik/smuggler?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/smuggler)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/smuggler/blob/master/LICENSE)

Smuggling

## Build instructions

### First time

```shell
$ mkdir deps
$ cd deps
$ git clone git@github.com:alanz/ghc-exactprint.git
$ cd ghc-exactprint
$ git checkout ghc-8.6
$ # comment line with `hashable` package in `packages` field in `cabal.project` file
$ cd ../..
$ cabal new-build --allow-newer
```

### Second time

```shell
$ cabal new-build --allow-newer
```
