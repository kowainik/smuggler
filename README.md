# smuggler

![smuggler-logo](https://user-images.githubusercontent.com/4276606/45937457-c2715c00-bff2-11e8-9766-f91051d36ffe.png)
[![Hackage](https://img.shields.io/hackage/v/smuggler.svg?logo=haskell)](https://hackage.haskell.org/package/smuggler)
[![Build](https://img.shields.io/travis/kowainik/smuggler.svg?logo=travis)](http://travis-ci.org/kowainik/smuggler)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/smuggler/blob/master/LICENSE)

> “So many people consider their work a daily punishment. Whereas I love my work
> as a translator. Translation is a journey over a sea from one shore to the
> other. Sometimes I think of myself as a smuggler: I cross the frontier of
> language with my booty of words, ideas, images, and metaphors.”
>
> ― Amara Lakhous, Clash of Civilizations Over an Elevator in Piazza Vittorio

Haskell Source Plugin which removes unused imports automatically.

## How to use

Add `smuggler` to the dependencies of your project. Then add the following
compiler options:

```
-fplugin=Smuggler.Plugin
```

You can also use `smuggler` with `ghcid` to enable on-the-fly features:

```
ghcid --command='cabal new-repl --ghc-options="-fplugin=Smuggler.Plugin"'
```

## For contributors

Requirements:

* `ghc-8.6.3`

### Cabal: How to build?

```shell
$ cabal new-update
$ cabal new-build
```

#### Stack: How to build?

```shell
$ stack build
```

### Run tests

```shell
$ cabal new-test --allow-newer
```
