# smuggler

![smuggler-logo](https://user-images.githubusercontent.com/4276606/45937457-c2715c00-bff2-11e8-9766-f91051d36ffe.png)
<!--
[![Hackage](https://img.shields.io/hackage/v/smuggler.svg?logo=haskell)](https://hackage.haskell.org/package/smuggler)
[![Build](https://img.shields.io/travis/kowainik/smuggler.svg?logo=travis)](http://travis-ci.org/kowainik/smuggler)
-->
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/smuggler/blob/master/LICENSE)
![Github CI](https://github.com/jrp2014/smuggler/workflows/Smuggler/badge.svg)


> “So many people consider their work a daily punishment. Whereas I love my work
> as a translator. Translation is a journey over a sea from one shore to the
> other. Sometimes I think of myself as a smuggler: I cross the frontier of
> language with my booty of words, ideas, images, and metaphors.”
>
> ― Amara Lakhous, Clash of Civilizations Over an Elevator in Piazza Vittorio

Haskell Source Plugin which removes unused imports and adds explicit exports automatically.

## How to use

Add `smuggler` to the dependencies of your project. Then add the following
compiler options:

```
-fplugin=Smuggler.Plugin
```

The Plugin has serveral (case-inseneitive) options:

* `NoImportProcessing` - do no import processing
* `PreserveInstanceImports` - remove unused imports, but preserve a library import stub.
such as  `import Mod ()`, to import only instances of typeclasses from it. (The default.)
* `MinimiseImports` - remove unused imports, including any that may be needed to
import typeclass instances.  This may, therefore, stop the module from compiling.

* `NoExportProcessing` - do no export processing
* `AddExplicitExports` - add an explicit list of all available exports (excluding
those that are imported) if there is no existing export list. (The default.)
You may want to edit it to keep specific values, types or classes local to the module.
At present, a typeclass and its class methods are exported individually.  You may want to
replace those exports with an abbreviation such as `C(..)`.
* `ReplaceExports` - replace any existing module export list with one containing all
available exports (which you can, of course, then prune to your requirements).

Any other option value is used to generate a source file with the option value used as
a new extension rather than replacing the original file. For example,
```
-fplugins-opt=Smuggler.Plugin:new
```
will create output files with a `.new` suffix rather the overwriting the originals.

A lovely addition to this package is that it automatically supports on-the-fly
feature if you use it with `ghcid`. Smuggler doesn't perform file changes when
there are no unused imports. So you can just run `ghcid` as usual:

```
ghcid --command='cabal repl'
```

## Caveats
`smuggler` does not remove imports completely because an import may be being
used to only import instances of typeclasses, So it will leave stubs like

```haskell
import Mod ()
```

that you may need to remove manually.

## For contributors

Requirements:

* `ghc-8.8.3`
* `cabal >= 3.0` or `stack >= 2.0`

### Cabal: How to build?

```shell
cabal update
cabal build
```

To build with debugging:

```shell
cabal bulid -fdebug
```

#### Stack: How to build?

```shell
stack build
```

### Run tests

```shell
cabal test --enable-tests
```
