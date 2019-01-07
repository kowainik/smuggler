# Changelog

`smuggler` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## Unreleased

* [#7](https://github.com/kowainik/smuggler/issues/7):
  Remove trailing commas in imports.
* [#30](https://github.com/kowainik/smuggler/issues/30):
  Optimize whole import deletion.
* [#57](https://github.com/kowainik/smuggler/issues/57):
  Add tests for implicit imports.
* [#59](https://github.com/kowainik/smuggler/issues/59):
  Check if cache directory already exists before removing.
* [#52](https://github.com/kowainik/smuggler/issues/52):
  Use `ByteString` instead of `FilePath` in `Parser`.
* Upgrade to GHC-8.6.3
* Improve tests output.

## 0.1.0 — Sep 24, 2018

* [#29](https://github.com/kowainik/smuggler/issues/29):
  Improve documentation.
* [#43](https://github.com/kowainik/smuggler/issues/43):
  Remove `cabal.project` (there's no need in it anymore).

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/smuggler/releases
