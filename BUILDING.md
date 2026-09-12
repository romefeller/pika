# Building Pika

The Cabal package provides `lib:pika` (`Picalc` and `Syntax`) and `exe:pika`
(the original REPL). The library sources are `src/Picalc.hs` and `src/Syntax.hs`;
the executable entry point is `app/Main.hs`. The source layout preserves the
original interpreter behavior. In `Syntax`, `reserved` names the reserved-word
list, and `reservedToken` names the token parser.

## Reproducible development environment

With Nix and the `nix-command` and `flakes` experimental features enabled:

```sh
nix develop path:.
ghc --version
cabal --version
z3 --version
cabal build all --offline
cabal run exe:pika --offline
```

`path:.` also works before the new configuration files have been added to Git.
The first Nix invocation downloads the locked toolchain. The shell includes
the existing Haskell dependencies and Z3; the current interpreter does not use
Z3. Subsequent Cabal builds can use the packages supplied by that shell without
a Hackage download. No global compiler selection is changed.

`flake.lock` pins Nixpkgs, including compiler, library, Cabal, and solver
versions. The validated Linux shell supplies GHC 9.10.3, Cabal 3.16.1.0,
and Z3 4.16.0. `cabal.project` fixes the Hackage index date for ordinary Cabal
dependency resolution. These are separate controls: a Hackage index date alone
does not pin a compiler or every installed package. When intentionally changing
dependencies, update and validate the lock/index settings together. A future
`cabal.project.freeze` must be generated for the chosen compiler; do not share
boot-package constraints between different GHC releases.

## Without Nix

Install GHC and cabal-install through [GHCup](https://www.haskell.org/ghcup/).
The compatibility build uses GHC 9.6.7 and cabal-install 3.10.3.0.
If installed tools are absent from your PATH, load GHCup's environment first:

```sh
. "$HOME/.ghcup/env"
ghc --version
cabal --version
cabal update
cabal build all
cabal run exe:pika
```

At the `Pika>` prompt, enter `0`, then `:q` to exit. Use `:q` for the existing
REPL; EOF handling is an interpreter exercise, not part of the packaging change.
Use `cabal repl lib:pika` to inspect the library. Type annotations are needed
when choosing the polymorphic payload type. The unchanged REPL selects
`Pi String`; building the package does not add integer-language support.

## Package validation

```sh
cabal build all
cabal haddock lib:pika
cabal sdist
cabal check
```

Build warnings from the original sources are visible through `-Wall` and are
not promoted to errors. `-threaded` and `-rtsopts` enable the executable's
threaded runtime and optional RTS arguments, without forcing every CPU into use.
For example, `cabal run exe:pika -- +RTS -N2 -RTS` selects two capabilities.

The executable depends on the package library, so `Picalc` and `Syntax` are
compiled in the library and reused by the executable. Put future library modules
under `src/` and register them in the library stanza. Keep entry-point code under
`app/` and future test runners in their own test directories.

The GitHub Actions build uses the compatibility toolchain, builds documentation,
smoke-checks `0` followed by `:q`, and rebuilds an unpacked source archive. Action
revisions and tool versions are pinned. The workflow is supplied here; its hosted
execution happens after the files are pushed to GitHub.

No test suites are declared until their implementations exist. A successful
build or source archive does not establish interpreter correctness.

## Distribution metadata

The repository contains no license grant. `license: NONE` records that fact
without choosing a license for the author. `cabal check` exits nonzero and reports
that Hackage would reject the package for this reason. Before publishing a release, the owner must select a
license, add its text, update the Cabal metadata, and rerun `cabal check`.
The maintainer field points to the existing project's issue tracker rather
than inventing an email address.

This setup prepares the build and development workflow. Runtime reliability,
semantic fixes, integer servers, and proof obligations remain the exercises in
`tmp/pika-tutotral.md`. That local tutorial and other `tmp/` contents are ignored
by Git and excluded from the Cabal source distribution.
