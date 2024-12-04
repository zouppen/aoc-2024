# Advent of Code "framework" for Haskell

![Lambdapukki](lambdapukki.webp)

This is my "framework" for Advent of Code 2024. Written to make
parsing and output pretty-printing nice, also allowing easier
benchmarking among friends by making it easy to run old assingments
instead of just doing them ad hoc in REPL environment.

I'm pretty sure there isn't a single person who is interested about
using this, but in case you do, the license is GNU GPL 3.0 or (at your
option) later. You may (at your option) also send me a post card for
Christmas.

## Logbook

See my notes of daily tasks on the [logbook page](log.md)

## Installation

### Fedora

```sh
sudo dnf install ghc-aeson-devel ghc-attoparsec-devel ghc-cmdargs-devel
cabal install --overwrite-policy=always
```

### Debian and Ubuntu

```sh
sudo apt install libghc-aeson-dev libghc-attoparsec-dev libghc-cmdargs-dev
cabal install --overwrite-policy=always
```

## Usage

To run assignment for 2nd day, reading the input from default location, in this case `inputs/02`:

```sh
~/.cabal/bin/aoc-zouppen-2024 2
```

To run assignment for 1st day, reading the input from specified file:

```sh
~/.cabal/bin/aoc-zouppen-2024 -f input.txt 1
```

To make test run for parser for day 1, with JSON output:

```sh
~/.cabal/bin/aoc-zouppen-2024 -f input.txt -j -p input 1
```

For full info, run `~/.cabal/bin/aoc-zouppen-2024 --help`.

## Tips and tricks

During development, import module `Tonttu` which contains function
`parseBinFile` which can be used for running the parser in REPL
environment.

To add the assignment to the command-line utility, add the module to
files `aoc-zouppen.cabal` and `src/Tontut.hs`.
