# TsouTAPL

Implementations of various calculi of Pierce's book
"Types and Programming Languages".


# Description

Implementations in Haskell of various lambda-calculus based systems
close to those studied in Benjamin Pierce's "Types and Programming
Languages" book.  Made primarily for fun and secondarily as a project
for a graduate course given by Nikolaos Papaspyrou,
at the [MPLA postgraduate programme][mpla], back in 2010.
The [project website][tsoutapl] is still online for further info.

# Installation

     make
     make test

## Requirements

A rather *old* version of GHC (6.6 to 6.12).
Eventually I will update this to work with current Haskell trends and GHC,
but it is currently broken.

# Usage

     tsoutapl [options]
     tsoutapl mode [options] FILE

## Options

Run `tsoutapl --help`

## File formats

The examples contain comments that explain the formats.

[tsoutapl]: http://www.tsouanas.org/tsoutapl/
[mpla]: http://mpla.math.uoa.gr/
