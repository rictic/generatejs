Javascript Generator
====================

A program for generating widely varied, syntactically valid Javascript source code conforming to the ECMA-262 Standard.

Requires Haskell, easily installed at [The Haskell Platform](http://hackage.haskell.org/platform/)

Getting the Code
----------------

The latest version of the code can always be gotten from github, either by cloning it with git:

    git clone git://github.com/rictic/generatejs.git
    cd generatejs
    
Or by downloading a tarball:

    curl -L http://github.com/rictic/generatejs/tarball/master > generatejs.tar.gz
    tar -xzf generatejs.tar.gz
    cd *generatejs*/
    
Installation
------------

Installation should be as simple as

    cabal install

Running
-------

    generatejs [--stdout] <number-of-programs-to-create>

If you include `--stdout`, the programs are written to standard out, one to a line.

Otherwise it will create a directory `gen/` in your current directory.  This directory will in turn be filled with numbered subdirectories, each of which holds up to 20000 numbered js programs.  This is done to keep the filesystem from freaking out from too many files in any one directory.


Hacking
-------

The command line runner is in generateJS.hs, the actual generation code is in jsgenerator.js.

The code is written in a declarative style, with a pretty direct mapping from the productions in the standard to Symbols in the code.

i.e. the production

    SignedInteger ::: 
        DecimalDigits
        + DecimalDigits
        - DecimalDigits

becomes

    signedInteger = Nonterminal [
        [decimalDigits],
        [Terminal "+", decimalDigits],
        [Terminal "-", decimalDigits]]

I found it helpful to construct a few `Symbol`s with `Terminal` and `Nonterminal` and pass the result to `getAll` until I was sure I had it worked out.
