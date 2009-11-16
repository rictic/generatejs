Javascript Generator
====================

A program for generating widely varied, syntactically valid, Javascript source code conforming to the ECMA-262 Standard.

Requires Haskell, easily installed from here: http://hackage.haskell.org/platform/ 

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

    generatejs <number-of-programs-to-create

This will create a directory gen/ in your current directory.  This directory will in turn be filled with numbered subdirectories, each of which holds up to 20000 numbered js programs.  This is done to keep the filesystem from freaking out from too many files in any one directory.
