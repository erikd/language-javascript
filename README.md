Parser for JavaScript
---------------------

[![Build Status](https://secure.travis-ci.org/erikd/language-javascript.png?branch=master)](http://travis-ci.org/erikd/language-javascript)

Based (loosely) on language-python

Two Versions
------------

There are currently two versions:

* 0.5 series : Is a continuation of the 0.5.X.Y series, from the [master]
(https://github.com/erikd/language-javascript/tree/master) branch of this
github repository.

* 0.6 series : This has a vastly different and improved AST which makes if far
more difficult to build an non-sensical Javascript AST. This code is in the
[new-ast](https://github.com/erikd/language-javascript/tree/new-ast) branch of
this github repository.


How to build
------------

Library:

    cabal clean && cabal configure && cabal build

Tests:

    cabal clean && cabal configure -fbuildtests && cabal build

Running the tests

    ./dist/build/runtests/runtests


To debug the grammar

    happy -iparse.txt -g -a  -d src/Language/JavaScript/Parser/Grammar5.y

This generates src/Language/JavaScript/Parser/Grammar5.hs, delete this
when done with the debug version


UTF8/Unicode version
--------------------

Alex 3.0 now supports unicode natively, and has been included as a
dependency in the cabal file.

Note: The generation of the lexical analyser has been separated out,
      to remove the install-time dependency on Alex. If any changes
      need to be made to the lexer, the Lexer.x source lies in
      src-dev, and the runalex.sh script will invoke Alex with the
      appropriate directories.
