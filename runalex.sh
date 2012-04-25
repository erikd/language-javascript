#!/bin/sh

# This contortion exists so that Lexer.hs can be included in the
# package, which can then be built for installation without requiring
# to have alex > 3.0 installed

alex -g -o src/Language/JavaScript/Parser/Lexer.hs src-dev/Language/JavaScript/Parser/Lexer.x
# To debug the lexer
#alex -d -g -o src/Language/JavaScript/Parser/Lexer.hs src-dev/Language/JavaScript/Parser/Lexer.x


