
LIBSRC = $(shell find src/Language -name \*.hs) $(LEXER) $(GRAMMAR)

LEXER = dist/build/Language/JavaScript/Parser/Lexer.hs
GRAMMAR = dist/build/Language/JavaScript/Parser/Grammar5.hs

GHC = cabal exec -- ghc
GHCFLAGS = -Wall -fwarn-tabs -rtsopts -prof -auto-all -caf-all


check : runtests.exe
	./runtests.exe


clean :
	find dist/build/ src/ -name \*.{o -o -name \*.hi | xargs rm -f
	rm -f $(LEXER) $(GRAMMAR) $(TARGETS) *.exe

%.exe : %.hs $(LIBSRC)
	$(GHC) $(GHCFLAGS) -O2 -i:src -i:dist/build --make $< -o $@


$(GRAMMAR) : src/Language/JavaScript/Parser/Grammar5.y
	happy $+ -o $@

$(LEXER) : src/Language/JavaScript/Parser/Lexer.x
	alex $+ -o $@
