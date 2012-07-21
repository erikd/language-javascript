
TARGETS = runtests.exe singletest.exe

LIBSRC = $(shell find src/Language -name \*.hs) $(LEXER).hs $(GRAMMAR).hs

LEXER = src/Language/JavaScript/Parser/Lexer
GRAMMAR = src/Language/JavaScript/Parser/Grammar5

all: runtests.exe
	./runtests.exe

single : singletest.exe
	./singletest.exe

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -f $(GRAMMAR).hs $(TARGETS)

singletest.exe : singletest.hs $(LIBSRC)
	ghc -Wall --make -i:src $< -o $@

runtests.exe : runtests.hs $(LIBSRC)
	ghc -Wall --make -i:src $< -o $@

$(GRAMMAR).hs : $(GRAMMAR).y
	happy $+ -o $@

$(LEXER).hs : src-dev/Language/JavaScript/Parser/Lexer.x
	./runalex.sh
