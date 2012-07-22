
LIBSRC = $(shell find src/Language -name \*.hs) $(LEXER).hs $(GRAMMAR).hs

LEXER = src/Language/JavaScript/Parser/Lexer
GRAMMAR = src/Language/JavaScript/Parser/Grammar5

check : runtests.exe
	./runtests.exe

quickcheck : quickcheck.exe
	./quickcheck.exe

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -f $(GRAMMAR).hs $(TARGETS)

%.exe : %.hs $(LIBSRC)
	ghc -Wall --make -i:src $< -o $@


$(GRAMMAR).hs : $(GRAMMAR).y
	happy $+ -o $@

$(LEXER).hs : src-dev/Language/JavaScript/Parser/Lexer.x
	./runalex.sh
