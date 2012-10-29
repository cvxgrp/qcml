SRC = $(wildcard Expression/*.hs) $(wildcard Parser/*.hs) $(wildcard Rewriter/*.hs) 
MAINSRC = Main.hs $(SRC)
TESTSRC = Test.hs $(SRC) $(wildcard TestCases/*.hs) $(wildcard OldParser/*.hs)
PROBSRC = EFE.hs $(SRC) $(wildcard CodeGenerator/*.hs)

.PHONY: all
all: main test efe

main: $(MAINSRC)
	ghc --make $<

test: $(TESTSRC)
	ghc --make $<

efe: $(PROBSRC)
	ghc --make $<

clean:
	rm -rf *.hi *.o Expression/*.hi Expression/*o Parser/*.hi Parser/*.o TestCases/*.o TestCases/*.hi main test probtocvx
