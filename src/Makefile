SRC = $(wildcard Expression/*.hs) $(wildcard Parser/*.hs) $(wildcard Rewriter/*.hs) 
MAINSRC = Main.hs $(SRC)
TESTSRC = Test.hs $(SRC) $(wildcard TestCases/*.hs) $(wildcard OldParser/*.hs)
PROBSRC = ProbToCVX.hs $(SRC) $(wildcard CodeGenerator/*.hs)

.PHONY: all
all: main test probtocvx

main: $(MAINSRC)
	ghc --make $<

test: $(TESTSRC)
	ghc --make $<

probtocvx: $(PROBSRC)
	ghc --make $<

clean:
	rm -rf *.hi *.o Expression/*.hi Expression/*o Parser/*.hi Parser/*.o TestCases/*.o TestCases/*.hi main test probtocvx