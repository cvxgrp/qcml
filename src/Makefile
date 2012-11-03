SRC = $(wildcard Expression/*.hs) $(wildcard Parser/*.hs) $(wildcard Atoms/*.hs) 
PROBSRC = EFE.hs $(SRC) $(wildcard CodeGenerator/*.hs)

.PHONY: all
all: efe

efe: $(PROBSRC)
	ghc --make $<

clean:
	rm -rf *.hi *.o Expression/*.hi Expression/*o Parser/*.hi Parser/*.o TestCases/*.o TestCases/*.hi efe
